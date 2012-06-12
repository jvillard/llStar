open Printf
open Llvm
open TypeKind
open ValueKind
open Cfg_core
open Psyntax
open Logic_spec

(** used to catch metadata information in values *)
exception MetaData of llvalue

(* a few maps and sets *)
module IdMap = Map.Make (struct type t = llvalue let compare = compare end)
module LlvalueSet = Set.Make (struct type t = llvalue let compare = compare end)
module LltypeSet = Set.Make (struct type t = lltype let compare = compare end)

(** verification environment *)
type verify_env = {
  mutable context: llcontext; (** llvm stuff *)
  mutable target: Llvm_target.TargetData.t; (** llvm stuff regarding the target *)
                                            (* we need it to compute type sizes,
					       alignment and padding *)
  mutable logic: Psyntax.logic; (** inference rules for logical entailments *)
  mutable abs_rules: Psyntax.logic; (** abstraction rules for formulas *)
  mutable specs: funspec list; (** the hoare triples we assume for declared
				   functions and strive to prove for defined ones  *)
  mutable gvars: (string * args * args) list; (** global variable: id * type * value *)
  mutable idMap: string IdMap.t; (** maps LLVM nameless values to identifiers *)
  mutable result: bool; (** result of the proof. Yes or No only... *)
}

let env = {
  context = global_context ();
  target = Llvm_target.TargetData.create ""; (* has to be filled with a legal value later *)
  logic = empty_logic;
  abs_rules = empty_logic;
  specs = [];
  gvars = [];
  idMap = IdMap.empty;
  result = true;
}

let warn s =
  print_endline ("WARNING: "^s)

(** gets names of named and unnamed variables *)
(* Ideally, we would get them from the SlotTracker thingy in
   lib/VMCore/AsmWriter.cpp, but it's not in the ocaml bindings... *)
let value_id v =
  let id = value_name v in
  if id = "" then
    try
      IdMap.find v env.idMap
    with Not_found ->
      let id = "%"^(string_of_int (IdMap.cardinal env.idMap + 1)) in
      env.idMap <- IdMap.add v id env.idMap;
      id
  else id

(** return value expression *)
let ret_arg = Arg_var(Spec.ret_v1)

(* shorthands for integer expressions *)
let args_num i = Arg_op("numeric_const",[Arg_string (string_of_int i)])
let args_num_0 = Arg_op("numeric_const",[Arg_string "0"])
let args_num_1 = Arg_op("numeric_const",[Arg_string "1"])

(* a few functions for creating predicates. Adds a layer of
   type-safety and avoids catastrophic typos *)
let mkPointer ptr ptr_t v = mkSPred ("pointer", [ptr; ptr_t; v])
let mkArray ptr start_idx end_idx size array_t v =
  mkSPred ("array", [ptr; start_idx; end_idx; size; array_t; v])

let mkEmptySpec = Spec.mk_spec mkEmpty mkEmpty Spec.ClassMap.empty

let env_add_logic_seq_rules sr  =
  env.logic <- { env.logic with seq_rules = env.logic.seq_rules@sr }

(** We want fewer of these *)
let implement_this s = failwith ("Not implemented: "^s)

let spec_of_fun_id fid =
  let rec aux = function
    | Logic_spec.Funspec(i, spec)::ss when i = fid -> spec
    | _::ss -> aux ss
    | [] -> warn ("no spec found for "^fid); mkEmptySpec in
  aux env.specs


(* The real stuff begins *)

(* Part I: turning llvm types and values into CoreStar expressions (args) *)

let rec args_of_type t = match (classify_type t) with
  | Void -> Arg_op("void_type",[])
  | Float
  | Half
  | Double
  | X86fp80
  | Fp128
  | Ppc_fp128 -> Arg_op("float_type", [])
  | Label -> Arg_op("label type", [])
  | Integer -> Arg_op("integer_type", [])
  | TypeKind.Function -> (* silly name clash *)
    let ret_type = return_type t in
    let par_types = param_types t in
    Arg_op("function_type", args_of_type ret_type::(args_of_type_array par_types))
  | Struct -> (
    if is_opaque t then Arg_op("opaque_type", [])
    else match struct_name t with
      | None ->
	let elts_types = struct_element_types t in
	Arg_op("struct_type", args_of_type_array elts_types)
      | Some n ->
	Arg_op("named_type", [Arg_string n])
  )
  | Array ->
    let elt_t = element_type t in
    Arg_op("array_type", [args_of_type elt_t])
  | Pointer ->
    let elt_t = element_type t in
    Arg_op("pointer_type", [args_of_type elt_t])
  | Vector ->
    let elt_t = element_type t in
    Arg_op("vector_type", [args_of_type elt_t])
  | Metadata -> raise (MetaData (const_null t))
and args_of_type_array ta =
  Array.to_list (Array.map args_of_type ta)

let args_of_int_const v = match int64_of_const v with
  | Some i -> Arg_op("numeric_const", [Arg_string(Int64.to_string i)])
  | None -> Arg_var (Vars.freshe ())

let rec args_of_const_expr v = match constexpr_opcode v with
  | Opcode.Add ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_plus", [x; y])
  | Opcode.Sub ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_minus", [x; y])
  | Opcode.Mul ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_mult", [x; y])
  | Opcode.UDiv
  | Opcode.SDiv ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_div", [x; y])
  | Opcode.URem
  | Opcode.SRem ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_rem", [x; y])
  | Opcode.FAdd
  | Opcode.FSub
  | Opcode.FMul
  | Opcode.FDiv
  | Opcode.FRem ->
    (* TODO: This is the only safe thing until floats are supported *)
    Arg_var (Vars.freshe ())    
  (* TODO@CoreStar: translate bitwise operation to z3 *)  
  | Opcode.Shl ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_shl", [x; y])
  | Opcode.LShr
  | Opcode.AShr ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_shr", [x; y])
  | Opcode.And ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_and", [x; y])
  | Opcode.Or ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_or", [x; y])
  | Opcode.Xor ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_xor", [x; y])
  (* /TODO CoreStar/z3 *)
  | Opcode.Unwind | Opcode.LandingPad | Opcode.Resume | Opcode.AtomicRMW
  | Opcode.AtomicCmpXchg | Opcode.Fence | Opcode.InsertValue
  | Opcode.ExtractValue | Opcode.ShuffleVector | Opcode.InsertElement
  | Opcode.ExtractElement | Opcode.VAArg | Opcode.UserOp2 | Opcode.UserOp1
  | Opcode.Select | Opcode.Call | Opcode.PHI | Opcode.FCmp | Opcode.ICmp
  | Opcode.BitCast | Opcode.IntToPtr | Opcode.PtrToInt | Opcode.FPExt
  | Opcode.FPTrunc | Opcode.SIToFP | Opcode.UIToFP | Opcode.FPToSI
  | Opcode.FPToUI | Opcode.SExt | Opcode.ZExt | Opcode.Trunc
  | Opcode.GetElementPtr | Opcode.Store | Opcode.Load | Opcode.Alloca
  | Opcode.Unreachable | Opcode.Invalid2 | Opcode.Invoke | Opcode.IndirectBr
  | Opcode.Switch | Opcode.Br | Opcode.Ret | Opcode.Invalid ->
    (* TODO: implement *)
    Arg_var (Vars.freshe ())

and args_of_value v = match classify_value v with
  | NullValue -> Arg_op("zeroinitializer", [])
  | Argument -> Arg_var (Vars.concretep_str (value_id v))
  | BasicBlock -> failwith "Invalid bitcode? Unexpected BasickBlock value"
  | InlineAsm -> Arg_var (Vars.freshe ())
  | MDNode -> raise (MetaData v)
  | MDString -> raise (MetaData v)
  | BlockAddress -> Arg_op("block_addr", [args_of_value (operand v 0)])
  | ConstantAggregateZero ->  Arg_op("zeroinitializer", [])
  | ConstantArray -> args_of_composite_value "array" v
  | ConstantExpr -> args_of_const_expr v
  | ConstantFP -> Arg_var (Vars.freshe ())
  | ConstantInt -> args_of_int_const v
  | ConstantPointerNull -> Arg_op("zeroinitializer", [])
  | ConstantStruct -> args_of_composite_value "struct" v
  | ConstantVector -> args_of_composite_value "vector" v
  | Function -> Arg_op("function", [args_of_value (operand v 0)])
  | GlobalAlias -> implement_this "value is a global alias" (* undocumented? *)
  | GlobalVariable -> Arg_var (Vars.concretep_str (value_id v))
  | UndefValue -> Arg_op("undef", [])
  | Instruction op -> Arg_var (Vars.concretep_str (value_id v))

and args_of_composite_value aggr v =
  let size = num_operands v in
  let rec args_of_ops n =
    if n < size then args_of_value (operand v n)::(args_of_ops (n+1))
    else [] in
  Arg_op(aggr, args_of_ops 0)


(** compute the predicate describing the shape of an object of type t pointed to by p in memory *)
let rec spred_of_type ptr t = match (classify_type t) with
  | Void
  | Float
  | Half
  | Double
  | X86fp80
  | Fp128
  | Ppc_fp128
  | Label
  | Integer -> mkEmpty
  | TypeKind.Function -> (* silly name conflict with ValueKind *)
    implement_this "SPred of function type"
  | Struct
  | Pointer ->
    let ptr_t = args_of_type t in
    let e = Arg_var (Vars.freshe ()) in
    mkPointer ptr ptr_t e
  | Array ->
    let ptr_t = args_of_type t in
    let size = Arg_op ("numeric_const", [Arg_string (string_of_int (array_length t))]) in
    let e = Arg_var (Vars.freshe ()) in
    let start = Arg_op ("numeric_const", [Arg_string "0"]) in
    mkArray ptr start size size ptr_t e
  | Vector
  | Metadata -> mkEmpty

(** compute the pure predicate corresponding to a getelementpointer instruction *)
let ppred_of_gep x t ptr lidx =
  let rec jump_chain_of_lidx = function
    | [] -> Arg_op ("jump_end", [])
    | idx::tl ->
      let args_idx = args_of_value idx in
      Arg_op ("jump", [args_idx; jump_chain_of_lidx tl]) in
  let jump_chain = jump_chain_of_lidx lidx in
  mkPPred ("eltptr", [x; args_of_type t; ptr; jump_chain])

(** compile an llvm instruction into a CoreStar program *) 
let cfg_node_of_instr instr = match instr_opcode instr with
  (* Terminator Instructions *)
  | Opcode.Ret ->
    if num_operands instr != 0 then
      let ret_val = operand instr 0 in
      let p0 = Arg_var(Vars.concretep_str ("@parameter"^(string_of_int 0)^":")) in
      let post = mkEQ(ret_arg, p0) in
      let spec = Spec.mk_spec [] post Spec.ClassMap.empty in
      mk_node (Core.Assignment_core  ([], spec, [args_of_value ret_val]))
    else mk_node (Core.End)
  | Opcode.Br ->
    (* this is how you get a block's label using the ocaml bindings... *)
    let label_of_bblock b =
      value_id (value_of_block b) in
    if num_operands instr = 1 then
      (* Unconditional branch instr *)
      let next_label = label_of_bblock (block_of_value (operand instr 0)) in
      mk_node (Core.Goto_stmt_core [next_label])
    else
      (* Conditional branch instr, num_operands instr = 3 *)
      let then_label = label_of_bblock (block_of_value (operand instr 1)) in
      let else_label = label_of_bblock (block_of_value (operand instr 2)) in
      (* TODO: implement cond *)
      mk_node (Core.Goto_stmt_core [then_label;else_label])
  | Opcode.Switch -> implement_this "switch instruction"
  | Opcode.IndirectBr -> implement_this "indirect branch"
  | Opcode.Invoke -> implement_this "Invoke block terminator"
  | Opcode.Invalid2 -> failwith "\"Invalid\" instruction"
  | Opcode.Unreachable ->
    (* if llvm thinks it's unreachable, let's tell hopstar by assuming False *)
    let spec = Spec.mk_spec [] mkFalse Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([],spec,[]))
  | Opcode.Invalid -> failwith "\"Invalid\" instruction"
  (* Standard Binary Operators *)
  | Opcode.Add
  | Opcode.FAdd ->
    let id = value_id instr in
    let v1 = args_of_value (operand instr 0) in
    let v2 = args_of_value (operand instr 1) in
    let pre = mkEmpty in
    let post = mkEQ (ret_arg, Arg_op("builtin_plus", [v1; v2])) in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))
  | Opcode.Sub
  | Opcode.FSub ->
    let id = value_id instr in
    let v1 = args_of_value (operand instr 0) in
    let v2 = args_of_value (operand instr 1) in
    let pre = mkEmpty in
    let post = mkEQ (ret_arg, Arg_op("builtin_minus", [v1; v2])) in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))
  | Opcode.Mul
  | Opcode.FMul ->
    let id = value_id instr in
    let v1 = args_of_value (operand instr 0) in
    let v2 = args_of_value (operand instr 1) in
    let pre = mkEmpty in
    let post = mkEQ (ret_arg, Arg_op("builtin_mult", [v1; v2])) in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))
  | Opcode.UDiv
  | Opcode.SDiv
  | Opcode.FDiv ->
    let id = value_id instr in
    let v1 = args_of_value (operand instr 0) in
    let v2 = args_of_value (operand instr 1) in
    let pre = mkEmpty in
    let post = mkEQ (ret_arg, Arg_op("builtin_div", [v1; v2])) in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))
  | Opcode.URem
  | Opcode.SRem
  | Opcode.FRem -> implement_this "?rem instr"
  (* Logical Operators *)
  | Opcode.Shl
  | Opcode.LShr
  | Opcode.AShr
  | Opcode.And
  | Opcode.Or
  | Opcode.Xor -> implement_this "bitwise op instr"
  (* Memory Operators *)
  | Opcode.Alloca ->
    let id = value_id instr in
    let t = type_of instr in
    let x = Arg_var (Vars.concretep_str id) in
    let e = Arg_var (Vars.freshe ()) in
    let ptr_t = args_of_type t in
    let pointed_type = element_type t in
    let alloca = mkSPred ("alloca", [x; ptr_t]) in
    let pointer = mkPointer x ptr_t e in
    let post = mkStar alloca (mkStar pointer (spred_of_type e pointed_type)) in
    let spec = Spec.mk_spec [] post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([], spec, []))
  | Opcode.Load ->
    let id = value_id instr in
    (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l00225 *)
    let ptr_v = operand instr 0 in
    let e = Arg_var (Vars.freshe ()) in
    let ptr = args_of_value ptr_v in
    let ptr_t = args_of_type (type_of ptr_v) in
    let pointer = mkPointer ptr ptr_t e in
    let pre = pointer in
    let post = pconjunction (mkEQ(ret_arg, e)) pointer in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([Vars.concretep_str id], spec, []))
  | Opcode.Store ->
    (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l00343 *)
    let value = operand instr 0 in
    (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l00346 *)
    let ptr_v = operand instr 1 in
    let t = type_of ptr_v in
    let ptr = args_of_value ptr_v in
    let e = Arg_var (Vars.freshe ()) in
    let v = args_of_value value in
    let ptr_t = args_of_type t in
    let pointer_pre = mkPointer ptr ptr_t e in
    let pointer_post = mkPointer ptr ptr_t v in
    let pre = pointer_pre in
    let post = pointer_post in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([], spec, []))
  | Opcode.GetElementPtr ->
    let id = value_id instr in
    (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l00788 *)
    let value = operand instr 0 in
    (* jump indices are in operands 1 to (num_operands instr) of instr *)
    let max_op = num_operands instr in
    let rec jlist_from_op i =
      if i = max_op then []
      else operand instr i::(jlist_from_op (i+1)) in
    let jump_indices = jlist_from_op 1 in
    let post = ppred_of_gep ret_arg (type_of value) (args_of_value value) jump_indices  in
    let spec = Spec.mk_spec [] post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))
  (* Cast Operators *)
  | Opcode.BitCast when integer_bitwidth (element_type (type_of (operand instr 0))) = 8 ->
    (* hack to handle malloc + bitcast. Let's hope for the best... *)
    (* TODO: really, we should identify mallocs with more accuracy
       (LLVM knows about them, but then translates them away...) *)
    (* TODO: only works when the type is sized. Should have a
       best-scenario (ever so slightly unsound) heuristic for when it
       isn't (i.e.: assume the malloc blob is of the right size. *)
    let id = value_id instr in
    let value = operand instr 0 in
    let new_t = element_type (type_of instr) in
    let s = Llvm_target.store_size env.target new_t in
    let size_of_new_t = Arg_op("numeric_const", [Arg_string(Int64.to_string s)]) in
    let v = args_of_value value in
    let e = Arg_var (Vars.freshe ()) in
    let ptr_new_t = args_of_type (type_of instr) in
    let blob = mkSPred ("blob", [v;size_of_new_t]) in
    let cast_pointer = mkPointer ret_arg ptr_new_t e in
    let pre = blob in
    let post = pconjunction (mkEQ(ret_arg, v))
      (mkStar cast_pointer (spred_of_type e new_t)) in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))
  | Opcode.BitCast
  | Opcode.Trunc
  | Opcode.ZExt
  | Opcode.SExt
  | Opcode.FPToUI
  | Opcode.FPToSI
  | Opcode.UIToFP
  | Opcode.SIToFP
  | Opcode.FPTrunc
  | Opcode.FPExt
  | Opcode.PtrToInt
  | Opcode.IntToPtr ->
    let id = value_id instr in
    let value = operand instr 0 in
    let v = args_of_value value in
    let pre = mkEmpty in
    let post = mkEQ(ret_arg, v) in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))
  (* Other Operators *)
  | Opcode.ICmp ->
    let id = value_id instr in
    let v1 = args_of_value (operand instr 0) in
    let v2 = args_of_value (operand instr 1) in
    let pre = mkEmpty in
    let post = mkEQ (ret_arg, Arg_op("builtin_eq", [v1; v2])) in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))
  | Opcode.FCmp -> implement_this "fcmp instr"
  | Opcode.PHI -> implement_this "phi instr"
  | Opcode.Call -> (
    try
      let id = value_id instr in
      (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l01339 *)
      let fun_called = operand instr (num_operands instr - 1) in
      let fid = value_id fun_called in
      (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l01237 *)
      let max_param_idx = num_operands instr - 1 in
      let rec params_from_idx i =
	if i = max_param_idx then []
	else args_of_value (operand instr i)::(params_from_idx (i+1)) in
      let params = params_from_idx 0 in
      let call_spec = spec_of_fun_id fid in
      mk_node (Core.Assignment_core ([Vars.concretep_str id],
				     call_spec,
				     params))
    with MetaData _ ->
      (* a function call with meta-data in its arguments is for debug info *)
      (* and may be safely ignored *)
      mk_node Core.Nop_stmt_core)
  | Opcode.Select -> implement_this "select instr"
  | Opcode.UserOp1
  | Opcode.UserOp2 ->
    warn "Skipping user-defined instruction";
    mk_node Core.Nop_stmt_core
  | Opcode.VAArg ->
    warn "Skipping VAArg instruction";
    mk_node Core.Nop_stmt_core
  | Opcode.ExtractElement
  | Opcode.InsertElement
  | Opcode.ShuffleVector
  | Opcode.ExtractValue
  | Opcode.InsertValue ->
    warn "Skipping vector instruction";
    mk_node Core.Nop_stmt_core
  | Opcode.Fence ->
    (* this skip is safe since we assume a sequential semantics *)
    mk_node Core.Nop_stmt_core
  | Opcode.AtomicCmpXchg -> implement_this "atomic cmp xchange instr"
  | Opcode.AtomicRMW -> implement_this "atomic RMW instr"
  | Opcode.Resume -> implement_this "resume instr"
  | Opcode.LandingPad -> mk_node Core.Nop_stmt_core
  | Opcode.Unwind -> implement_this "unwind"

let cfg_nodes_of_block b =
  (* insert label command from the block's label l, followed by the sequence
     of commands of the block *)
  let l = value_id (value_of_block b) in
  let label_node = if l = "" then mk_node Core.Nop_stmt_core
    else mk_node (Core.Label_stmt_core l) in
  let body_nodes = fold_left_instrs (fun cfgs i -> cfgs@[cfg_node_of_instr i]) [] b in
  label_node::body_nodes

let verify_function f =
  let id = value_id f in
  if not (is_declaration f) then (
    print_endline ("verifying "^id);
    let cfg_nodes = fold_left_blocks (fun ns b -> ns@(cfg_nodes_of_block b)) [] f in
    let spec = spec_of_fun_id id in
    env.result <- env.result &&
      (Symexec.verify id cfg_nodes spec env.logic env.abs_rules)
  )

let env_add_gvar gvar =
  env.gvars <- (value_id gvar, args_of_type (type_of gvar), args_of_value gvar)::env.gvars

let verify_module m =
  iter_globals env_add_gvar m;
  iter_functions verify_function m


(** Outputs logical rules to teach CoreStar how to fold and unfold a
    named struct of name @name and type @t. *)
let logic_of_named_struct (name,t) =
  (* first, let's define a bunch of stuff that will be useful for more or
     less all the rules *)
  let types = struct_element_types t in
  let args_t = args_of_type t in
  let args_ptr_t = args_of_type (pointer_type t) in
  let i8_ptr_t = args_of_type (pointer_type (i8_type (global_context ()))) in
  let struct_size =
    Arg_op("numeric_const",
	   [Arg_string(Int64.to_string (Llvm_target.store_size env.target t))]) in
  let subpointer p off_to_val offset subt =
    (mkPointer
       (Arg_op ("builtin_plus", [p; args_num offset]))
       (Arg_op ("pointer_type", [args_of_type subt]))
       (off_to_val (string_of_int offset))) in
  let any_var x = Arg_var (Vars.AnyVar (0, x)) in
  let p = any_var "p" in
  let vp = any_var "vp" in
  let x = any_var "x" in
  let j = any_var "j" in
  let st = any_var "st" in
  let v = any_var "v" in
  let target_pointer = mkPointer x st v in

  (* TODO: narrate me *)
  (* eltptr_concl and etlptr_prem are also used in the next rule, so be 
     careful if you modify those *)
  let subpointers_fields =
    Array.mapi
      (subpointer p (fun offset -> Arg_op ("field", [Arg_op ("numeric_const", [Arg_string offset]); vp])))
      types in
  let subpointers_indep =
    Array.mapi
      (subpointer p (fun offset -> vp))
      types in
  let unfolded_form = Array.fold_right (fun p f -> mkStar p f) subpointers_fields mkEmpty in
  let geteltptr_rule rule_name eltptr_concl eltptr_prem target offset subptr =
    let conclusion_lhs =
      pconjunction (eltptr_concl offset)
	(mkPointer p (Arg_op ("named_type", [Arg_string name])) vp) in
    let conclusion_rhs = target offset in
    let conclusion = (mkEmpty, conclusion_lhs, conclusion_rhs, mkEmpty) in
    let premise_lhs = pconjunction (eltptr_prem offset) unfolded_form in
    let premise_rhs = target offset in
    let premise = (mkEmpty, premise_lhs, premise_rhs, mkEmpty) in
    (conclusion, [[premise]], name^"_"^rule_name^(string_of_int offset), ([], []), []) in
  let geteltptr_unfold_rule rule_name eltptr_concl eltptr_prem target offset subptr =
    let conclusion = (mkEmpty, pconjunction (eltptr_concl offset) subptr,
		      target offset, mkEmpty) in
    let premise = (mkEmpty, pconjunction (eltptr_prem offset) subptr,
		   target offset, mkEmpty) in
    (conclusion, [[premise]], name^"_unfold_"^rule_name^(string_of_int offset),
     ([], []), []) in
  let eltptr_struct_concl i =
    mkPPred ("eltptr", [x; args_t; p; Arg_op ("jump", [args_num i; j])]) in
  let eltptr_struct_prem i =
    let ith_t = args_of_type (Array.get types i) in
    mkPPred ("eltptr", [x; ith_t; Arg_op ("builtin_plus", [p; args_num i]); j]) in
  let eltptr_raw_concl i =
    let jump_offset = Int64.to_string (Llvm_target.offset_of_element env.target t i) in
    mkPPred ("eltptr",
	     [x; i8_ptr_t; p;
	      Arg_op ("jump", [Arg_op("numeric_const", [Arg_string jump_offset]); j])]) in
  let eltptr_raw_prem i =
    let ith_t = args_of_type (Array.get types i) in
    mkPPred ("eltptr", [x; ith_t; Arg_op ("builtin_plus", [p; args_num i]); j]) in
  let target_ptr_fun i = target_pointer in
  let target_blob_fun i =
    let ith_elt_size = Llvm_target.store_size env.target (Array.get types i) in
    let a = Arg_op("numeric_const", [Arg_string(Int64.to_string ith_elt_size)]) in
    mkSPred ("blob", [x; a]) in
  let gep_generators =
    geteltptr_rule "gep_ptr" eltptr_struct_concl eltptr_struct_prem target_ptr_fun
    ::geteltptr_rule "gep_blob" eltptr_struct_concl eltptr_struct_prem target_blob_fun
    ::geteltptr_rule "raw_gep_ptr" eltptr_raw_concl eltptr_raw_prem target_ptr_fun
    ::geteltptr_rule "raw_gep_blob" eltptr_raw_concl eltptr_raw_prem target_blob_fun
    ::[] in
  let gep_unfold_generators =
    geteltptr_unfold_rule "gep_ptr" eltptr_struct_concl eltptr_struct_prem target_ptr_fun
    ::geteltptr_unfold_rule "gep_blob" eltptr_struct_concl eltptr_struct_prem target_blob_fun
    ::geteltptr_unfold_rule "raw_gep_ptr" eltptr_raw_concl eltptr_raw_prem target_ptr_fun
    ::geteltptr_unfold_rule "raw_gep_blob" eltptr_raw_concl eltptr_raw_prem target_blob_fun
    ::[] in
  let geteltptr_rules =
    List.flatten (List.map
		    (fun gep_gen ->
		      Array.to_list (Array.mapi gep_gen subpointers_fields))
		    gep_generators) in
  let geteltptr_unfold_rules =
    List.flatten (List.map
		    (fun gep_gen ->
		      Array.to_list (Array.mapi gep_gen subpointers_indep))
		    gep_unfold_generators) in

  (* TODO: narrate me *)
  let subpointers =
    Array.mapi
      (subpointer p (fun off -> any_var ("v"^off)))
      types in
  let unfolded_form = Array.fold_right (fun p f -> mkStar p f) subpointers mkEmpty in
  let named_pointer = mkPointer p args_t vp in
  let conclusion = (mkEmpty, unfolded_form, named_pointer, mkEmpty) in
  let premise = (unfolded_form, mkEmpty, mkEmpty, mkEmpty) in
  let fold_struct_rule = (conclusion, [[premise]], name^"_fold", ([], []), []) in
  let sz = any_var "sz" in
  let blob =  mkSPred ("blob", [p; sz]) in
  let conclusion = (mkEmpty, unfolded_form, blob, mkEmpty) in
  let premise = (unfolded_form, mkEmpty, mkEQ(sz,struct_size), mkEmpty) in
  let fold_blob_rule = (conclusion, [[premise]], name^"_blob_fold", ([], []), []) in
  
  (* blob of full size in the rhs, folded struct in the lhs *)
  let struct_pointer = mkPointer x args_t v in
  let blob = mkSPred ("blob", [x; struct_size]) in
  let conclusion = (mkEmpty, struct_pointer, blob, mkEmpty) in
  let premise = (struct_pointer, mkEmpty, mkEmpty, mkEmpty) in
  let folded_entails_blob =
    (conclusion, [[premise]], name^"_blob_folded", ([], []), []) in

  (* blob of the full size in the rhs, unfolded struct in the lhs *)
  let subpointers =
    Array.mapi
      (subpointer p (fun off -> any_var ("v"^off)))
      types in (* subpointers are reused down below, be careful *)
  let unfolded_form = Array.fold_right (fun p f -> mkStar p f) subpointers mkEmpty in
  let blob = mkSPred ("blob", [x; struct_size]) in
  let conclusion = (mkEmpty, unfolded_form, blob, mkEmpty) in
  let premise = (unfolded_form, mkEmpty, mkEmpty, mkEmpty) in
  let unfolded_entails_blob =
    (conclusion, [[premise]], name^"_blob_unfolded", ([], []), []) in

  (* blob of the size of the first element in the rhs, folded struct in the lhs *)
  let folded_entails_first_blob =
    try
      let t0 = Array.get types 0 in
      let p0 = Array.get subpointers 0 in
      let ptl = Array.sub subpointers 1 (Array.length subpointers -1) in
      let first_elt_size =
	Arg_op("numeric_const",
	       [Arg_string(Int64.to_string (Llvm_target.store_size env.target t0))]) in
      let blob = mkSPred ("blob", [p; first_elt_size]) in
      let ptr_to_struct = mkPointer p args_ptr_t x in
      let struct_ptr = mkPointer x args_t v in
      let conclusion = (mkEmpty,  mkStar ptr_to_struct struct_ptr, blob, mkEmpty) in
      let premise = (p0, Array.fold_right (fun p f -> mkStar p f) ptl mkEmpty,
		     mkEmpty, mkEmpty) in
      [(conclusion, [[premise]], name^"_blob_first", ([], []), [])]
    with Invalid_argument _ -> [] (* Yep, a C struct can have an empty declaration. *) in

  (* return a list of all the rules defined above *)
  geteltptr_rules@geteltptr_unfold_rules@fold_struct_rule::fold_blob_rule::folded_entails_blob::unfolded_entails_blob::folded_entails_first_blob


(************** Collect all the named structs in a module *)
(* this would not have to exist if findUsedStructTypes from
   lib/VMCore/Module.cpp was accessible from the OCaml or C
   bindings
*)

(** looks for new named structs in the type t *)
let rec collect_named_structs_of_type (seen_t, seen_const, lns) t =
  if (LltypeSet.mem t seen_t) then (seen_t, seen_const, lns)
  else
    let seen_t = LltypeSet.add t seen_t in
    match (classify_type t) with
    | Struct -> (
      match struct_name t with
	| None ->
	  (* FIXME: is this right? maybe we should name all structs... *)
	  (seen_t, seen_const, lns)
	| Some n ->
	  if List.mem_assoc n lns then (seen_t, seen_const, lns)
	  else
	    let lns = (n,t)::lns in
	    Array.fold_left collect_named_structs_of_type
	      (seen_t, seen_const, lns) (struct_element_types t)
    )
    | Pointer
    | Array
    | Vector -> collect_named_structs_of_type (seen_t, seen_const, lns) (element_type t)
    | _ -> (seen_t, seen_const, lns)

(** looks for new named structs in the value v *)
let rec collect_named_structs_of_value o v =
  let (seen_t, seen_const, lns) = collect_named_structs_of_type o (type_of v) in
  (*  if v is not a constant, then its structs have already been collected explicitly *)
  if (not (is_constant v)) || (LlvalueSet.mem v seen_const) then (seen_t, seen_const, lns)
  else
    let seen_const = LlvalueSet.add v seen_const in
    let o = (seen_t, seen_const, lns) in
    let num_op = num_operands v in
    let rec collect_from_op o n =
      if n = num_op then o
      else (
	let o = collect_named_structs_of_value o (operand v n) in
	collect_from_op o (n+1)) in
    collect_from_op o 0

(** looks for new named structs in the instruction i *)
let collect_named_structs_of_instr o i =
  let num_op = num_operands i in
  let o = collect_named_structs_of_type o (type_of i) in
  let rec collect_from_op o n =
    if n = num_op then o
    else (
      let o = collect_named_structs_of_value o (operand i n) in
      collect_from_op o (n+1)) in
  collect_from_op o 0

let collect_named_structs_of_block o b =
  fold_left_instrs collect_named_structs_of_instr o b

let collect_named_structs_of_function o f =
  let o = collect_named_structs_of_type o (type_of f) in
  fold_left_blocks collect_named_structs_of_block o f

(** collects all named structs that are referred to by the functions of module m *)
let collect_named_structs_of_module m =
  (* we only care about the named structs that are used inside functions,
     so let's collect only those *)
  let o = (LltypeSet.empty,LlvalueSet.empty,[]) in
  let (_, _, ltf) = fold_left_functions collect_named_structs_of_function o m in
  print_endline ("found "^(string_of_int (List.length ltf))^" struct(s)"); ltf

(************** /Collect all the named structs in a module *)

let logic_of_module m =
  let ltf = collect_named_structs_of_module m in
  List.flatten (List.map logic_of_named_struct ltf)

(** dumps the generated fold/unfold logic into a file *)
let dump_logic_rules rs =
  let file = Sys.getcwd() ^  "/.logic_rules.txt" in
  let rules_out = open_out file in
  let rules_fmt = Format.formatter_of_out_channel rules_out in
  List.iter (pp_sequent_rule rules_fmt) rs

let go logic abs_rules spec_list m =
  print_endline "It is on!";
  env.context <- module_context m;
  env.target <- Llvm_target.TargetData.create (data_layout m);
  env.logic <- logic; env.abs_rules <- abs_rules; env.specs <- spec_list;
  print_endline ("Added specs for "^string_of_int (List.length spec_list)^" functions");
  print_endline "Adding unfolding logic for named structs... ";
  let l = logic_of_module m in
  env_add_logic_seq_rules l;
  print_endline (string_of_int (List.length l)^" rules added");
  dump_logic_rules l;
  if false then List.iter (pp_sequent_rule Format.std_formatter) l;
  verify_module m;
  env.result
