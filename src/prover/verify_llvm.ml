open Printf
open Llvm
open Llvm.TypeKind
open Llvm.ValueKind
open Cfg_core
open Psyntax
open Logic_spec

type verify_env = {
  mutable target: Llvm_target.TargetData.t;
  mutable logic: Psyntax.logic;
  mutable abs_rules: Psyntax.logic;
  mutable specs: funspec list;
  mutable gvars: (string * args * args) list; (** global variable: id * type * value *)
  mutable idMap: (llvalue * string) list; (* TODO: make that a real map *)
  mutable result: bool;
}

let env = {
  target = Llvm_target.TargetData.create "";
  logic = empty_logic;
  abs_rules = empty_logic;
  specs = [];
  gvars = [];
  idMap = [];
  result = true;
}

(* placeholder for a better way of getting names of unnamed variables *)
let value_id v =
  let id = value_name v in
  if id = "" then
    try
      List.assoc v env.idMap
    with Not_found ->
      (* TODO: write a real fresh identifier generator *)
      let id = "%"^(string_of_int (List.length env.idMap)) in
      env.idMap <- (v, id)::env.idMap;
      id
  else id

let ret_arg = Arg_var(Spec.ret_v1)

let args_num_0 = Arg_op("numeric_const",[Arg_string "0"])
let args_num_1 = Arg_op("numeric_const",[Arg_string "1"])

let mkPointer ptr ptr_t v = mkSPred ("pointer", [ptr; ptr_t; v])
let mkArray ptr start_idx end_idx size array_t v =
  mkSPred ("array", [ptr; start_idx; end_idx; size; array_t; v])

let mkEmptySpec = Spec.mk_spec mkEmpty mkEmpty Spec.ClassMap.empty

let env_add_gvar gvar =
  env.gvar <- (value_id gvar, args_of_type (type_of gvar), args_of_value gvar)

let env_add_logic_seq_rules sr  =
  env.logic <- { env.logic with seq_rules = env.logic.seq_rules@sr }

let implement_this s = failwith ("Not implemented: "^s)

let spec_of_fun_id fid =
  let rec aux = function
    | Logic_spec.Funspec(i, spec)::ss when i = fid -> spec
    | _::ss -> aux ss
    | [] -> print_endline ("WARN: no spec found for "^fid); mkEmptySpec in
  aux env.specs

let rec args_of_type t = match (classify_type t) with
  | Void -> Arg_op("void_type",[])
  | Float
  | Double
  | X86fp80
  | Fp128
  | Ppc_fp128 -> Arg_op("float_type", [])
  | Label -> implement_this "label type"
  | Integer -> Arg_op("integer_type", [])
  | TypeKind.Function ->
    let ret_type = return_type t in
    let par_types = param_types t in
    Arg_op("function_type", args_of_type ret_type::(args_of_type_array par_types))
  | Struct -> (
    match struct_name t with
      | None ->
	print_string "UNUSUAL: unnamed struct\n";
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
  | Metadata -> implement_this "metadata type"
and args_of_type_array ta =
  Array.to_list (Array.map args_of_type ta)

let args_of_int_const v = match int64_of_const v with
  | Some i -> Arg_op("numeric_const", [Arg_string(Int64.to_string i)])
  | None -> Arg_var (Vars.freshe ())

let rec args_of_const_expr v = match constexpr_opcode v with
  (* TODO: implement floats *)
  | Opcode.Add
  | Opcode.FAdd ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_plus", [x; y])
  | Opcode.Sub
  | Opcode.FSub ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_minus", [x; y])
  | Opcode.Mul
  | Opcode.FMul ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_mult", [x; y])
  | Opcode.UDiv
  | Opcode.SDiv
  | Opcode.FDiv ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_div", [x; y])
  | Opcode.URem
  | Opcode.SRem
  | Opcode.FRem ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_rem", [x; y])
  | Opcode.Shl
  | Opcode.LShr
  | Opcode.AShr
  | Opcode.And
  | Opcode.Or
  | Opcode.Xor -> Arg_var (Vars.freshe ()) (* TODO: implement *)
  | _ -> Arg_var (Vars.freshe ()) (* TODO: implement *)

and args_of_value v = match classify_value v with
  | NullValue -> args_num_0
  | Argument -> Arg_var (Vars.concretep_str (value_id v))
  | BasicBlock -> implement_this "value is a basic block"
  | InlineAsm -> implement_this "value is inlined assembly"
  | MDNode -> implement_this "value is a metadata node"
  | MDString -> implement_this "value is a metadata string"
  | BlockAddress -> implement_this "value is a block address"
  | ConstantAggregateZero -> implement_this "value is an aggregate 0"
  | ConstantArray -> implement_this "value is an array"
  | ConstantExpr -> args_of_const_expr v
  | ConstantFP -> implement_this "value is floating point"
  | ConstantInt -> args_of_int_const v
  | ConstantPointerNull -> args_num_0
  | ConstantStruct -> implement_this "value is a struct"
  | ConstantVector -> implement_this "value is a vector"
  | Function -> implement_this "value is a function"
  | GlobalAlias -> implement_this "value is a global alias"
  | GlobalVariable -> Arg_var (Vars.concretep_str (value_id v))
  | UndefValue -> Arg_op("undef", [])
  | Instruction op -> Arg_var (Vars.concretep_str (value_id v))

let rec spred_of_type ptr t = match (classify_type t) with
  | Void
  | Float
  | Double
  | X86fp80
  | Fp128
  | Ppc_fp128
  | Label
  | Integer -> mkEmpty
  | TypeKind.Function -> (* stupid name conflict with ValueKind *)
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
  | Vector -> implement_this "SPred of vector"
  | Metadata -> implement_this "SPred of metadata"


let ppred_of_gep x ptr lidx =
  let rec jump_chain_of_lidx = function
    | [] -> Arg_op ("jump_end", [])
    | idx::tl ->
      let args_idx = args_of_value idx in
      Arg_op ("jump", [args_idx; jump_chain_of_lidx tl]) in
  let jump_chain = jump_chain_of_lidx lidx in
  mkPPred ("eltptr", [x; ptr; jump_chain])

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
      let l = value_id (value_of_block b) in
      if l = "" then
	(* if this ever shows up, we need to give a fresh label to the
	   target block *)
	print_string "FIXME: branch to a block with no label";
      l in
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
    let post = ppred_of_gep ret_arg (args_of_value value) jump_indices  in
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
    let ptr_t = args_of_type (type_of instr) in
    let malloc_blob = mkSPred ("malloc_blob", [v;size_of_new_t]) in
    let cast_pointer = mkPointer ret_arg ptr_t e in
    let malloced = mkSPred ("malloced", [ret_arg; args_of_type new_t]) in
    let pre = malloc_blob in
    let post = pconjunction (mkEQ(ret_arg, v))
      (mkStar malloced (mkStar cast_pointer (spred_of_type e new_t))) in
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
  | Opcode.Call ->
    let id = value_id instr in
    (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l01339 *)
    let fun_called = operand instr (num_operands instr - 1) in
    let fid = value_id fun_called in
    (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l01237 *)
    let max_param_idx = num_operands instr - 1 in
    let rec params_from_idx i =
      if i = max_param_idx then []
      else operand instr i::(params_from_idx (i+1)) in
    let params = params_from_idx 0 in
    let call_spec = spec_of_fun_id fid in
    mk_node (Core.Assignment_core ([Vars.concretep_str id],
				   call_spec,
				   List.map args_of_value params))
  | Opcode.Select -> implement_this "select instr"
  | Opcode.UserOp1 -> implement_this "userop1 instr"
  | Opcode.UserOp2 -> implement_this "userop2 instr"
  | Opcode.VAArg -> implement_this "va arg instr"
  | Opcode.ExtractElement -> implement_this "extract element instr"
  | Opcode.InsertElement -> implement_this "insert element instr"
  | Opcode.ShuffleVector -> implement_this "shuffle vector instr"
  | Opcode.ExtractValue -> implement_this "extract value instr"
  | Opcode.InsertValue -> implement_this "instert value instr"
  | Opcode.Fence -> implement_this "fence instr"
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
  let body_nodes = fold_right_instrs (fun i cfgs -> cfg_node_of_instr i::cfgs) b [] in
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

let verify_module m =
  iter_globals env_add_gvar m;
  iter_functions verify_function m

let logic_of_named_struct (name,t) =
  let types = struct_element_types t in
  let subpointer p off_to_val offset t =
    let off = string_of_int offset in
    (mkPointer
       (Arg_op ("builtin_plus", [p; Arg_op("numeric_const", [Arg_string off])]))
       (Arg_op ("pointer_type", [args_of_type t]))
       (off_to_val off)) in
  let any_var x = Arg_var (Vars.AnyVar (0, x)) in
  let p = any_var "p" in
  let vp = any_var "vp" in
  let x = any_var "x" in
  let n = any_var "n" in
  let j = any_var "j" in
  let t = any_var "t" in
  let v = any_var "v" in
  let target_pointer = mkPointer x t v in
  let eltptr_concl = mkPPred ("eltptr", [x; p; Arg_op ("jump", [n; j])]) in
  let eltptr_prem = mkPPred ("eltptr", [x; Arg_op ("builtin_plus", [p;n]); j]) in

  let subpointers =
    Array.mapi
      (subpointer p (fun offset -> Arg_op ("field", [Arg_op ("numeric_const", [Arg_string offset]); vp])))
      types in
  let unfolded_form = Array.fold_right (fun p f -> mkStar p f) subpointers mkEmpty in
  let conclusion_lhs =
    pconjunction
      (mkPointer p (Arg_op ("named_type", [Arg_string name])) vp)
      eltptr_concl in
  let conclusion_rhs = target_pointer in
  let conclusion = (mkEmpty, conclusion_lhs, conclusion_rhs, mkEmpty) in
  let premise_lhs = pconjunction unfolded_form eltptr_prem in
  let premise_rhs = target_pointer in
  let premise = (mkEmpty, premise_lhs, premise_rhs, mkEmpty) in
  let without = mkEQ (p,x) in
  let geteltptr_rule = (conclusion, [[premise]], "geteltptr_"^name, (without, []), []) in


  let subpointers =
    Array.mapi
      (subpointer p (fun off -> any_var ("v"^off)))
      types in
  let unfolded_form = Array.fold_right (fun p f -> mkStar p f) subpointers mkEmpty in
  let named_pointer = mkPointer p (Arg_op ("named_type", [Arg_string name])) vp in
  let conclusion = (mkEmpty, unfolded_form, named_pointer, mkEmpty) in
  let premise = (unfolded_form, mkEmpty, mkEmpty, mkEmpty) in
  let fold_rule = (conclusion, [[premise]], "fold_"^name, ([], []), []) in

  let subpointers =
    Array.mapi
      (subpointer p (fun _ -> vp))
      types in
  let unfolded_form = Array.fold_right (fun p f -> mkStar p f) subpointers mkEmpty in
  let rec geteltptr_defer_rules offset = function
    | [] -> []
    | p::tl ->
      let conclusion = (mkEmpty, pconjunction [p] eltptr_concl, target_pointer, mkEmpty) in
      let premise = (mkEmpty, pconjunction [p] eltptr_prem, target_pointer, mkEmpty) in
      (conclusion, [[premise]],
       "geteltptr_defer_"^name^(string_of_int offset),
       (without, []), [])::(geteltptr_defer_rules (offset+1) tl) in

  geteltptr_rule::fold_rule::(geteltptr_defer_rules 0 unfolded_form)


let rec collect_named_structs_of_type lns t = match (classify_type t) with
  | Struct -> (
    match struct_name t with
      | None -> lns
      | Some n ->
	if List.mem_assoc n lns then lns
	else
	  let lns = (n,t)::lns in
	  Array.fold_left collect_named_structs_of_type lns (struct_element_types t)
  )
  | Pointer
  | Array
  | Vector -> collect_named_structs_of_type lns (element_type t)
  | _ -> lns

let rec collect_named_structs_of_value lns v = match classify_value v with
  | NullValue | BasicBlock | InlineAsm | MDNode | MDString | BlockAddress -> lns

  | Argument -> collect_named_structs_of_type lns (type_of v)

  | ConstantAggregateZero | ConstantArray | ConstantExpr | ConstantFP 
  | ConstantInt | ConstantPointerNull | ConstantStruct | ConstantVector
  | Function | GlobalAlias | GlobalVariable | UndefValue | Instruction _ ->
    let num_op = num_operands v in
    let lns = collect_named_structs_of_type lns (type_of v) in
    let rec collect_from_op lns n =
      if n = num_op then lns
      else (
	let lns = collect_named_structs_of_value lns (operand v n) in
	collect_from_op lns (n+1)) in
    collect_from_op lns 0

let collect_named_structs_of_block lns b =
  (* insert label command from the block's label l, followed by the sequence
     of commands of the block *)
  fold_left_instrs collect_named_structs_of_value lns b

let collect_named_structs_of_function lns f =
  (* we might need to collect the types from the parameters too here *)
  fold_left_blocks collect_named_structs_of_block lns f

let logic_of_module m =
  (* we only care about the named structs that are used inside functions,
     so let's collect only those *)
  let ltf = fold_left_functions collect_named_structs_of_function [] m in
  List.flatten (List.map logic_of_named_struct ltf)

let go logic abs_rules spec_list m =
  print_endline "It is on!";
  env.target <- Llvm_target.TargetData.create (data_layout m);
  env.logic <- logic; env.abs_rules <- abs_rules; env.specs <- spec_list;
  print_endline ("Added specs for "^string_of_int (List.length spec_list)^" functions");
  print_string "Adding unfolding logic for named structs... ";
  let l = logic_of_module m in
  env_add_logic_seq_rules l;
  print_endline (string_of_int (List.length l)^" rules added");
  if false then List.iter (pp_sequent_rule Format.std_formatter) l;
  verify_module m;
  env.result
