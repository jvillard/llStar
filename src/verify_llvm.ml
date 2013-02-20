open Format
open Debug
open Llvm
open TypeKind
open ValueKind
open Cfg_core
open Psyntax
open Logic_spec
open Config

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
  mutable abduct_logic: Psyntax.logic; (** abduction logic *)
  mutable specs: funspec list; (** the hoare triples we assume for declared
				   functions and strive to prove for defined ones  *)
  mutable gvars: (string * args * args) list; (** global variable: id * type * value *)
  mutable idMap: string IdMap.t; (** maps LLVM nameless values to identifiers *)
  mutable verif_result: bool; (** result of the proof. Yes or No only... *)
}

(** global environment *)
let env = {
  context = global_context ();
  target = Llvm_target.TargetData.create ""; (* has to be filled with a legal value later *)
  logic = empty_logic;
  abs_rules = empty_logic;
  abduct_logic = empty_logic;
  specs = [];
  gvars = [];
  idMap = IdMap.empty;
  verif_result = true;
}

let warn s =
  print_endline ("WARNING: "^s)

let string_of_struct s =
  match struct_name s with
  | None -> string_of_lltype s
  | Some name -> name

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

let env_add_abduct_rules sr  =
  env.abduct_logic <- { env.abduct_logic with seq_rules = env.abduct_logic.seq_rules@sr }

let env_add_seq_rules sr  =
  env.logic <- { env.logic with seq_rules = env.logic.seq_rules@sr };
  env_add_abduct_rules sr

let env_add_rw_rules rwr  =
  env.logic <- { env.logic with rw_rules = env.logic.rw_rules@rwr };
  env.abduct_logic <- { env.abduct_logic with rw_rules = env.abduct_logic.rw_rules@rwr }

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
  | Label -> Arg_op("label_type", [])
  | Integer -> Arg_op("integer_type", [args_num (integer_bitwidth t)])
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
  | NullValue -> args_num_0
  | Argument -> Arg_var (Vars.concretep_str (value_id v))
  | BasicBlock -> failwith "Invalid bitcode? Unexpected BasickBlock value"
  | InlineAsm -> Arg_var (Vars.freshe ())
  | MDNode -> raise (MetaData v)
  | MDString -> raise (MetaData v)
  | BlockAddress -> Arg_op("block_addr", [args_of_value (operand v 0)])
  | ConstantAggregateZero -> args_num_0
  | ConstantArray -> args_of_composite_value "array" v
  | ConstantExpr -> args_of_const_expr v
  | ConstantFP -> Arg_var (Vars.freshe ())
  | ConstantInt -> args_of_int_const v
  | ConstantPointerNull -> args_num_0
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

(** compute the pure predicate corresponding to a getelementpointer instruction *)
let ppred_of_gep x t ptr lidx =
  let rec jump_chain_of_lidx = function
    | [] -> Arg_op ("jump_end", [])
    | idx::tl ->
      let args_idx = args_of_value idx in
      Arg_op ("jump", [args_idx; jump_chain_of_lidx tl]) in
  let jump_chain = jump_chain_of_lidx lidx in
  mkPPred ("eltptr", [x; args_of_type t; ptr; jump_chain])


(** the current function context
 * contains indirection information for basic blocks (see
 * cfg_node_of_instr, Br case), and basic alloca information
 * TODO: for now, allocas are global to the function.
*)
type fun_env = {
  mutable fun_alloca_pred: pform; (** current footprint of local variables *)
  mutable fun_blk_label: string;
  mutable fun_br_to_orig: string -> string;
  mutable fun_br_to_dest: string -> string -> string;
  mutable fun_br_blocks: (string * cfg_node list) list;
  mutable fun_phi_nodes: (string * (llbasicblock * (string list * llvalue list)) list) list;
  mutable fun_cur_blk_phi_nodes: (llbasicblock * (string list * llvalue list)) list;
}

let mk_empty_fun_env () =
  { fun_alloca_pred = mkEmpty;
    fun_blk_label = "";
    fun_br_to_orig = (fun br -> br);
    fun_br_to_dest = (fun br dest -> dest);
    fun_br_blocks = [];
    fun_phi_nodes = [];
    fun_cur_blk_phi_nodes = []; }

let mk_br_block fun_env br_label_orig br_assume =
  let lab_src = fun_env.fun_blk_label in
  let lab_br = lab_src^"_br_"^br_label_orig in
  let label_node = mk_node (Core.Label_stmt_core lab_br) in
  let branch = mk_node (Core.Goto_stmt_core [br_label_orig]) in
  let br_blocks = fun_env.fun_br_blocks in
  fun_env.fun_br_blocks <-
    (lab_br, [label_node; br_assume; branch])::br_blocks;
  let f = fun_env.fun_br_to_orig in
  fun_env.fun_br_to_orig <-
    (fun l ->
      if l = lab_br then lab_src
      else f l);
  lab_br

let args_sizeof t =
  let size64 = Llvm_target.store_size env.target t in
  Arg_op("numeric_const", [Arg_string(Int64.to_string size64)])

(** compile an llvm instruction into a CoreStar program *) 
let cfg_node_of_instr fun_env instr = match instr_opcode instr with
  (* Terminator Instructions *)
  | Opcode.Ret ->
    (* the return instruction consumes the footprint of local
       variables and sets the return value*)
    let (post,params) =
      if num_operands instr != 0 then
	let ret_val = operand instr 0 in
	let p0 = Arg_var(Vars.concretep_str ("@parameter"^(string_of_int 0)^":")) in
	mkEQ(ret_arg, p0), [args_of_value ret_val]
      else mkEmpty, [] in
    let spec = Spec.mk_spec fun_env.fun_alloca_pred post Spec.ClassMap.empty in
    [mk_node (Core.Assignment_core  ([], spec, params)); mk_node Core.End]
  | Opcode.Br ->
    (* this is how you get a block's label using the ocaml bindings... *)
    let label_of_bblock b =
      value_id (value_of_block b) in
    if num_operands instr = 1 then
      (* Unconditional branch instr *)
      let next_label = label_of_bblock (block_of_value (operand instr 0)) in
      [mk_node (Core.Goto_stmt_core [next_label])]
    else
      (* Conditional branch instr, num_operands instr = 3 *)
      (* Since CoreStar doesn't know about conditionals, only about
	 non-deterministic choice, we have to create new blocks and
	 place them between the current block and the branch targets
	 (using the fun_env environment to remember the
	 indirection). These blocks will assume "cond" and "not cond"
	 respectively, thus simulating a conditional branching. *)
      let then_label_orig = label_of_bblock (block_of_value (operand instr 2)) in
      let else_label_orig = label_of_bblock (block_of_value (operand instr 1)) in
      (* the boolean value whose truth we're branching upon *)
      let args_cond = args_of_value (operand instr 0) in
      let assume_then_spec = Spec.mk_spec mkEmpty
	(mkEQ (args_num_1, args_cond)) Spec.ClassMap.empty in
      let assume_then = mk_node (Core.Assignment_core ([],assume_then_spec,[])) in
      let assume_else_spec = Spec.mk_spec mkEmpty
	(mkEQ (args_num_0, args_cond)) Spec.ClassMap.empty in
      let assume_else = mk_node (Core.Assignment_core ([],assume_else_spec,[])) in
      let then_label = mk_br_block fun_env then_label_orig assume_then in
      let else_label = mk_br_block fun_env else_label_orig assume_else in
      [mk_node (Core.Goto_stmt_core [then_label;else_label])]
  | Opcode.Switch -> implement_this "switch instruction"
  | Opcode.IndirectBr -> implement_this "indirect branch"
  | Opcode.Invoke -> implement_this "Invoke block terminator"
  | Opcode.Invalid2 -> failwith "\"Invalid\" instruction"
  | Opcode.Unreachable ->
    (* if llvm thinks it's unreachable, let's tell hopstar by assuming False *)
    let spec = Spec.mk_spec [] mkFalse Spec.ClassMap.empty in
    [mk_node (Core.Assignment_core ([],spec,[]))]
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
    [mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))]
  | Opcode.Sub
  | Opcode.FSub ->
    let id = value_id instr in
    let v1 = args_of_value (operand instr 0) in
    let v2 = args_of_value (operand instr 1) in
    let pre = mkEmpty in
    let post = mkEQ (ret_arg, Arg_op("builtin_minus", [v1; v2])) in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    [mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))]
  | Opcode.Mul
  | Opcode.FMul ->
    let id = value_id instr in
    let v1 = args_of_value (operand instr 0) in
    let v2 = args_of_value (operand instr 1) in
    let pre = mkEmpty in
    let post = mkEQ (ret_arg, Arg_op("builtin_mult", [v1; v2])) in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    [mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))]
  | Opcode.UDiv
  | Opcode.SDiv
  | Opcode.FDiv ->
    let id = value_id instr in
    let v1 = args_of_value (operand instr 0) in
    let v2 = args_of_value (operand instr 1) in
    let pre = mkEmpty in
    let post = mkEQ (ret_arg, Arg_op("builtin_div", [v1; v2])) in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    [mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))]
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
    let ptr_t = type_of instr in
    let value_t = element_type ptr_t in
    let sz = args_sizeof value_t in
    let x = Arg_var (Vars.concretep_str id) in
    let e = Arg_var (Vars.freshe ()) in
    let alloca = mkSPred ("alloca", [x; sz]) in
    let pointer = mkPointer x sz e in
    let post = mkStar alloca pointer in
    fun_env.fun_alloca_pred <- mkStar fun_env.fun_alloca_pred post;
    let spec = Spec.mk_spec [] post Spec.ClassMap.empty in
    [mk_node (Core.Assignment_core ([], spec, []))]
  | Opcode.Load ->
    let id = value_id instr in
    (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l00225 *)
    let ptr_v = operand instr 0 in
    let ptr = args_of_value ptr_v in
    let value_t = type_of instr in
    let e = Arg_var (Vars.freshe ()) in
    let pointer = mkPointer ptr (args_sizeof value_t) e in
    let pre = pointer in
    let post = pconjunction (mkEQ(ret_arg, e)) pointer in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    [mk_node (Core.Assignment_core ([Vars.concretep_str id], spec, []))]
  | Opcode.Store ->
    (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l00343 *)
    let value = operand instr 0 in
    (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l00346 *)
    let ptr_v = operand instr 1 in
    let ptr = args_of_value ptr_v in
    let e = Arg_var (Vars.freshe ()) in
    let v = args_of_value value in
    let value_t = type_of value in
    let pointer_pre = mkPointer ptr (args_sizeof value_t) e in
    let pointer_post = mkPointer ptr (args_sizeof value_t) v in
    let pre = pointer_pre in
    let post = pointer_post in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    [mk_node (Core.Assignment_core ([], spec, []))]
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
    [mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))]
  (* Cast Operators *)
  | Opcode.BitCast ->
    (* we just assign the old value to the new variable, since we
       leave type checking in the hands of llvm *)
    let id = value_id instr in
    let value = operand instr 0 in
    let v = args_of_value value in
    let pre = mkEmpty in
    let post = mkEQ(ret_arg, v) in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    [mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))]
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
    (* TODO: most of these can change the actual value *)
    let id = value_id instr in
    let value = operand instr 0 in
    let v = args_of_value value in
    let pre = mkEmpty in
    let post = mkEQ(ret_arg, v) in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    [mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))]
  (* Other Operators *)
  | Opcode.ICmp ->
    let id = value_id instr in
    let op = match icmp_predicate instr with
      | None -> assert false
      | Some p -> match p with
	  | Icmp.Eq -> "builtin_eq"
	  | Icmp.Ne -> "builtin_neq"
	  | Icmp.Ugt
	  | Icmp.Sgt -> "builtin_gt"
	  | Icmp.Uge
	  | Icmp.Sge -> "builtin_ge"
	  | Icmp.Ult
	  | Icmp.Slt -> "builtin_lt"
	  | Icmp.Ule
	  | Icmp.Sle -> "builtin_le" in
    let v1 = args_of_value (operand instr 0) in
    let v2 = args_of_value (operand instr 1) in
    let pre = mkEmpty in
    let post = mkEQ (ret_arg, Arg_op(op, [v1; v2])) in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    [mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))]
  | Opcode.FCmp -> implement_this "fcmp instr"
  | Opcode.PHI ->
    (* We cannot treat phi nodes directly. Instead, we remember the
       phi nodes of the current block to execute them all the
       assignments that correspond to the same predecessor block at
       once (as they should according to their semantics) in separate
       blocks. We also record the extra indirection between
       predecessors of the current block and this block. Once all the
       blocks have been translated, we will update the inter-blocks
       links to go through the phi nodes. See the
       "update_cfg_with_new_phi_blocks" function. *)
    let x = value_id instr in
    let rec dispatch groups = function
      | [] -> groups
      | (v,b)::tl ->
	try
	  let (lx,lv) = List.assoc b groups in
	  let groups = (b,(x::lx,v::lv))::(List.remove_assoc b groups) in
	  dispatch groups tl
	with Not_found ->
	  dispatch ((b,([x],[v]))::groups) tl in
    let cur_phi_nodes = fun_env.fun_cur_blk_phi_nodes in
    fun_env.fun_cur_blk_phi_nodes <- dispatch cur_phi_nodes (incoming instr);
    [mk_node Core.Nop_stmt_core]
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
      [mk_node (Core.Assignment_core ([Vars.concretep_str id],
				      call_spec,
				      params))]
    with MetaData _ ->
      (* a function call with meta-data in its arguments is for debug info *)
      (* and may be safely ignored *)
      [mk_node Core.Nop_stmt_core])
  | Opcode.Select -> implement_this "select instr"
  | Opcode.UserOp1
  | Opcode.UserOp2 ->
    warn "Skipping user-defined instruction";
    [mk_node Core.Nop_stmt_core]
  | Opcode.VAArg ->
    warn "Skipping VAArg instruction";
    [mk_node Core.Nop_stmt_core]
  | Opcode.ExtractElement
  | Opcode.InsertElement
  | Opcode.ShuffleVector
  | Opcode.ExtractValue
  | Opcode.InsertValue ->
    warn "Skipping vector instruction";
    [mk_node Core.Nop_stmt_core]
  | Opcode.Fence ->
    (* this skip is safe since we assume a sequential semantics *)
    [mk_node Core.Nop_stmt_core]
  | Opcode.AtomicCmpXchg -> implement_this "atomic cmp xchange instr"
  | Opcode.AtomicRMW -> implement_this "atomic RMW instr"
  | Opcode.Resume -> implement_this "resume instr"
  | Opcode.LandingPad -> [mk_node Core.Nop_stmt_core]
  | Opcode.Unwind -> implement_this "unwind"

let rec update_cfg_with_new_phi_blocks bfwd bbwd lsrc = function
  | [] -> []
  | x::[] ->
    let y = match x.skind with
      | Core.Goto_stmt_core [l] ->
	mk_node (Core.Goto_stmt_core [bfwd (bbwd lsrc) l])
      | _ -> x in
    y::[]
  | x::y::tl -> x::(update_cfg_with_new_phi_blocks bfwd bbwd lsrc (y::tl))

let make_phi_blocks fun_env =
  let blocks_of_blk_phi_nodes (lab_target,b) =
    let block_of_group (b,(lx,lv)) =
      let lab_src = value_id (value_of_block b) in
      let lab = lab_src^"_to_"^lab_target in
      let label_node = mk_node (Core.Label_stmt_core lab) in
      let equalities =
	fst (List.fold_left
	       (fun (f,i) _ ->
		 let reti = Vars.concretep_str ("$ret_v"^(string_of_int (i+1))) in
		 let parami = Vars.concretep_str (Symexec.parameter i) in
		 (pconjunction f (mkEQ (Arg_var(reti), Arg_var(parami))), i+1))
	       (mkEmpty,0) lx) in
      let spec = Spec.mk_spec mkEmpty equalities Spec.ClassMap.empty in
      let xs = List.map Vars.concretep_str lx in
      let vs = List.map args_of_value lv in
      let assign = mk_node (Core.Assignment_core (xs,spec,vs)) in
      let branch = mk_node (Core.Goto_stmt_core [lab_target]) in
      (lab_src, (lab,[label_node; assign; branch])) in
    let blocks = List.map block_of_group b in
    let f = fun_env.fun_br_to_dest in
    fun_env.fun_br_to_dest <- (fun lsrc ldest ->
      if (ldest = lab_target) && (List.mem_assoc lsrc blocks) then
	fst (List.assoc lsrc blocks)
      else f lsrc ldest);
    List.map snd blocks in
  List.flatten (List.map blocks_of_blk_phi_nodes fun_env.fun_phi_nodes)

let cfg_nodes_of_block fun_env lnsl b =
  (* insert label command from the block's label l, followed by the sequence
     of commands of the block *)
  let l = value_id (value_of_block b) in
  fun_env.fun_blk_label <- l;
  fun_env.fun_cur_blk_phi_nodes <- [];
  let label_node = mk_node (Core.Label_stmt_core l) in
  let body_nodes = fold_left_instrs
    (fun cfgs i -> cfgs@(cfg_node_of_instr fun_env i)) [] b in
  if fun_env.fun_cur_blk_phi_nodes <> [] then (
    let phi_nodes = fun_env.fun_phi_nodes in
    fun_env.fun_phi_nodes <- (l, fun_env.fun_cur_blk_phi_nodes)::phi_nodes;);
  lnsl@[(l,label_node::body_nodes)]

let cfg_nodes_of_function f =
  let fun_env = mk_empty_fun_env () in
  (* it would be more efficient to fold_right here, but then the
     identifiers we generate would be in reversed order, which is
     confusing in output messages *)
  let lab_nodes_list =
    fold_left_blocks (cfg_nodes_of_block fun_env) [] f in
  (* print_endline ("*** Found "^(string_of_int (List.length fun_env.fun_br_blocks))^" conditional branchings and "^(string_of_int (List.length fun_env.fun_phi_nodes))^" blocks with phi nodes"); *)
  let lnsl = lab_nodes_list@fun_env.fun_br_blocks in
  let phi_cfg = make_phi_blocks fun_env in
  let lnsl = List.map
    (fun (l,cfg) ->
      (l, update_cfg_with_new_phi_blocks
	fun_env.fun_br_to_dest fun_env.fun_br_to_orig l cfg)) lnsl in
  let lnsl = lnsl@phi_cfg in
  List.flatten (List.map snd lnsl)

let pp_spec fmt (pre,post) =
  Format.fprintf fmt "@[{%a}\n{%a}\n@."
    Sepprover.string_inner_form pre Sepprover.string_inner_form post

let dump_specs_of_function fid specs =
  let file = Filename.concat !Lstar_config.outdir (!Lstar_config.program_base_name ^ "." ^ fid ^ ".specs") in
  let specs_out = open_out file in
  let specs_fmt = Format.formatter_of_out_channel specs_out in
  Format.fprintf specs_fmt "%a" (Debug.pp_list pp_spec) specs;
  close_out specs_out

let verify_function f =
  let id = value_id f in
  if not (is_declaration f) then (
    if !Lstar_config.abduction_flag then
      Format.fprintf logf "@[Abducing spec of function %s...@]%!@\n" id
    else
      Format.fprintf logf "@[Verifying function %s...@]%!@\n" id;
    let cfg_nodes = cfg_nodes_of_function f in
    let spec = spec_of_fun_id id in
    (* replace "@parameter%i%:" logical values with the function arguments *)
    let rec add_param_subst (i,subst) fun_param =
      let arg = args_of_value fun_param in
      let param = Vars.concretep_str ("@parameter"^(string_of_int i)^":") in
      (i+1, add_subst param arg subst) in
    let (_,subst) = fold_left_params add_param_subst (0,empty) f in
    let spec_to_verify =
      { spec with
	Spec.pre = subst_form subst spec.Spec.pre;
	Spec.post = subst_form subst spec.Spec.post; } in
    stmts_to_cfg cfg_nodes;
    print_icfg_dotty [(cfg_nodes, id)] id;
    let success =
      if !Lstar_config.abduction_flag then
	let specs = Symexec.bi_abduct id cfg_nodes spec_to_verify
	  env.logic env.abduct_logic env.abs_rules in
	dump_specs_of_function id specs;
	specs <> []
      else
	Symexec.verify id cfg_nodes spec_to_verify env.logic env.abs_rules in
    env.verif_result <- env.verif_result && success
  )

let env_add_gvar gvar =
  env.gvars <- (value_id gvar, args_of_type (type_of gvar), args_of_value gvar)::env.gvars

let verify_module m =
  iter_globals env_add_gvar m;
  iter_functions verify_function m

let mk_simple_seq_rule name (pre_lhs,pre_rhs) (post_lhs, post_rhs) =
  let conclusion = (mkEmpty, post_lhs, post_rhs, mkEmpty) in
  let premise = (mkEmpty, pre_lhs, pre_rhs, mkEmpty) in
  (conclusion, [[premise]], name, ([], []), [])

let gen_seq_rules_of_equiv name (equiv_left, equiv_right) =
  mk_simple_seq_rule (name^"_left") (equiv_right, mkEmpty) (equiv_left, mkEmpty)::
  mk_simple_seq_rule (name^"_right") (mkEmpty, equiv_right) (mkEmpty, equiv_left)::[]

(** dumps logic rules into a file in the current directory *)
let dump_logic_rules name rs =
  let file = Filename.concat !Lstar_config.outdir (!Lstar_config.program_base_name ^ "." ^ name) in
  let rules_out = open_out file in
  let rules_fmt = Format.formatter_of_out_channel rules_out in
  Format.fprintf rules_fmt "@[%a@." (Debug.pp_list pp_sequent_rule) rs;
  close_out rules_out

let add_sizeof_logic_of_type t =
  let args_t = args_of_type t in
  let args_size = args_sizeof t in
  let rewrite_rule =
    { function_name = "sizeof";
      arguments=[args_t];
      result=args_size;
      guard={without_form=[];rewrite_where=[];if_form=[]};
      rewrite_name="sizeof_"^(string_of_lltype t);
      saturate=false} in
  env_add_rw_rules [rewrite_rule]
  

(** the physical offset inside the struct, as a logical expression *)
let offset_of_field struct_t i =
  let offset = Llvm_target.offset_of_element env.target struct_t i in
  Arg_op("numeric_const", [Arg_string(Int64.to_string offset)])

let offset_of_field_end struct_t i =
  let offset = Llvm_target.offset_of_element env.target struct_t i in
  let field_type = Array.get (struct_element_types struct_t) i in
  let field_size = Llvm_target.store_size env.target field_type in
  let field_end = Int64.add offset field_size in
  Arg_op("numeric_const", [Arg_string(Int64.to_string field_end)])

(* a few definitions to make the rule definitions more readable *)
let args_sizeof_field struct_t i =
  args_sizeof (Array.get (struct_element_types struct_t) i)

let mk_padding_of_field struct_t i root =
  let offset = Llvm_target.offset_of_element env.target struct_t i in
  let next_offset =
    if i = Array.length (struct_element_types struct_t) - 1 then
      Llvm_target.store_size env.target struct_t
    else Llvm_target.offset_of_element env.target struct_t (i+1) in
  let elt_size = Llvm_target.store_size env.target
    (Array.get (struct_element_types struct_t) i) in
  let pad_size = Int64.sub next_offset (Int64.add offset elt_size) in
  if pad_size = Int64.zero then None
  else
    let offset = Arg_string (Int64.to_string (Int64.add offset elt_size)) in
    let pad_addr = Arg_op("builtin_plus", [root; offset]) in
    Some (mkSPred ("padding", [pad_addr; Arg_string(Int64.to_string pad_size)]))

let mk_field_pointer struct_t i root value =
  if i = 0 then
    mkPointer root (args_sizeof_field struct_t i) value
  else
    let offset = Arg_op ("builtin_plus", [root; offset_of_field struct_t i]) in
    mkPointer offset (args_sizeof_field struct_t i) value

let mk_padded_field_pointer struct_t i root value =
  let field_pointer = mk_field_pointer struct_t i root value in
  match mk_padding_of_field struct_t i root with
    | None -> field_pointer
    | Some pad -> mkStar field_pointer pad

let mk_unfolded_struct struct_t root fields_values =
  let mk_field i subelt_t =
    let v = Array.get fields_values i in
    mk_padded_field_pointer struct_t i root v in
  let pointers = Array.mapi mk_field (struct_element_types struct_t) in
  Array.fold_left (fun f p -> mkStar f p) mkEmpty pointers

let add_eltptr_logic_of_struct t = match struct_name t with
  | None -> (* TODO: handle unnamed structs *) ()
  | Some name ->
    let args_struct_t = args_of_type t in
    (** the physical offset inside the struct, as a logical expression *)
    let offset_of_field i =
      let offset = Llvm_target.offset_of_element env.target t i in
      Arg_op("numeric_const", [Arg_string(Int64.to_string offset)]) in
    let x_var = Arg_var (Vars.AnyVar (0, "x")) in
    let root_var = Arg_var (Vars.AnyVar (0, "r")) in
    let jump_var = Arg_var (Vars.AnyVar (0, "j")) in
    let subelt_eltptr_rules i subelt_type =
      let jump = Arg_op ("jump", [args_num i; jump_var]) in
      let equiv_left =
	mkPPred ("eltptr", [x_var; args_struct_t;
			    root_var; jump]) in
      let new_root =
	if i = 0 then root_var
	else Arg_op ("builtin_plus", [root_var; offset_of_field i]) in
      let equiv_right =
	mkPPred ("eltptr", [x_var; args_of_type subelt_type;
			    new_root; jump_var]) in
      gen_seq_rules_of_equiv ("eltptr_"^(string_of_struct t))
	(equiv_left, equiv_right) in
    let eltptr_rules =
      List.flatten 
	(Array.to_list
	   (Array.mapi subelt_eltptr_rules (struct_element_types t))) in
    env_add_seq_rules eltptr_rules

let add_logic_of_struct t =
  add_eltptr_logic_of_struct t;
  let field_values =
    Array.mapi (fun i _ -> Arg_var (Vars.AnyVar (0, "v"^(string_of_int i))))
      (struct_element_types t) in
  
  let collate_field_values =
    let rec aux = function
      | [] -> assert false (* TODO: handle empty structs (sigh) *)
      | v::[] -> v
      | v1::v2::tl -> Arg_op ("collate", [v1; aux (v2::tl)]) in
    aux (Array.to_list field_values) in

  let field_ranged_values base_val =
    let range i =
      let b = offset_of_field t i in
      let e = offset_of_field_end t i in
      Arg_op ("rg", [b; e; base_val]) in
    Array.mapi (fun i _ -> range i) (struct_element_types t) in

  let x_var = Arg_var (Vars.AnyVar (0, "x")) in
  let v_var = Arg_var (Vars.AnyVar (0, "v")) in
  let w_var = Arg_var (Vars.AnyVar (0, "w")) in

  let mk_struct_pointer root value =
    mkPointer root (args_sizeof t) value in

  let mk_field_rule i subelt_type =
    let b = offset_of_field t i in
    let e = offset_of_field_end t i in
    let field_value = Arg_op ("rg", [b; e; v_var]) in
    let target_pointer = mk_field_pointer t i x_var w_var in
    mk_simple_seq_rule ((string_of_struct t)^"_field_"^(string_of_int i))
      (mk_unfolded_struct t x_var (field_ranged_values v_var),
       pconjunction (mkEQ (w_var,field_value)) target_pointer)
      (mk_struct_pointer x_var v_var, target_pointer) in

  let field_rules =
    let rules_array = Array.mapi mk_field_rule (struct_element_types t) in
    Array.to_list rules_array in

  let rules =
    if Array.length (struct_element_types t) > 1 then
      mk_simple_seq_rule ("collate_"^(string_of_struct t))
	(mk_struct_pointer x_var collate_field_values, mk_struct_pointer x_var v_var)
	(mk_unfolded_struct t x_var field_values, mk_struct_pointer x_var v_var)::
	field_rules
    else [] in
  env_add_seq_rules rules

let add_list_logic_of_struct t name rec_field =
  let fresh_field_values =
    Array.map (fun _ -> Arg_var (Vars.freshe ())) (struct_element_types t) in

  let mk_rec_field root value = mk_field_pointer t rec_field root value in
  let mk_lseg b e = mkSPred ("lseg", [name;b;e]) in
  let mk_lseg_ne b e = mkSPred ("lseg_ne", [name;b;e]) in
  let mk_node ptr value = mkSPred ("node", [name;ptr; value]) in

  let i_var = Arg_var (Vars.AnyVar (0, "i")) in
  let j_var = Arg_var (Vars.AnyVar (0, "j")) in
  let n_var = Arg_var (Vars.AnyVar (0, "n")) in
  let n2_var = Arg_var (Vars.AnyVar (0, "n2")) in

  let mk_unfolded_struct_ root i value =
    let old_field_val = Array.get fresh_field_values i in
    Array.set fresh_field_values i value;
    let us = mk_unfolded_struct t root fresh_field_values in
    Array.set fresh_field_values i old_field_val;
    us in

  (** Expand node.

      If we have a node predicate on the LHS of an implication and one of the
      fields of the node object appears on the RHS, then we rewrite the node in terms
      of its constituent fields. *)
  let mk_expand_node_rule i subelt_type =
    if i = rec_field then
      (mk_simple_seq_rule "node_lookup_next"
	 (mk_unfolded_struct_ i_var rec_field n_var, mk_rec_field i_var n2_var)
	 (mk_node i_var n_var, mk_rec_field i_var n2_var))
    else
      (mk_simple_seq_rule ("node_lookup_field_"^(string_of_int i))
	 (mk_unfolded_struct_ i_var rec_field n_var, mk_field_pointer t i i_var n2_var)
	 (mk_node i_var n_var, mk_field_pointer t i i_var n2_var)) in

  let expand_node_rules =
    let rules_array = Array.mapi mk_expand_node_rule (struct_element_types t) in
    Array.to_list rules_array in

  (** the list of rules that need to know about the structure of the nodes
      * the ones that don't are included in the distribution instead of generated
      * see logic/lseg.logic *)
  let rules =
    (* Equality of list segment nodes.

       If the RHS contains a node of a LHS list segment, we expand the node on the
       RHS into its constituent fields. Afterwards the rules lseg_cons_field_lookup
       can be applied. *)
    mk_simple_seq_rule "lseg_node_lookup_first"
      (mk_lseg_ne i_var j_var, mk_unfolded_struct_ i_var rec_field n_var)
      (mk_lseg_ne i_var j_var, mk_node i_var n_var)::
    (* If we have a field on the RHS of the first node in a non_empty list segment
       on the LHS, we expand the list segment on the LHS. *)
      (let v = Arg_var (Vars.freshe ()) in
       mk_simple_seq_rule "lseg_cons_field_lookup"
	 (mkStar (mk_lseg v j_var) (mk_node i_var v), mk_rec_field i_var n_var)
	 (mk_lseg_ne i_var j_var, mk_rec_field i_var n_var))::
      expand_node_rules@
    (* Collapse to node.

       If all fields of a node object are present on the lhs, we collapse them to
       the node predicate. *)
      (mk_simple_seq_rule "node_rollup_left"
	 (mk_node i_var n_var, mkEmpty)
	 (mk_unfolded_struct_ i_var rec_field n_var, mkEmpty))::
    (* Convert all nodes on LHS to singleton list segments. *)
      (* (mk_simple_seq_rule "lseg_node_rollup_left" *)
      (* 	 (mk_lseg_ne i_var n_var, mkEmpty) *)
      (* 	 (mk_node i_var n_var, mkEmpty)):: *)
      (mk_simple_seq_rule "node_expand_right"
	 (mk_node i_var n_var, mk_unfolded_struct_ i_var rec_field n2_var)
	 (mk_node i_var n_var, mk_node i_var n2_var))::
      (mk_simple_seq_rule "node_rollup_right"
	 (mkEmpty, mk_node i_var n_var)
	 (mkEmpty, mk_unfolded_struct_ i_var rec_field n_var))::[] in
  env_add_seq_rules rules;

  (* rules for abduction of list nodes *)

  (** abduce an entire node whenever we need to abduce any of the fields *)
  let mk_abduce_node_rule i subelt_type =
    if i = rec_field then
      let conclusion = (mkEmpty, mkEmpty, mk_rec_field i_var n_var, mkEmpty) in
      let premise = (mkEmpty,
		     mk_unfolded_struct_ i_var rec_field n_var,
		     mk_rec_field i_var n_var,
		     mk_node i_var n_var) in
      let without = mkPointer i_var (Arg_var (Vars.AnyVar (0, "s"))) (Arg_var (Vars.AnyVar (0, "v"))) in
      (conclusion, [[premise]], "abduce_node_next", (without, []), [])
    else
      let conclusion = (mkEmpty, mkEmpty, mk_field_pointer t i i_var n_var, mkEmpty) in
      let premise = (mkEmpty,
		     mk_unfolded_struct_ i_var i n_var,
		     mk_field_pointer t i i_var n_var,
		     mk_node i_var n2_var) in
      let without = mkPointer i_var (Arg_var (Vars.AnyVar (0, "s"))) (Arg_var (Vars.AnyVar (0, "v"))) in
      (conclusion, [[premise]], "abduce_node_field_"^(string_of_int i), (without, []), []) in

  let abduce_node_rules =
    let rules_array = Array.mapi mk_abduce_node_rule (struct_element_types t) in
    Array.to_list rules_array in

  let abduct_rules = abduce_node_rules in
  env_add_abduct_rules abduct_rules

let add_list_logic_of_type t = match struct_name t with
  | None -> (* recursive structs are necessarily named *) ()
  | Some name ->
    (* let's find the first recursive field, which we assume is the
       forward pointer of the linked list *)
    let numed_types = Array.mapi (fun i tt -> (i,tt)) (struct_element_types t) in
    let subelt_types = Array.to_list numed_types in
    try
      let points_to_struct typ = pointer_type t = typ in
      let (rec_field,_) =
	List.find (fun (i,subelt_t) -> points_to_struct subelt_t) subelt_types in
      add_list_logic_of_struct t (Arg_string name) rec_field
    with Not_found -> ()

(************** Collect all the types in a module *)

let rec collect_type (seen_t, seen_const) t =
  if (LltypeSet.mem t seen_t) then (seen_t, seen_const)
  else
    let seen_t = LltypeSet.add t seen_t in
    match (classify_type t) with
      | Struct ->
	Array.fold_left collect_type (seen_t, seen_const) (struct_element_types t)
      | Pointer
      | Array
      | Vector -> collect_type (seen_t, seen_const) (element_type t)
      | _ -> (seen_t, seen_const)

(** looks for new types in the value v *)
let rec collect_types_in_value o v =
  let (seen_t, seen_const) = collect_type o (type_of v) in
  (*  if v is not a constant, then its structs have already been collected explicitly *)
  if (not (is_constant v)) || (LlvalueSet.mem v seen_const) then (seen_t, seen_const)
  else
    let seen_const = LlvalueSet.add v seen_const in
    let o = (seen_t, seen_const) in
    let num_op = num_operands v in
    let rec collect_from_op o n =
      if n = num_op then o
      else (
	let o = collect_types_in_value o (operand v n) in
	collect_from_op o (n+1)) in
    collect_from_op o 0

(** looks for new types in the instruction i *)
let collect_types_in_instr o i =
  let num_op = num_operands i in
  let o = collect_type o (type_of i) in
  let rec collect_from_op o n =
    if n = num_op then o
    else (
      let o = collect_types_in_value o (operand i n) in
      collect_from_op o (n+1)) in
  collect_from_op o 0

let collect_types_in_block o b =
  fold_left_instrs collect_types_in_instr o b

let collect_types_in_function o f =
  let o = collect_type o (type_of f) in
  fold_left_blocks collect_types_in_block o f

(** collects all the types referred to by the functions of module m *)
let collect_types_in_module m =
  (* we only care about the types that are used inside functions,
     so let's collect only those *)
  let o = (LltypeSet.empty,LlvalueSet.empty) in
  let (typs, _) = fold_left_functions collect_types_in_function o m in
  LltypeSet.elements typs

(************** /Collect all the types in a module *)

let filter_int_and_struct t = match classify_type t with
  | Integer
  | Struct -> true
  | _ -> false

let filter_struct t = match classify_type t with
  | Struct -> true
  | _ -> false

let add_logic_of_module m =
  let typs = collect_types_in_module m in
  let typs = List.filter filter_int_and_struct typs in
  List.iter add_sizeof_logic_of_type typs;
  let typs = List.filter filter_struct typs in
  List.iter add_logic_of_struct typs;
  if !Lstar_config.auto_gen_list_logic then (
    print_endline "Generating list logic for the module...";
    List.iter add_list_logic_of_type typs
  )

let go logic abduct_logic abs_rules spec_list m =
  env.context <- module_context m;
  env.target <- Llvm_target.TargetData.create (data_layout m);
  env.logic <- logic;
  env.abduct_logic <- abduct_logic;
  env.abs_rules <- abs_rules;
  env.specs <- spec_list;
  if log log_phase then
    Format.fprintf logf "Generating logic for the module...@\n";
  add_logic_of_module m;
  dump_logic_rules "logic_rules.txt" (env.logic.seq_rules);
  if !Lstar_config.abduction_flag then (
    dump_logic_rules "abduct_rules.txt" (env.abduct_logic.seq_rules)
  );
  verify_module m;
  env.verif_result
