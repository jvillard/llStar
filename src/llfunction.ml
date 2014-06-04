(*** Llfunction: translate a bitcode module into a coreStar CFG *)

(* LLVM modules *)
open Llvm
(* coreStar modules *)
open Corestar_std
module C = Core
(* llStar modules *)
open Llexpression
open Llutils

let z3_ctx = Syntax.z3_ctx

(** context when translating a function
 * contains indirection information for basic blocks (see
 * statements_of_instr, Br case), and basic alloca information
 * TODO: for now, allocas are global to the function.
*)
type fun_env = {
  mutable fun_alloca_pred: Z3.Expr.expr; (** current footprint of local variables *)
  mutable fun_blk_label: string;
  mutable fun_br_to_orig: string -> string;
  mutable fun_br_to_dest: string -> string -> string;
  mutable fun_br_blocks: (string * C.statement list) list;
  mutable fun_phi_nodes: (string * (llbasicblock * (Z3.Expr.expr * Z3.Expr.expr) list) list) list;
  mutable fun_cur_blk_phi_nodes: (llbasicblock * (Z3.Expr.expr * Z3.Expr.expr) list) list;
}

let mk_empty_fun_env () =
  { fun_alloca_pred = Syntax.mk_emp;
    fun_blk_label = "";
    fun_br_to_orig = (fun br -> br);
    fun_br_to_dest = (fun br dest -> dest);
    fun_br_blocks = [];
    fun_phi_nodes = [];
    fun_cur_blk_phi_nodes = []; }

let mk_br_block fun_env br_label_orig br_assume =
  let lab_src = fun_env.fun_blk_label in
  let lab_br = lab_src^"_br_"^br_label_orig in
  let label_stmt = C.Label_stmt_core lab_br in
  let branch = C.Goto_stmt_core [br_label_orig] in
  let br_blocks = fun_env.fun_br_blocks in
  fun_env.fun_br_blocks <-
    (lab_br, [label_stmt; br_assume; branch])::br_blocks;
  let f = fun_env.fun_br_to_orig in
  fun_env.fun_br_to_orig <-
    (fun l ->
      if l = lab_br then lab_src
      else f l);
  lab_br


let mk_simple_asgn pre post =
  let triple =
    { C.pre = pre; post = post; modifies = [] } in
  let spec = C.TripleSet.singleton triple in
  let asgn = { C.asgn_rets = []; asgn_args = [];
	       asgn_rets_formal = []; asgn_args_formal = [];
	       asgn_spec = spec } in
  C.Assignment_core asgn

(** compile an llvm instruction within a function into a piece of CoreStar CFG *) 
let statements_of_instr retv fun_env instr =
  (* TODO: restore *)
  (* let mk_node stmt = *)
  (*   let loc = location_of_instr instr in *)
  (*   let node = Cfg_core.mk_node stmt in *)
  (*   Printing.add_location node.sid loc; *)
  (*   node in *)
  match instr_opcode instr with
  (* Terminator Instructions *)
  | Opcode.Ret ->
    (* the return instruction consumes the footprint of local
       variables and sets the return value*)
    let post =
      if num_operands instr != 0 then
	let retv = from_some retv in
	let ret_val = expr_of_llvalue (operand instr 0) in
	Z3.Boolean.mk_eq z3_ctx retv ret_val
      else Syntax.mk_emp in
    let out = option [] (fun x -> [x]) retv in
    let triple =
      { C.pre = fun_env.fun_alloca_pred; post = post; modifies = [] } in
    let spec = C.TripleSet.singleton triple in
    let ret_list = option [] (fun x -> [x]) retv in
    let asgn = { C.asgn_rets = out; asgn_rets_formal = ret_list;
		 asgn_args = []; asgn_args_formal = [];
		 asgn_spec = spec } in
    [C.Assignment_core asgn; C.End]
  | Opcode.Br ->
    if num_operands instr = 1 then
      (* Unconditional branch instr *)
      let next_label = label_of_bblock (block_of_value (operand instr 0)) in
      [C.Goto_stmt_core [next_label]]
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
      let expr_cond = expr_of_llvalue (operand instr 0) in
      let then_post = Z3.Boolean.mk_eq z3_ctx (mk_bv 1 "1") expr_cond in
      let assume_then = mk_simple_asgn Syntax.mk_emp then_post in
      let else_post = Z3.Boolean.mk_eq z3_ctx (mk_bv 1 "0") expr_cond in
      let assume_else = mk_simple_asgn Syntax.mk_emp else_post in
      let then_label = mk_br_block fun_env then_label_orig assume_then in
      let else_label = mk_br_block fun_env else_label_orig assume_else in
      [C.Goto_stmt_core [then_label;else_label]]
  | Opcode.Switch ->
    let expr_val = expr_of_llvalue (operand instr 0) in
    let mk_case v l =
      let label_orig = label_of_bblock (block_of_value l) in
      let expr_cond = expr_of_llvalue v in
      let post = Z3.Boolean.mk_eq z3_ctx expr_val expr_cond in
      let assume_case = mk_simple_asgn Syntax.mk_emp post in
      mk_br_block fun_env label_orig assume_case in
    let default_label =
      let default_orig = label_of_bblock (switch_default_dest instr) in
      let cond = ref(Syntax.mk_emp) in
      for i = 1 to (num_operands instr)/2 -1 do
	let case_val = expr_of_llvalue (operand instr (i*2)) in
	cond := Syntax.mk_star !cond
	  (Z3.Boolean.mk_distinct z3_ctx [expr_val; case_val])
      done;
      let assume = mk_simple_asgn Syntax.mk_emp !cond in
      mk_br_block fun_env default_orig assume in
    let cases_labels = ref [] in
    for i = 1 to (num_operands instr)/2 -1 do
      cases_labels := mk_case (operand instr (i*2)) (operand instr (i*2 + 1))::
	!cases_labels
    done;
    [C.Goto_stmt_core (default_label::!cases_labels)]
  | Opcode.IndirectBr -> implement_this "indirect branch"
  | Opcode.Invoke -> implement_this "Invoke block terminator"
  | Opcode.Unreachable ->
    (* assert False *)
    [mk_simple_asgn Syntax.mk_false Syntax.mk_false]
  | Opcode.Invalid
  | Opcode.Invalid2 -> failwith "\"Invalid\" instruction"
  (* Memory Operators *)
  | Opcode.Alloca ->
    let ptr_t = type_of instr in
    let ptr_s = sort_of_lltype ptr_t in
    let id = Syntax.mk_plvar ptr_s (value_id instr) in
    let value_t = element_type ptr_t in
    let e = Syntax.mk_fresh_lvar llmem_sort "v" in
    let heap = mk_pointer id (expr_of_lltype value_t) e in
    fun_env.fun_alloca_pred <- Syntax.mk_star fun_env.fun_alloca_pred heap;
    let triple = { C.pre = Syntax.mk_emp; post = heap; modifies = [] } in
    let spec = C.TripleSet.singleton triple in
    let asgn = { C.asgn_rets = [id]; asgn_rets_formal = [id];
		 asgn_args = []; asgn_args_formal = [];
		 asgn_spec = spec } in
    [C.Assignment_core asgn]
  | Opcode.Load ->
    (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l00225 *)
    let ptr_v = operand instr 0 in
    let ptr = expr_of_llvalue ptr_v in
    let value_t = type_of instr in
    let value_s = sort_of_lltype value_t in
    let value_e = Syntax.mk_fresh_lvar value_s "v" in
    let mem_val = as_llmem value_s value_e in
    let pointer = mk_pointer ptr (expr_of_lltype value_t) mem_val in
    let pre = pointer in
    let id = Syntax.mk_plvar value_s (value_id instr) in
    let post = Syntax.mk_star (Z3.Boolean.mk_eq z3_ctx value_e id) pointer in
    let triple = { C.pre = pre; post = post; modifies = [] } in
    let spec = C.TripleSet.singleton triple in
    let asgn = { C.asgn_rets = [id]; asgn_rets_formal = [id];
		 asgn_args = []; asgn_args_formal = [];
		 asgn_spec = spec } in
    [C.Assignment_core asgn]
  | Opcode.Store ->
    (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l003.4 *)
    let value = operand instr 0 in
    (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l00346 *)
    let ptr_v = operand instr 1 in
    let ptr = expr_of_llvalue ptr_v in
    let value_t = type_of value in
    let value_s = sort_of_lltype value_t in
    let e = Syntax.mk_fresh_lvar llmem_sort "v" in
    let v = as_llmem value_s (expr_of_llvalue value) in
    let pre = mk_pointer ptr (expr_of_lltype value_t) e in
    let post = mk_pointer ptr (expr_of_lltype value_t) v in
    [mk_simple_asgn pre post]
  (* misc instructions that cannot be used in constant expressions *)
  | Opcode.PHI ->
    (* We cannot treat phi nodes directly. Instead, we accumulate the
       phi nodes of the current block to execute all the assignments
       that correspond to the same predecessor block at once (as they
       should according to their semantics) in separate blocks. We
       also record the extra indirection between predecessors of the
       current block and this block. Once all the blocks have been
       translated, we will update the inter-blocks links to go through
       the phi nodes. See the "update_cfg_with_new_phi_blocks"
       function. *)
    let id = value_id instr in
    let s = sort_of_lltype (type_of instr) in
    let x = Syntax.mk_plvar s id in
    let rec dispatch groups = function
      | [] -> groups
      | (v,b)::tl ->
	let e = expr_of_llvalue v in
	try
	  let l = List.assoc b groups in
	  let groups = (b,(x,e)::l)::(List.remove_assoc b groups) in
	  dispatch groups tl
	with Not_found ->
	  dispatch ((b,[x,e])::groups) tl in
    let cur_phi_nodes = fun_env.fun_cur_blk_phi_nodes in
    fun_env.fun_cur_blk_phi_nodes <- dispatch cur_phi_nodes (incoming instr);
    [C.Nop_stmt_core]
  | Opcode.Call -> (
    try
      (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l01339 *)
      let fun_called = operand instr (num_operands instr - 1) in
      let fid = value_id fun_called in
      (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l01237 *)
      let max_param_idx = num_operands instr - 1 in
      let rec params_from_idx i =
	if i = max_param_idx then []
	else expr_of_llvalue (operand instr i)::(params_from_idx (i+1)) in
      let params = params_from_idx 0 in
      let value_t = type_of instr in
      let out = match classify_type value_t with
	| TypeKind.Void -> []
	| _ ->
	  let value_s = sort_of_lltype value_t in
	  [Syntax.mk_plvar value_s (value_id instr)] in
      let call = { C.call_name = fid; call_rets = out; call_args = params } in
      [C.Call_core call]
    with MetaData _ ->
      (* a function call with meta-data in its arguments is for debug info *)
      (* and may be safely ignored *)
      [C.Nop_stmt_core])
  | Opcode.UserOp1
  | Opcode.UserOp2 ->
    warn "Skipping user-defined instruction";
    [C.Nop_stmt_core]
  | Opcode.VAArg ->
    warn "Skipping VAArg instruction";
    [C.Nop_stmt_core]
  | Opcode.Fence ->
    (* this nop is safe since we assume a sequential semantics *)
    [C.Nop_stmt_core]
  | Opcode.AtomicCmpXchg -> implement_this "atomic cmp xchange instr"
  | Opcode.AtomicRMW -> implement_this "atomic RMW instr"
  | Opcode.Resume -> implement_this "resume instr"
  | Opcode.LandingPad -> [C.Nop_stmt_core]
  | Opcode.BitCast
    (* TODO: restore *)
    (* -> *)
    (* let id = value_id instr in *)
    (* let t = expr_of_lltype (type_of instr) in *)
    (* let expr_e = expr_of_op (instr_opcode instr) instr in *)
    (* let post = pconjunction (mkPPred ("bitcast", [mkTypedExpr t ret_arg; expr_of_lltype (type_of instr) ])) *)
    (*   (Z3.Boolean.mk_eq z3_ctx  (mkTypedExpr t ret_arg, expr_e)) in *)
    (* let pre = Syntax.mk_emp in *)
    (* let spec = Spec.mk_spec pre post Spec.ClassMap.empty in *)
    (* [C.Assignment_core ([Vars.concretep_str id],spec,[])] *)
  (* the remaining opcodes are shared with constant expressions *)
  | Opcode.Add | Opcode.FAdd | Opcode.Sub | Opcode.FSub | Opcode.Mul
  | Opcode.FMul | Opcode.UDiv | Opcode.SDiv | Opcode.FDiv | Opcode.URem
  | Opcode.SRem | Opcode.FRem | Opcode.Shl | Opcode.LShr | Opcode.AShr
  | Opcode.And | Opcode.Or | Opcode.Xor | Opcode.Trunc
  | Opcode.ZExt | Opcode.SExt | Opcode.PtrToInt | Opcode.IntToPtr
  | Opcode.GetElementPtr | Opcode.FPToUI | Opcode.FPToSI | Opcode.UIToFP
  | Opcode.SIToFP | Opcode.FPTrunc | Opcode.FPExt | Opcode.ICmp
  | Opcode.FCmp | Opcode.Select | Opcode.ExtractElement | Opcode.InsertElement
  | Opcode.ShuffleVector | Opcode.ExtractValue | Opcode.InsertValue ->
    let s = sort_of_lltype (type_of instr) in
    let id = Syntax.mk_plvar s (value_id instr) in
    let expr_e = expr_of_op (instr_opcode instr) instr in
    let post = Z3.Boolean.mk_eq z3_ctx id expr_e in
    let pre = Syntax.mk_emp in
    let triple = { C.pre = pre; post = post; modifies = [] } in
    let spec = C.TripleSet.singleton triple in
    let asgn = { C.asgn_rets = [id]; asgn_rets_formal = [id];
		 asgn_args = []; asgn_args_formal = [];
		 asgn_spec = spec } in
    [C.Assignment_core asgn]

let rec update_cfg_with_new_phi_blocks bfwd bbwd lsrc = function
  | [] -> []
  | x::[] ->
    let y = match x with
      | C.Goto_stmt_core [l] ->
	C.Goto_stmt_core [bfwd (bbwd lsrc) l]
      | _ -> x in
    y::[]
  | x::y::tl -> x::(update_cfg_with_new_phi_blocks bfwd bbwd lsrc (y::tl))

let make_phi_blocks fun_env =
  let blocks_of_blk_phi_nodes (lab_target,b) =
    let block_of_group (b,l) =
      let lab_src = value_id (value_of_block b) in
      let lab = lab_src^"_to_"^lab_target in
      let label_node = C.Label_stmt_core lab in
      let equalities =
	List.fold_left
	  (fun f (x,v) ->
	    Syntax.mk_star f (Z3.Boolean.mk_eq z3_ctx x v))
	  Syntax.mk_emp l in
      let rets = List.map fst l in
      let triple = { C.pre = Syntax.mk_emp; post = equalities; modifies = [] } in
      let spec = C.TripleSet.singleton triple in
      let asgn = { C.asgn_rets = rets; asgn_rets_formal = rets;
		   asgn_args = []; asgn_args_formal = [];
		   asgn_spec = spec } in
      let branch = C.Goto_stmt_core [lab_target] in
      (lab_src, (lab,[label_node; C.Assignment_core asgn; branch])) in
    let blocks = List.map block_of_group b in
    let f = fun_env.fun_br_to_dest in
    fun_env.fun_br_to_dest <- (fun lsrc ldest ->
      if (ldest = lab_target) && (List.mem_assoc lsrc blocks) then
	fst (List.assoc lsrc blocks)
      else f lsrc ldest);
    List.map snd blocks in
  List.flatten (List.map blocks_of_blk_phi_nodes fun_env.fun_phi_nodes)

let statements_of_block retv fun_env lnsl b =
  (* insert label command from the block's label l, followed by the sequence
     of commands of the block *)
  let l = value_id (value_of_block b) in
  fun_env.fun_blk_label <- l;
  fun_env.fun_cur_blk_phi_nodes <- [];
  let label_node = C.Label_stmt_core l in
  let body_nodes = fold_left_instrs
    (fun cfgs i -> cfgs@(statements_of_instr retv fun_env i)) [] b in
  if fun_env.fun_cur_blk_phi_nodes <> [] then (
    let phi_nodes = fun_env.fun_phi_nodes in
    fun_env.fun_phi_nodes <- (l, fun_env.fun_cur_blk_phi_nodes)::phi_nodes;);
  lnsl@[(l,label_node::body_nodes)]

let body_of_function retv f =
  let fun_env = mk_empty_fun_env () in
  (* it would be more efficient to fold_right here, but then the
     identifiers we generate would be in reversed order, which is
     confusing in output messages *)
  let lab_nodes_list =
    fold_left_blocks (statements_of_block retv fun_env) [] f in
  (* print_endline ("*** Found "^(string_of_int (List.length fun_env.fun_br_blocks))^" conditional branchings and "^(string_of_int (List.length fun_env.fun_phi_nodes))^" blocks with phi nodes"); *)
  let lnsl = lab_nodes_list@fun_env.fun_br_blocks in
  let phi_cfg = make_phi_blocks fun_env in
  let lnsl = List.map
    (fun (l,cfg) ->
      (l, update_cfg_with_new_phi_blocks
	fun_env.fun_br_to_dest fun_env.fun_br_to_orig l cfg)) lnsl in
  let lnsl = lnsl@phi_cfg in
  List.flatten (List.map snd lnsl)

let add_body_of_llfunction procs f =
  let fid = value_id f in
  Format.fprintf Debug.logf "Translating proc %s@\n" fid;
  let proc =
    try List.find (fun p -> p.C.proc_name = fid) procs
    with Not_found -> implement_this ("spec for proc "^fid^ " not found") in
  if Llvm.is_declaration f then proc
  else
    let retv =
      match proc.C.proc_rets with
      | [a] -> Some a
      | [] -> None
      | _ -> assert (false) in
    let body = body_of_function retv f in
    { proc with C.proc_body = Some body }

let question_of_llmodule q m =
  let fill_proc f p = (add_body_of_llfunction q.C.q_procs f)::p in
  let procs = Llvm.fold_right_functions fill_proc m [] in
  { q with C.q_procs = procs }
