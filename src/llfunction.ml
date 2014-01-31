(*** Llfunction: translate a bitcode module into a coreStar CFG *)

(* LLVM modules *)
open Llvm
(* coreStar modules *)
open Psyntax
open Cfg_core
(* llStar modules *)
open Llexpression
open Llutils

(** context when translating a function
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

(** compile an llvm instruction within a function into a piece of CoreStar CFG *) 
let cfg_node_of_instr specs fun_env instr =
  let mk_node stmt =
    let loc = location_of_instr instr in
    let node = Cfg_core.mk_node stmt in
    Printing.add_location node.sid loc;
    node in
  match instr_opcode instr with
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
	(mkEQ (bvargs_of_int 1 1, args_cond)) Spec.ClassMap.empty in
      let assume_then = mk_node (Core.Assignment_core ([],assume_then_spec,[])) in
      let assume_else_spec = Spec.mk_spec mkEmpty
	(mkEQ (bvargs_of_int 1 0, args_cond)) Spec.ClassMap.empty in
      let assume_else = mk_node (Core.Assignment_core ([],assume_else_spec,[])) in
      let then_label = mk_br_block fun_env then_label_orig assume_then in
      let else_label = mk_br_block fun_env else_label_orig assume_else in
      [mk_node (Core.Goto_stmt_core [then_label;else_label])]
  | Opcode.Switch ->
    let args_val = args_of_value (operand instr 0) in
    let mk_case v l =
      let label_orig = label_of_bblock (block_of_value l) in
      let args_cond = args_of_value v in
      let assume_case_spec = Spec.mk_spec mkEmpty
	(mkEQ (args_val, args_cond)) Spec.ClassMap.empty in
      let assume_case = mk_node (Core.Assignment_core ([],assume_case_spec,[])) in
      mk_br_block fun_env label_orig assume_case in
    let default_label =
      let default_orig = label_of_bblock (switch_default_dest instr) in
      let cond = ref(mkEmpty) in
      for i = 1 to (num_operands instr)/2 -1 do
	cond := mkStar !cond (mkNEQ (args_val, args_of_value (operand instr (i*2))))
      done;
      let default_spec = Spec.mk_spec mkEmpty !cond Spec.ClassMap.empty in
      let assume = mk_node (Core.Assignment_core ([],default_spec,[])) in
      mk_br_block fun_env default_orig assume in
    let cases_labels = ref [] in
    for i = 1 to (num_operands instr)/2 -1 do
      cases_labels := mk_case (operand instr (i*2)) (operand instr (i*2 + 1))::
	!cases_labels
    done;
    [mk_node (Core.Goto_stmt_core (default_label::!cases_labels))]
  | Opcode.IndirectBr -> implement_this "indirect branch"
  | Opcode.Invoke -> implement_this "Invoke block terminator"
  | Opcode.Unreachable ->
    (* assert False *)
    let spec = Spec.mk_spec mkFalse [] Spec.ClassMap.empty in
    [mk_node (Core.Assignment_core ([],spec,[]))]
  | Opcode.Invalid
  | Opcode.Invalid2 -> failwith "\"Invalid\" instruction"
  (* Memory Operators *)
  | Opcode.Alloca ->
    let id = Vars.concretep_str (value_id instr) in
    let ptr_t = type_of instr in
    let value_t = element_type ptr_t in
    let e = Arg_var (Vars.freshe ()) in
    let mk_heap x = mkPointer x (args_of_type value_t) e in
    fun_env.fun_alloca_pred <- mkStar fun_env.fun_alloca_pred (mk_heap (Arg_var id));
    let post = mk_heap ret_arg in
    let spec = Spec.mk_spec [] post Spec.ClassMap.empty in
    [mk_node (Core.Assignment_core ([id], spec, []))]
  | Opcode.Load ->
    let id = Vars.concretep_str (value_id instr) in
    (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l00225 *)
    let ptr_v = operand instr 0 in
    let ptr = args_of_value ptr_v in
    let value_t = type_of instr in
    let value_e = Arg_var (Vars.freshe ()) in
    let pointer = mkPointer ptr (args_of_type value_t) value_e in
    let pre = pointer in
    let post = pconjunction (mkEQ(value_e, ret_arg)) pointer in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    [mk_node (Core.Assignment_core ([id], spec, []))]
  | Opcode.Store ->
    (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l003.4 *)
    let value = operand instr 0 in
    (* Hardcoded from http://llvm.org/docs/doxygen/html/Instructions_8h_source.html#l00346 *)
    let ptr_v = operand instr 1 in
    let ptr = args_of_value ptr_v in
    let e = Arg_var (Vars.freshe ()) in
    let v = args_of_value value in
    let value_t = type_of value in
    let pointer_pre = mkPointer ptr (args_of_type value_t) e in
    let pointer_post = mkPointer ptr (args_of_type value_t) v in
    let pre = pointer_pre in
    let post = pointer_post in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    [mk_node (Core.Assignment_core ([], spec, []))]
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
      let call_spec = Logic_spec.spec_of_fun_id specs fid in
      [mk_node (Core.Assignment_core ([Vars.concretep_str id],
				      call_spec,
				      params))]
    with MetaData _ ->
      (* a function call with meta-data in its arguments is for debug info *)
      (* and may be safely ignored *)
      [mk_node Core.Nop_stmt_core])
  | Opcode.UserOp1
  | Opcode.UserOp2 ->
    warn "Skipping user-defined instruction";
    [mk_node Core.Nop_stmt_core]
  | Opcode.VAArg ->
    warn "Skipping VAArg instruction";
    [mk_node Core.Nop_stmt_core]
  | Opcode.Fence ->
    (* this nop is safe since we assume a sequential semantics *)
    [mk_node Core.Nop_stmt_core]
  | Opcode.AtomicCmpXchg -> implement_this "atomic cmp xchange instr"
  | Opcode.AtomicRMW -> implement_this "atomic RMW instr"
  | Opcode.Resume -> implement_this "resume instr"
  | Opcode.LandingPad -> [mk_node Core.Nop_stmt_core]
  | Opcode.BitCast ->
    let id = value_id instr in
    let args_e = args_of_op (instr_opcode instr) instr in
    let post = pconjunction (mkPPred ("bitcast", [ret_arg; args_of_type (type_of instr) ]))
      (mkEQ (ret_arg, args_e)) in
    let pre = mkEmpty in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    [mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))]
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
    let id = value_id instr in
    let args_e = args_of_op (instr_opcode instr) instr in
    let post = mkEQ (ret_arg, args_e) in
    let pre = mkEmpty in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    [mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))]

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

let cfg_nodes_of_block specs fun_env lnsl b =
  (* insert label command from the block's label l, followed by the sequence
     of commands of the block *)
  let l = value_id (value_of_block b) in
  fun_env.fun_blk_label <- l;
  fun_env.fun_cur_blk_phi_nodes <- [];
  let label_node = mk_node (Core.Label_stmt_core l) in
  let body_nodes = fold_left_instrs
    (fun cfgs i -> cfgs@(cfg_node_of_instr specs fun_env i)) [] b in
  if fun_env.fun_cur_blk_phi_nodes <> [] then (
    let phi_nodes = fun_env.fun_phi_nodes in
    fun_env.fun_phi_nodes <- (l, fun_env.fun_cur_blk_phi_nodes)::phi_nodes;);
  lnsl@[(l,label_node::body_nodes)]

let cfg_nodes_of_function specs f =
  let fun_env = mk_empty_fun_env () in
  (* it would be more efficient to fold_right here, but then the
     identifiers we generate would be in reversed order, which is
     confusing in output messages *)
  let lab_nodes_list =
    fold_left_blocks (cfg_nodes_of_block specs fun_env) [] f in
  (* print_endline ("*** Found "^(string_of_int (List.length fun_env.fun_br_blocks))^" conditional branchings and "^(string_of_int (List.length fun_env.fun_phi_nodes))^" blocks with phi nodes"); *)
  let lnsl = lab_nodes_list@fun_env.fun_br_blocks in
  let phi_cfg = make_phi_blocks fun_env in
  let lnsl = List.map
    (fun (l,cfg) ->
      (l, update_cfg_with_new_phi_blocks
	fun_env.fun_br_to_dest fun_env.fun_br_to_orig l cfg)) lnsl in
  let lnsl = lnsl@phi_cfg in
  List.flatten (List.map snd lnsl)
