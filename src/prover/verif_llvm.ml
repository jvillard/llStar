open Printf
open Llvm
open Syntax.LLVMsyntax
open Cfg_core
open Psyntax

let implement_this s = failwith ("Not implemented: "^s)

let spec_of_fun_id fid =
  Spec.mk_spec [] (mkEQ(Arg_var(Spec.ret_v1),Arg_op("numeric_const",[Arg_string "0"]))) Spec.ClassMap.empty

let rec args_of_const = function
  | Coq_const_zeroinitializer (typ) -> implement_this "zeroinitializer"
  | Coq_const_int (sz, coq_Int) ->
    Arg_op("numeric_const",[Arg_string(APInt.to_string coq_Int)])
  | Coq_const_floatpoint (floating_point, coq_Float) ->
    Arg_op("numeric_const",[])
  | Coq_const_undef (typ) -> implement_this "undef"
  | Coq_const_null (typ) ->
    Arg_op("nil",[])
  | Coq_const_arr (typ, list_const) -> implement_this "arr"
  | Coq_const_struct (list_const) -> implement_this "struct"
  | Coq_const_gid (typ, id) -> implement_this "gid"
  | Coq_const_truncop (truncop, const, typ) -> implement_this "truncop"
  | Coq_const_extop (extop, const, typ) -> implement_this "extop"
  | Coq_const_castop (castop, const, typ) -> implement_this "castop"
  | Coq_const_gep (inbounds, const, list_const) -> implement_this "gep"
  | Coq_const_select (const, const', const'') -> implement_this "select"
  | Coq_const_icmp (cond, const, const') -> implement_this "icmp"
  | Coq_const_fcmp (fcond, const, const') -> implement_this "fcmp"
  | Coq_const_extractvalue (const, list_const) -> implement_this "extractvalue"
  | Coq_const_insertvalue (const, const', list_const) -> implement_this "insertvalue"
  | Coq_const_bop (bop, const, const') -> implement_this "bop"
  | Coq_const_fbop (fbop, const, const') -> implement_this "fbop"
and args_of_list_const = function
  | Nil_list_const -> implement_this ""
  | Cons_list_const (const, list_const) -> implement_this ""
    
let args_of_value = function
  | Coq_value_id (id) -> Arg_var (Vars.concretep_str id)
  | Coq_value_const (const) -> args_of_const const



let verif_list verif_fun l =
  List.fold_left (fun b x -> b && (verif_fun x)) true l

let cfg_node_of_cmd = function
  | Coq_insn_bop (id, bop, sz, value, value') -> mk_node Core.Nop_stmt_core
  | Coq_insn_fbop (id, fbop, floating_point, value, value') -> mk_node Core.Nop_stmt_core
  | Coq_insn_extractvalue (id, typ, value, list_const) -> mk_node Core.Nop_stmt_core
  | Coq_insn_insertvalue (id, typ, value, typ', value', list_const) -> mk_node Core.Nop_stmt_core
  | Coq_insn_malloc (id, typ, value, align) -> mk_node Core.Nop_stmt_core
  | Coq_insn_free (id, typ, value) -> mk_node Core.Nop_stmt_core
  | Coq_insn_alloca (id, typ, value, align) ->
    let x = Vars.concretep_str id in
    let e = Vars.freshe () in
    let post = [P_SPred("field", [Arg_var x; Arg_string "ptr";Arg_var e])] in
    let spec = Spec.mk_spec [] post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([], spec, []))
  | Coq_insn_load (id, typ, value, align) ->
    let e = Vars.freshe () in
    let v = args_of_value value in
    let pre = [P_SPred("field", [v; Arg_string "ptr";Arg_var e])] in
    let post = pconjunction (mkEQ(Arg_var(Spec.ret_v1), Arg_var e))
      ([P_SPred("field", [v; Arg_string "ptr";Arg_var e])]) in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([Vars.concretep_str id], spec, []))
  | Coq_insn_store (id, typ, value, pointer, align) ->
    let x = args_of_value pointer in
    let e = Vars.freshe () in
    let v = args_of_value value in
    let pre = [P_SPred("field", [x; Arg_string "ptr";Arg_var e])] in
    let post = [P_SPred("field", [x; Arg_string "ptr"; v])] in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([], spec, []))
  | Coq_insn_gep (id, inbounds, typ, value, list_sz_value) -> mk_node Core.Nop_stmt_core
  | Coq_insn_trunc (id, truncop, typ, value, typ') -> mk_node Core.Nop_stmt_core
  | Coq_insn_ext (id, extop, typ, value, typ') -> mk_node Core.Nop_stmt_core
  | Coq_insn_cast (id, castop, typ, value, typ') -> mk_node Core.Nop_stmt_core
  | Coq_insn_icmp (id, cond, typ, value, value') -> mk_node Core.Nop_stmt_core
  | Coq_insn_fcmp (id, fcond, floating_point, value, value') -> mk_node Core.Nop_stmt_core
  | Coq_insn_select (id, value, typ, value', value'') -> mk_node Core.Nop_stmt_core
  | Coq_insn_call (id, noret, clattrs, typ, value, params) -> mk_node Core.Nop_stmt_core

let cfg_nodes_of_cmds cmds =
  List.map cfg_node_of_cmd cmds

let cfg_nodes_of_block = function
  | Coq_block_intro (l, phinodes, cmds, terminator) ->
    (* insert label command from the block's label l, followed by the sequence
       of commands of the block *)
    let label_node = mk_node (Core.Label_stmt_core l) in
    let body_nodes = cfg_nodes_of_cmds cmds in
    let terminator_stmts = match terminator with
      | Coq_insn_return (id, typ, value) ->
	let p0 = Arg_var(Vars.concretep_str ("@parameter"^(string_of_int 0)^":")) in
	let post = mkEQ(Arg_var(Spec.ret_v1),p0) in
	let spec = Spec.mk_spec [] post Spec.ClassMap.empty in
	[Core.Assignment_core  ([], spec, [args_of_value value]); Core.End ]
      | Coq_insn_return_void (id) -> [Core.End]
      | Coq_insn_br (id, value, lif, lelse) -> [Core.Goto_stmt_core [lif;lelse]; Core.End] (* TODO: implement cond *)
      | Coq_insn_br_uncond (id, l) -> [Core.Goto_stmt_core [l]; Core.End]
      | Coq_insn_unreachable (id) ->
	(* if llvm thinks it's unreachable, let's tell hopstar by assuming False *)
	let spec = Spec.mk_spec [] mkFalse Spec.ClassMap.empty in
	[Core.Assignment_core ([],spec,[]); Core.End]
    in
    label_node::(body_nodes@(List.map mk_node terminator_stmts))

let cfg_nodes_of_blocks bs =
  let ns = List.fold_left (fun ns b -> ns@(cfg_nodes_of_block b)) [] bs in
  stmts_to_cfg ns; ns

let verif_fdef logic abs_rules spec_list = function
  | Coq_fdef_intro (Coq_fheader_intro (fnattrs, typ, id, args, varg), blocks) ->
    let cfg_nodes = cfg_nodes_of_blocks blocks in
    let spec = spec_of_fun_id id in
    Symexec.verify id cfg_nodes spec logic abs_rules

let verif_product logic abs_rules spec_list = function
  | Coq_product_gvar _ -> true (* TODO: gvar *)
  | Coq_product_fdec _ -> true
  | Coq_product_fdef fdef -> verif_fdef logic abs_rules spec_list fdef

let verif_module logic abs_rules spec_list = function
  | Coq_module_intro (lay, ndts, prods) ->
    verif_list (verif_product logic abs_rules spec_list) prods
