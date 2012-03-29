open Printf
open Llvm
open Syntax.LLVMsyntax
open Cfg_core
open Psyntax
open Logic_spec

type verify_env = {
  mutable logic: Psyntax.logic;
  mutable abs_rules: Psyntax.logic;
  mutable specs: funspecs;
  mutable namedts: (id * typ) list;
  mutable gvars: id list; (* TODO: wrong *)
  mutable result: bool;
}

let env = {
  logic = empty_logic;
  abs_rules = empty_logic;
  specs = [];
  namedts = [];
  gvars = [];
  result = true;
}

let int_of_coq_Int n =
  Int64.to_int (APInt.get_sext_value n)

let string_of_coq_Int n =
  string_of_int (int_of_coq_Int n)

let ret_arg = Arg_var(Spec.ret_v1)

let env_add_namedts ndts =
  env.namedts <- List.map (function Coq_namedt_intro (i,t) -> (i,t)) ndts@(env.namedts)

let env_add_gvar gvar =
  ()

let env_add_logic_seq_rules sr  =
  ()
  (* env.logic <- { env.logic with seq_rules = env.logic.seq_rules@sr } *)

let implement_this s = failwith ("Not implemented: "^s)

let spec_of_fun_id fid =
  let rec aux = function
    | Logic_spec.Funspec(i, spec)::ss when "@"^i = fid -> spec
    | _::ss -> aux ss
    | [] -> failwith ("spec for "^fid^" not found.") in
  aux env.specs

let rec args_of_typ = function
    | Coq_typ_int s ->
      Arg_op("integer_type", [])
    | Coq_typ_floatpoint f10 ->
      Arg_op("float_type", [])
    | Coq_typ_void ->
      Arg_op("void",[])
    | Coq_typ_label -> implement_this "label type"
    | Coq_typ_metadata -> implement_this "metadata type"
    | Coq_typ_array (s, t) ->
      Arg_op("array_type", [args_of_typ t])
    | Coq_typ_function (t, l, v) when not v ->
      Arg_op("function_type", args_of_typ t::(args_of_list_typ l))
    | Coq_typ_function (t, l, v) -> (* v is true *)
      implement_this "function type with varg"
    | Coq_typ_struct l ->
      Arg_op("struct_type", args_of_list_typ l)
    | Coq_typ_pointer t ->
      Arg_op("pointer_type", [args_of_typ t])
    | Coq_typ_opaque ->
      Arg_op("opaque_type", [])
    | Coq_typ_namedt i ->
      Arg_op("named_type", [Arg_string i])
and args_of_list_typ = function
    | Nil_list_typ -> []
    | Cons_list_typ (t, l) -> args_of_typ t::(args_of_list_typ l)

let argsbop_of_bop = function
  | Coq_bop_add -> "builtin_plus"
  | Coq_bop_sub -> "builtin_minus"
  | Coq_bop_mul -> "builtin_mult"
  | Coq_bop_udiv
  | Coq_bop_sdiv
  | Coq_bop_urem
  | Coq_bop_srem 
  | Coq_bop_shl
  | Coq_bop_lshr
  | Coq_bop_ashr
  | Coq_bop_and
  | Coq_bop_or
  | Coq_bop_xor -> "bop_undet"

let rec args_of_const = function
  | Coq_const_zeroinitializer (typ) -> implement_this "zeroinitializer"
  | Coq_const_int (sz, coq_Int) ->
    Arg_op("numeric_const",[Arg_string(string_of_coq_Int coq_Int)])
  | Coq_const_floatpoint (floating_point, coq_Float) ->
    Arg_op("numeric_const",[])
  | Coq_const_undef (typ) -> implement_this "undef"
  | Coq_const_null (typ) ->
    Arg_op("nil",[])
  | Coq_const_arr (typ, list_const) -> implement_this "arr"
  | Coq_const_struct (list_const) -> implement_this "struct"
  | Coq_const_gid (typ, id) ->
    Arg_var (Vars.concretep_str id)
  | Coq_const_truncop (truncop, const, typ) -> implement_this "truncop"
  | Coq_const_extop (extop, const, typ) -> implement_this "extop"
  | Coq_const_castop (castop, const, typ) -> implement_this "castop"
  | Coq_const_gep (inbounds, const, list_const) ->
    Arg_op("getelementpointer", args_of_const const::(args_of_list_const list_const))
  | Coq_const_select (const, const', const'') -> implement_this "select"
  | Coq_const_icmp (cond, const, const') -> implement_this "icmp"
  | Coq_const_fcmp (fcond, const, const') -> implement_this "fcmp"
  | Coq_const_extractvalue (const, list_const) -> implement_this "extractvalue"
  | Coq_const_insertvalue (const, const', list_const) -> implement_this "insertvalue"
  | Coq_const_bop (bop, const, const') ->
    let abop = argsbop_of_bop bop in
    Arg_op(abop, [args_of_const const; args_of_const const'])
  | Coq_const_fbop (fbop, const, const') -> implement_this "fbop"
and args_of_list_const = function
  | Nil_list_const -> implement_this ""
  | Cons_list_const (const, list_const) -> implement_this ""
    
let args_of_value = function
  | Coq_value_id (id) -> Arg_var (Vars.concretep_str id)
  | Coq_value_const (const) -> args_of_const const

let args_of_param ((typ, attributes), value) = args_of_value value

let logic_of_namedt name = function
  | Coq_typ_int _ 
  | Coq_typ_floatpoint _
  | Coq_typ_void
  | Coq_typ_label
  | Coq_typ_opaque
  | Coq_typ_namedt _
  | Coq_typ_metadata
  | Coq_typ_function _
  | Coq_typ_pointer _ -> []
  | Coq_typ_struct (typs) -> []
  (*   let rec aux n = function *)
  (*     | Nil_list_typ -> *)
  (* 	if n>=0 then *)
  (* 	  failwith "out of struct break_up_aggregate?" *)
  (* 	else [] *)
  (*     | Cons_list_typ(typ, tail) -> *)
  (* 	(if n <> 0 then *)
  (* 	    let ptr_typ = args_of_typ (Coq_typ_pointer typ) in *)
  (* 	    let e = Vars.freshe () in *)
  (* 	    [P_SPred("pointer", [root; ptr; ptr_typ; Arg_var e])] *)
  (* 	 else *)
  (* 	    break_up_aggregate id root typ ptr list_sz_value *)
  (* 	)@(aux (n-1) tail) in *)
  (*   match value with *)
  (*     | Coq_value_const(Coq_const_int (sz, coq_Int)) -> *)
  (* 	print_string ("blerk "^(APInt.to_string coq_Int)^"\n"); *)
  (* 	let n = int_of_string (APInt.to_string coq_Int) in *)
  (* 	aux n typs *)
  (*     | _ -> failwith "non-constant value in break_up_aggregate" *)
  (* ) *)
  | Coq_typ_array _ -> implement_this "array aggregate"

let rec spred_of_typ ptr = function
  | Coq_typ_int _
  | Coq_typ_floatpoint _
  | Coq_typ_void
  | Coq_typ_label
  | Coq_typ_opaque
  | Coq_typ_metadata
  | Coq_typ_function _ -> mkEmpty
  | Coq_typ_namedt tname as ptr_typ ->
    let ptr_typ_args = args_of_typ ptr_typ in
    let e = Arg_var (Vars.freshe ()) in
    let pointer = mkSPred ("pointer", [ptr; e]) in
    let typepp = mkPPred ("type", [ptr; ptr_typ_args]) in
    pconjunction pointer typepp
  | Coq_typ_pointer t as ptr_typ ->
    let ptr_typ_args = args_of_typ ptr_typ in
    let e = Arg_var (Vars.freshe ()) in
    let pointer = mkSPred ("pointer", [ptr; e]) in
    let typepp = mkPPred ("type", [ptr; ptr_typ_args]) in
    pconjunction typepp
      (mkStar pointer (spred_of_typ e t))
  | Coq_typ_struct typs as ptr_typ ->
    let ptr_typ_args = args_of_typ ptr_typ in
    let e = Arg_var (Vars.freshe ()) in
    let pointer = mkSPred ("pointer", [ptr; e]) in
    let typepp = mkPPred ("type", [ptr; ptr_typ_args]) in
    pconjunction typepp
      (mkStar pointer (spred_of_list_typ e typs))
  | Coq_typ_array _ -> implement_this "SPred of array type"
and spred_of_list_typ ptr = function
  | Nil_list_typ -> mkEmpty
  | Cons_list_typ (t, l) ->
    let ptr_typ_args = args_of_typ t in
    let e = Arg_var (Vars.freshe ()) in
    let pointer = mkSPred ("pointer", [ptr; e]) in
    let typepp = mkPPred ("type", [ptr; ptr_typ_args]) in
    pconjunction typepp
      (mkStar pointer (spred_of_list_typ e l))

let ppred_of_gep typ x ptr lsv =
  let rec jump_chain_of_list_sz_value = function
  | Nil_list_sz_value -> Arg_op ("jump_end", [])
  | Cons_list_sz_value(sz, value, list_sz_value) ->
    match value with
      | Coq_value_const(Coq_const_int (sz, coq_Int)) ->
	let n = string_of_coq_Int coq_Int in
	Arg_op ("jump", [Arg_op ("numeric_const", [Arg_string n]);
			 jump_chain_of_list_sz_value list_sz_value])
      | _ -> failwith "non-constant value in break_up_aggregate" in
  let jump_chain = jump_chain_of_list_sz_value lsv in
  mkPPred ("eltptr", [x; ptr; jump_chain])

let verify_list verify_fun l =
  List.iter verify_fun l

let cfg_node_of_cmd = function
  | Coq_insn_bop (id, bop, sz, value, value') -> mk_node Core.Nop_stmt_core
  | Coq_insn_fbop (id, fbop, floating_point, value, value') -> mk_node Core.Nop_stmt_core
  | Coq_insn_extractvalue (id, typ, value, list_const) -> mk_node Core.Nop_stmt_core
  | Coq_insn_insertvalue (id, typ, value, typ', value', list_const) -> mk_node Core.Nop_stmt_core
  | Coq_insn_malloc (id, typ, value, align) -> mk_node Core.Nop_stmt_core
  | Coq_insn_free (id, typ, value) -> mk_node Core.Nop_stmt_core
  | Coq_insn_alloca (id, typ, value, align) ->
    let x = Arg_var (Vars.concretep_str id) in
    let e = Arg_var (Vars.freshe ()) in
    let t = args_of_typ (Coq_typ_pointer typ) in
    let typepp = mkPPred ("type", [x; t]) in
    let alloca = mkPPred ("alloca", [x; t]) in
    let pointer = mkSPred ("pointer", [x; e]) in
    let post = pconjunction alloca
      (pconjunction typepp
	 (mkStar pointer (spred_of_typ e typ))) in
    let spec = Spec.mk_spec [] post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([], spec, []))
  | Coq_insn_load (id, typ, ptr_v, align) ->
    let e = Arg_var (Vars.freshe ()) in
    let ptr = args_of_value ptr_v in
    let ptr_typ = args_of_typ (Coq_typ_pointer typ) in
    let typepp = mkPPred ("type", [ptr; ptr_typ]) in
    let pointer = mkSPred ("pointer", [ptr; e]) in
    let pre = pconjunction typepp pointer in
    let post = pconjunction (mkEQ(ret_arg, e)) pointer in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([Vars.concretep_str id], spec, []))
  | Coq_insn_store (id, typ, value, ptr_v, align) ->
    let ptr = args_of_value ptr_v in
    let e = Arg_var (Vars.freshe ()) in
    let v = args_of_value value in
    let ptr_typ = args_of_typ (Coq_typ_pointer typ) in
    let typepp = mkPPred ("type", [ptr; ptr_typ]) in
    let pointer_pre = mkSPred ("pointer", [ptr; e]) in
    let pointer_post = mkSPred ("pointer", [ptr; v]) in
    let pre = pconjunction typepp pointer_pre in
    let post = pointer_post in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([], spec, []))
  | Coq_insn_gep (id, inbounds, typ, value, list_sz_value) ->
    let post = ppred_of_gep typ ret_arg (args_of_value value) list_sz_value in
    let spec = Spec.mk_spec [] post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))
  | Coq_insn_trunc (id, truncop, typ, value, typ') -> mk_node Core.Nop_stmt_core
  | Coq_insn_ext (id, extop, typ, value, typ') -> mk_node Core.Nop_stmt_core
  | Coq_insn_cast (id, castop, typ, value, typ') ->
    let v = args_of_value value in
    let typepp_pre = mkPPred ("type", [v; args_of_typ typ]) in
    let typepp_post = mkPPred ("type", [ret_arg; args_of_typ typ']) in
    let pre = typepp_pre in
    let post = pconjunction (mkEQ(ret_arg, v)) typepp_post in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))
  | Coq_insn_icmp (id, cond, typ, value, value') -> mk_node Core.Nop_stmt_core
  | Coq_insn_fcmp (id, fcond, floating_point, value, value') -> mk_node Core.Nop_stmt_core
  | Coq_insn_select (id, value, typ, value', value'') -> mk_node Core.Nop_stmt_core
  | Coq_insn_call (id, noret, clattrs, typ, value, params) ->
    match (value, typ) with
      |	(Coq_value_const (Coq_const_gid (typ,fid)), Coq_typ_function (ret_typ, param_typs, varg)) ->
	let spec = spec_of_fun_id fid in
	let typepp_post = mkPPred ("type", [ret_arg; args_of_typ ret_typ]) in
	let call_spec =
	  { spec with Spec.post = pconjunction typepp_post spec.Spec.post } in
	mk_node (Core.Assignment_core ([Vars.concretep_str id],
				       call_spec,
				       List.map args_of_param params))
      | _ -> implement_this "fancy function call"


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
	let post = mkEQ(ret_arg, p0) in
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

let verify_fdef = function
  | Coq_fdef_intro (Coq_fheader_intro (fnattrs, typ, id, args, varg), blocks) ->
    print_string ("verifying "^id^"\n");
    let cfg_nodes = cfg_nodes_of_blocks blocks in
    let spec = spec_of_fun_id id in
    env.result <- Symexec.verify id cfg_nodes spec env.logic env.abs_rules

let verify_product = function
  | Coq_product_gvar gv -> env_add_gvar gv
  | Coq_product_fdec _ -> ()
  | Coq_product_fdef fdef -> verify_fdef fdef

let verify_module = function
  | Coq_module_intro (lay, ndts, prods) ->
    env_add_namedts ndts;
    let fold_unfold_logic = List.fold_left
      (fun logics (Coq_namedt_intro (id, typ)) -> logics@(logic_of_namedt id typ))
      []
      ndts in
    env_add_logic_seq_rules fold_unfold_logic;
    verify_list verify_product prods

let go logic abs_rules spec_list m =
  print_string ("It is on!\n");
  env.logic <- logic; env.abs_rules <- abs_rules; env.specs <- spec_list;
  verify_module m;
  env.result
