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

let mkPointer ptr ptr_typ v = mkSPred ("pointer", [ptr; ptr_typ; v])

let env_add_namedts ndts =
  env.namedts <- List.map (function Coq_namedt_intro (i,t) -> (i,t)) ndts@(env.namedts)

let env_add_gvar gvar =
  ()

let env_add_logic_seq_rules sr  =
  env.logic <- { env.logic with seq_rules = env.logic.seq_rules@sr }

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

let logic_of_namedt name typ =
  let typs = match typ with
    | Coq_typ_struct l -> l
    | _ -> implement_this "named type that is not a struct" in
  let rec unfolded_form p off_to_val offset = function
    | Nil_list_typ -> mkEmpty
    | Cons_list_typ (t, l) ->
      let off = string_of_int offset in
      mkStar
	(mkPointer
	   (Arg_op ("builtin_plus", [p; Arg_op("numeric_const", [Arg_string off])]))
	   (Arg_op ("pointer_type", [args_of_typ t]))
	   (off_to_val off))
	(unfolded_form p off_to_val (offset + 1) l) in
  let any_var x = Arg_var (Vars.AnyVar (0, x)) in
  let p = any_var "p" in
  let vp = any_var "vp" in
  let x = any_var "x" in
  let n = any_var "n" in
  let j = any_var "j" in
  let t = any_var "t" in
  let v = any_var "v" in
  let target_pointer = mkPointer x t v in
  let eltptr_concl = mkPPred ("eltptr", [x; p; Arg_op ("jump_named", [Arg_string name; n; j])]) in
  let eltptr_prem = mkPPred ("eltptr", [x; Arg_op ("builtin_plus", [p;n]); j]) in

  let unfolded_pointers = unfolded_form p
    (fun offset -> Arg_op ("field", [Arg_op ("numeric_const", [Arg_string offset]); vp]))
    0 typs in
  let conclusion_lhs =
    pconjunction
      (mkPointer p (Arg_op ("named_type", [Arg_string name])) vp)
      eltptr_concl in
  let conclusion_rhs = target_pointer in
  let conclusion = (mkEmpty, conclusion_lhs, conclusion_rhs, mkEmpty) in
  let premise_lhs = pconjunction unfolded_pointers eltptr_prem in
  let premise_rhs = target_pointer in
  let premise = (mkEmpty, premise_lhs, premise_rhs, mkEmpty) in
  let without = mkEQ (p,x) in
  let geteltptr_rule = (conclusion, [[premise]], "geteltptr_"^name, (without, []), []) in


  let unfolded_pointers = unfolded_form p (fun off -> any_var ("v"^off)) 0 typs in
  let named_pointer = mkPointer p (Arg_op ("named_type", [Arg_string name])) vp in
  let conclusion = (mkEmpty, unfolded_pointers, named_pointer, mkEmpty) in
  let premise = (unfolded_pointers, mkEmpty, mkEmpty, mkEmpty) in
  let fold_rule = (conclusion, [[premise]], "fold_"^name, ([], []), []) in

  let unfolded_pointers = unfolded_form p (fun _ -> vp) 0 typs in
  let rec geteltptr_defer_rules offset = function
    | [] -> []
    | p::tl ->
      let conclusion = (mkEmpty, pconjunction [p] eltptr_concl, target_pointer, mkEmpty) in
      let premise = (mkEmpty, pconjunction [p] eltptr_prem, target_pointer, mkEmpty) in
      (conclusion, [[premise]],
       "geteltptr_defer_"^name^(string_of_int offset),
       (without, []), [])::(geteltptr_defer_rules (offset+1) tl) in

  geteltptr_rule::fold_rule::(geteltptr_defer_rules 0 unfolded_pointers)

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
    let pointer = mkSPred ("pointer", [ptr; ptr_typ_args; e]) in
    pointer
  | Coq_typ_pointer t as ptr_typ ->
    let ptr_typ_args = args_of_typ ptr_typ in
    let e = Arg_var (Vars.freshe ()) in
    let pointer = mkSPred ("pointer", [ptr; ptr_typ_args; e]) in
    pointer
  | Coq_typ_struct typs as ptr_typ ->
    let ptr_typ_args = args_of_typ ptr_typ in
    let e = Arg_var (Vars.freshe ()) in
    let pointer = mkSPred ("pointer", [ptr; ptr_typ_args; e]) in
    mkStar pointer (spred_of_list_typ e typs)
  | Coq_typ_array _ -> implement_this "SPred of array type"
and spred_of_list_typ ptr = function
  | Nil_list_typ -> mkEmpty
  | Cons_list_typ (t, l) ->
    let ptr_typ_args = args_of_typ t in
    let e = Arg_var (Vars.freshe ()) in
    let pointer = mkSPred ("pointer", [ptr; ptr_typ_args; e]) in
    mkStar pointer (spred_of_list_typ e l)

let ppred_of_gep typ x ptr lsv =
  let jump_end = Arg_op ("jump_end", []) in
  let rec jump_chain_of_list_sz_value typ = function
  | Nil_list_sz_value -> jump_end
  | Cons_list_sz_value(sz, value, list_sz_value) ->
    let n =
    match value with
      | Coq_value_const(Coq_const_int (sz, coq_Int)) -> string_of_coq_Int coq_Int
      | _ -> failwith "non-constant value in break_up_aggregate" in
    match typ with
      | Coq_typ_int _ 
      | Coq_typ_floatpoint _
      | Coq_typ_void
      | Coq_typ_label
      | Coq_typ_opaque
      | Coq_typ_metadata
      | Coq_typ_struct _
      | Coq_typ_function _ -> implement_this "weird gep"
      | Coq_typ_namedt name ->
	Arg_op ("jump_named", [Arg_string name; Arg_op ("numeric_const", [Arg_string n]);
			       jump_end])
      | Coq_typ_pointer t ->
	Arg_op ("jump_ptr", [Arg_op ("numeric_const", [Arg_string n]);
			     jump_chain_of_list_sz_value t list_sz_value])
      | Coq_typ_array _ -> implement_this "array aggregate" in
  let jump_chain = jump_chain_of_list_sz_value typ lsv in
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
    let alloca = mkSPred ("alloca", [x; t]) in
    let pointer = mkSPred ("pointer", [x; t; e]) in
    let post = mkStar alloca (mkStar pointer (spred_of_typ e typ)) in
    let spec = Spec.mk_spec [] post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([], spec, []))
  | Coq_insn_load (id, typ, ptr_v, align) ->
    let e = Arg_var (Vars.freshe ()) in
    let ptr = args_of_value ptr_v in
    let ptr_typ = args_of_typ (Coq_typ_pointer typ) in
    let pointer = mkSPred ("pointer", [ptr; ptr_typ; e]) in
    let pre = pointer in
    let post = pconjunction (mkEQ(ret_arg, e)) pointer in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([Vars.concretep_str id], spec, []))
  | Coq_insn_store (id, typ, value, ptr_v, align) ->
    let ptr = args_of_value ptr_v in
    let e = Arg_var (Vars.freshe ()) in
    let v = args_of_value value in
    let ptr_typ = args_of_typ (Coq_typ_pointer typ) in
    let pointer_pre = mkSPred ("pointer", [ptr; ptr_typ; e]) in
    let pointer_post = mkSPred ("pointer", [ptr; ptr_typ; v]) in
    let pre = pointer_pre in
    let post = pointer_post in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([], spec, []))
  | Coq_insn_gep (id, inbounds, typ, value, list_sz_value) ->
    let post = ppred_of_gep (Coq_typ_pointer typ) ret_arg (args_of_value value) list_sz_value in
    let spec = Spec.mk_spec [] post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))
  | Coq_insn_trunc (id, truncop, typ, value, typ') -> mk_node Core.Nop_stmt_core
  | Coq_insn_ext (id, extop, typ, value, typ') -> mk_node Core.Nop_stmt_core
  | Coq_insn_cast (id, Coq_castop_bitcast, Coq_typ_pointer(Coq_typ_int i), value, Coq_typ_pointer typ') when i=8 ->
    (* hack to handle malloc + bitcast. Let's hope for the best... *)
    let v = args_of_value value in
    let e = Arg_var (Vars.freshe ()) in
    let ptr_typ = args_of_typ (Coq_typ_pointer typ') in
    let void_pointer = mkSPred ("malloc_block", [v]) in
    let cast_pointer = mkSPred ("pointer", [ret_arg; ptr_typ; e]) in
    let malloced = mkSPred ("malloced", [ret_arg; args_of_typ typ']) in
    let pre = void_pointer in
    let post = pconjunction (mkEQ(ret_arg, v))
      (mkStar malloced (mkStar cast_pointer (spred_of_typ e typ'))) in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))
  | Coq_insn_cast (id, castop, typ, value, typ') ->
    let v = args_of_value value in
    let pre = mkEmpty in
    let post = mkEQ(ret_arg, v) in
    let spec = Spec.mk_spec pre post Spec.ClassMap.empty in
    mk_node (Core.Assignment_core ([Vars.concretep_str id],spec,[]))
  | Coq_insn_icmp (id, cond, typ, value, value') -> mk_node Core.Nop_stmt_core
  | Coq_insn_fcmp (id, fcond, floating_point, value, value') -> mk_node Core.Nop_stmt_core
  | Coq_insn_select (id, value, typ, value', value'') -> mk_node Core.Nop_stmt_core
  | Coq_insn_call (id, noret, clattrs, typ, value, params) ->
    match (value, typ) with
      |	(Coq_value_const (Coq_const_gid (typ,fid)), Coq_typ_function (ret_typ, param_typs, varg)) ->
	let call_spec = spec_of_fun_id fid in
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
