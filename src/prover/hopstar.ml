open Printf
open Llvm
open Syntax.LLVMsyntax
open Cfg_core

let verif_list verif_fun l =
  List.fold_left (fun b x -> b && (verif_fun x)) true l

let cfg_node_of_cmd = function
  | Coq_insn_bop (id, bop, sz, value, value') -> Cfg_core.mk_node Core.Nop_stmt_core
  | Coq_insn_fbop (id, fbop, floating_point, value, value') -> Cfg_core.mk_node Core.Nop_stmt_core
  | Coq_insn_extractvalue (id, typ, value, list_const) -> Cfg_core.mk_node Core.Nop_stmt_core
  | Coq_insn_insertvalue (id, typ, value, typ', value', list_const) -> Cfg_core.mk_node Core.Nop_stmt_core
  | Coq_insn_malloc (id, typ, value, align) -> Cfg_core.mk_node Core.Nop_stmt_core
  | Coq_insn_free (id, typ, value) -> Cfg_core.mk_node Core.Nop_stmt_core
  | Coq_insn_alloca (id, typ, value, align) -> Cfg_core.mk_node Core.Nop_stmt_core
  | Coq_insn_load (id, typ, value, align) -> Cfg_core.mk_node Core.Nop_stmt_core
  | Coq_insn_store (id, typ, value, value', align) -> Cfg_core.mk_node Core.Nop_stmt_core
  | Coq_insn_gep (id, inbounds, typ, value, list_sz_value) -> Cfg_core.mk_node Core.Nop_stmt_core
  | Coq_insn_trunc (id, truncop, typ, value, typ') -> Cfg_core.mk_node Core.Nop_stmt_core
  | Coq_insn_ext (id, extop, typ, value, typ') -> Cfg_core.mk_node Core.Nop_stmt_core
  | Coq_insn_cast (id, castop, typ, value, typ') -> Cfg_core.mk_node Core.Nop_stmt_core
  | Coq_insn_icmp (id, cond, typ, value, value') -> Cfg_core.mk_node Core.Nop_stmt_core
  | Coq_insn_fcmp (id, fcond, floating_point, value, value') -> Cfg_core.mk_node Core.Nop_stmt_core
  | Coq_insn_select (id, value, typ, value', value'') -> Cfg_core.mk_node Core.Nop_stmt_core
  | Coq_insn_call (id, noret, clattrs, typ, value, params) -> Cfg_core.mk_node Core.Nop_stmt_core

let cfg_nodes_of_cmds cmds =
  let cfg_nodes = List.map cfg_node_of_cmd cmds in
  Cfg_core.stmts_to_cfg cfg_nodes;
  cfg_nodes

let verif_block fid = function
  | Coq_block_intro (l, phinodes, cmds, terminator) ->
    let cfg_nodes = cfg_nodes_of_cmds cmds in
    let spec = Spec.mk_spec [] [Psyntax.P_False] Spec.ClassMap.empty in
    let log = {Psyntax.seq_rules = []; Psyntax.rw_rules = []; Psyntax.consdecl = []; Psyntax.dummy = ();} in
    Symexec.verify fid cfg_nodes spec log log

let verif_fdef = function
  | Coq_fdef_intro (Coq_fheader_intro (fnattrs, typ, id, args, varg), blocks) ->
    verif_list (verif_block id) blocks

let verif_product = function
  | Coq_product_gvar _ -> true (* TODO: gvar *)
  | Coq_product_fdec _ -> true
  | Coq_product_fdef fdef -> verif_fdef fdef

let verif_module = function
  | Coq_module_intro (lay, ndts, prods) ->
    verif_list verif_product prods

let main in_filename =
  let ic = create_context () in
  let imbuf = MemoryBuffer.of_file in_filename in
  let im = Llvm_bitreader.parse_bitcode ic imbuf in
  let ist = SlotTracker.create_of_module im in
  
  (* Llvm_pretty_printer.travel_module ist im; *)
  let coqim = Llvm2coq.translate_module false ist im in
  (* Coq_pretty_printer.travel_module coqim; *)
 
  print_string "bouarf\n";

  print_string ("mama says "^(if (verif_module coqim) then "yes" else "no")^"\n");

  SlotTracker.dispose ist;
  dispose_module im

let () = match Sys.argv with
  | [| _; in_filename |] -> main in_filename
  | _ -> main "Input.bc"

