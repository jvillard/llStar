open Format
(* LLVM modules *)
open Llvm
(* coreStar modules *)
open Config
open Cfg_core
open Debug
open Psyntax
(* LStar modules *)
open Llexpression
open Llfunction
open Llutils
open Logic_spec
open Rulegen

let pp_spec fmt (pre,post) =
  Format.fprintf fmt "@[{%a}\n{%a}\n@."
    Sepprover.string_inner_form pre Sepprover.string_inner_form post

let dump_specs_of_function fid specs =
  let file = Filename.concat !Lstar_config.outdir (!Lstar_config.bitcode_base_name ^ "." ^ fid ^ ".specs") in
  let specs_out = open_out file in
  let specs_fmt = Format.formatter_of_out_channel specs_out in
  Format.fprintf specs_fmt "%a" (Debug.pp_list pp_spec) specs;
  close_out specs_out

(** dumps logic rules into a file in the current directory *)
let dump_logic_rules name rs =
  let file = Filename.concat !Lstar_config.outdir (!Lstar_config.bitcode_base_name ^ "." ^ name) in
  let rules_out = open_out file in
  let rules_fmt = Format.formatter_of_out_channel rules_out in
  Format.fprintf rules_fmt "@[%a@." (Debug.pp_list pp_sequent_rule) rs;
  close_out rules_out

let dump_rewrite_rules name rs =
  let file = Filename.concat !Lstar_config.outdir (!Lstar_config.bitcode_base_name ^ "." ^ name) in
  let rules_out = open_out file in
  let rules_fmt = Format.formatter_of_out_channel rules_out in
  Format.fprintf rules_fmt "@[%a@." (Debug.pp_list pp_rewrite_rule) rs;
  close_out rules_out

let verify_function logic abduct_logic abstraction_rules specs f =
  let id = value_id f in
  if not (is_declaration f) then (
    if !Lstar_config.abduction_flag then
      Format.fprintf logf "@[Abducing spec of function %s...@]%!@\n" id
    else
      Format.fprintf logf "@[Verifying function %s...@]%!@\n" id;
    let cfg_nodes = cfg_nodes_of_function specs f in
    let spec = spec_of_fun_id specs id in
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
    if !Lstar_config.abduction_flag then
      let specs = Symexec.bi_abduct id cfg_nodes spec_to_verify
	logic abduct_logic abstraction_rules in
      dump_specs_of_function id specs;
      specs <> []
    else
      Symexec.verify id cfg_nodes spec_to_verify logic abstraction_rules
  ) else true

let verify_module logic abduct_logic abstruction_rules specs m =
  set_source_name m;
  (* iter_globals env_add_gvar m; *) (* TODO: handle global variables *)
  let verif_fun = verify_function logic abduct_logic abstruction_rules specs in
  fold_left_functions (fun b f -> verif_fun f) true m

let go logic abduct_logic abs_rules spec_list m =
  llcontext := module_context m;
  lltarget := Llvm_target.TargetData.create (data_layout m);
  if log log_phase then
    Format.fprintf logf "Generating logic for the module@\n";
  let (module_logic, module_abduct_logic) = logic_of_module m in
  let logic = add_logic logic module_logic in
  let abduct_logic = add_logic abduct_logic module_abduct_logic in
  dump_logic_rules "logic_rules.txt" (logic.seq_rules);
  dump_rewrite_rules "rewrite_rules.txt" (logic.rw_rules);
  if !Lstar_config.abduction_flag then (
    dump_logic_rules "abduct_rules.txt" (abduct_logic.seq_rules)
  );
  verify_module logic abduct_logic abs_rules spec_list m
