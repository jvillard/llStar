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

let load_logic_rules_from_file fn =
  let l1,l2,cn = Load_logic.load_logic fn in
  {Psyntax.empty_logic with Psyntax.seq_rules=l1; Psyntax.rw_rules=l2; Psyntax.consdecl=cn}

let main () =
  Lstar_config.parse_args ();

  if log log_phase then
    fprintf logf "@[<2>Loading bitcode program %s@\n" !Lstar_config.bitcode_file_name;
  let ic = Llvm.create_context () in
  let imbuf = Llvm.MemoryBuffer.of_file !Lstar_config.bitcode_file_name in
  let llmod = Llvm_bitreader.parse_bitcode ic imbuf in

  if !Lstar_config.optimise_bc then (
    if log log_phase then
      fprintf logf "@[Running LLVM passes on bitcode@]@\n";
    let pm = Llvm.PassManager.create () in
    Llvm_scalar_opts.add_constant_propagation pm;
    Llvm_scalar_opts.add_dead_store_elimination pm;
    Llvm_scalar_opts.add_aggressive_dce pm;
    Llvm_scalar_opts.add_memory_to_register_promotion pm;
    Llvm_scalar_opts.add_ind_var_simplification pm;    
    Llvm_scalar_opts.add_correlated_value_propagation pm;
    Llvm_ipo.add_argument_promotion pm;
    Llvm_ipo.add_global_dce pm;
    Llvm_ipo.add_ipc_propagation pm;
    ignore (Llvm.PassManager.run_module llmod pm)
  );

  let fname = Filename.concat !Config.outdir !Lstar_config.bitcode_base_name in
  if log log_phase then
    fprintf logf "@[Analysed bitcode in %s@]@\n" fname;
  ignore (Llvm_bitwriter.write_bitcode_file llmod fname);

  let llvm_dis_pid =
    if !Lstar_config.output_ll <> "" then (
      let llname = Filename.concat !Config.outdir !Lstar_config.output_ll in
      if log log_phase then
	fprintf logf "@[ASCII version in %s@]@\n" llname;
      Some(Unix.create_process "llvm-dis-3.1" [|"llvm-dis-3.1";
						"-o"; llname;
						fname|] Unix.stdin Unix.stdout Unix.stderr)
    ) else None in

  if log log_phase then
    fprintf logf "@.@[<2>Setting up coreStar@\n";
  let signals = (if Sys.os_type="Win32" then [] else [Sys.sigint; Sys.sigquit; Sys.sigterm]) in
  List.iter
    (fun s ->  Sys.set_signal s (Sys.Signal_handle (fun x -> Symexec.pp_dotty_transition_system (); exit x)))
    signals;
  if !Config.smt_run then Smt.smt_init();
  (* Load abstract interpretation plugins *)
  List.iter (fun file_name -> Plugin_manager.load_plugin file_name) !Config.abs_int_plugins;       

  if log log_phase then
    fprintf logf "@.@[<2>Loading logic@\n";
  let logic = load_logic_rules_from_file !Lstar_config.logic_file_name in
  let abduct_logic = load_logic_rules_from_file !Lstar_config.abductrules_file_name in
  let abs_rules = load_logic_rules_from_file !Lstar_config.absrules_file_name in

  if log log_phase then
    fprintf logf "@.@[<2>Loading specs@\n";
  let spec_list = Load.import_flatten
    Cli_utils.specs_dirs            
    !Lstar_config.spec_file_name
    Logic_parser.spec_file Logic_lexer.token in

  if log log_phase then
    fprintf logf "@.@[Analysing module@\n";
  llcontext := module_context llmod;
  lltarget := Llvm_target.TargetData.create (data_layout llmod);
  if log log_phase then
    Format.fprintf logf "Generating logic for the module@\n";
  let (module_logic, module_abduct_logic) = logic_of_module llmod in
  let logic = add_logic logic module_logic in
  let abduct_logic = add_logic abduct_logic module_abduct_logic in
  dump_logic_rules "logic_rules.txt" (logic.seq_rules);
  dump_rewrite_rules "rewrite_rules.txt" (logic.rw_rules);
  if !Lstar_config.abduction_flag then (
    dump_logic_rules "abduct_rules.txt" (abduct_logic.seq_rules)
  );
  let verdict = verify_module logic abduct_logic abs_rules spec_list llmod in
  fprintf logf "@.@[Mama says %s@." (if verdict then "yes" else "no");
  Symexec.pp_dotty_transition_system ();
  Llvm.dispose_module llmod;
  match llvm_dis_pid with
  | Some(pid) -> ignore (Unix.waitpid [] pid);
  | None -> ()
  

let _ =
  System.set_signal_handlers ();
  let mf = {
    mark_open_tag = (function
      | "b" -> System.terminal_red (* bad *)
      | "g" -> System.terminal_green (* good *)
      | _ -> assert false);
    mark_close_tag = (fun _ -> System.terminal_white);
    print_open_tag = (fun _ -> ());
    print_close_tag = (fun _ -> ())} in
  set_formatter_tag_functions mf;
  pp_set_formatter_tag_functions err_formatter mf;
  set_tags true; pp_set_tags err_formatter true;
  main ()
