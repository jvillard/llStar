open Format
(* coreStar modules *)
open Config
open Debug
open Psyntax
(* LStar modules *)
open Llutils

let verify_function logic abduct_logic abstraction_rules specs f =
  let fid = value_id f in
  if not (Llvm.is_declaration f) then (
    if !Lstar_config.abduction_flag then
      fprintf logf "@[Abducing spec of function %s...@." fid
    else
      fprintf logf "@[Verifying function %s...@." fid;
    let cfg_nodes = Llfunction.cfg_nodes_of_function specs f in
    let spec = Logic_spec.spec_of_fun_id specs fid in
    (* we apply 2 substitutions to the spec *)
    (* subst 1: 
     * replace "@parameter%i%:" logical values with the function arguments *)
    let rec add_param_subst (i,subst) fun_param =
      let arg = Llexpression.args_of_value fun_param in
      let param = Vars.concretep_str ("@parameter"^(string_of_int i)^":") in
      (i+1, add_subst param arg subst) in
    let (_,subst) = Llvm.fold_left_params add_param_subst (0,empty) f in
    (* subst 2: 
     * apply the existential Hoare rule: make sure the
       existentials in the post match those of the pre of the same
       name. To achieve this, let's replace them by regular variables. *)
    let pre_ev = ev_form spec.Spec.pre in
    let subst = vs_fold
      (fun v subst ->
	let pvar = Arg_var (Vars.freshp_str (Vars.string_var v)) in
	add_subst v pvar subst) pre_ev subst in
    let spec_to_verify =
      { spec with
	Spec.pre = subst_form subst spec.Spec.pre;
	Spec.post = subst_form subst spec.Spec.post; } in
    Cfg_core.stmts_to_cfg cfg_nodes;
    Cfg_core.print_icfg_dotty [(cfg_nodes, fid)] fid;
    if !Lstar_config.abduction_flag then
      let specs = Symexec.bi_abduct fid cfg_nodes spec_to_verify
	logic abduct_logic abstraction_rules in
      dump_into_file (fid ^ ".specs") (Debug.pp_list pp_spec) specs;
      specs <> []
    else
      Symexec.verify fid cfg_nodes spec_to_verify logic abstraction_rules
  ) else true

let verify_module logic abduct_logic abstruction_rules specs m =
  (* iter_globals env_add_gvar m; *) (* TODO: handle global variables *)
  let verif_fun = verify_function logic abduct_logic abstruction_rules specs in
  let verdict = Llvm.fold_left_functions (fun b f -> b && verif_fun f) true m in
  fprintf logf "@.@[Mama says %s@." (if verdict then "yes" else "no")

let initialise_llvm () =
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
  llcontext := Llvm.module_context llmod;
  lltarget := Llvm_target.TargetData.create (Llvm.data_layout llmod);
  if log log_phase then fprintf logf "@]";
  (llvm_dis_pid,llmod)

let initialise_corestar llmod =
  if log log_phase then
    fprintf logf "@.@[<2>Setting up coreStar@\n";
  set_source_name llmod;
  let signals = (if Sys.os_type="Win32" then [] else [Sys.sigint; Sys.sigquit; Sys.sigterm]) in
  List.iter
    (fun s ->  Sys.set_signal s (Sys.Signal_handle (fun x -> Symexec.pp_dotty_transition_system (); exit x)))
    signals;
  if !Config.smt_run then Smt.smt_init();
  (* Load abstract interpretation plugins *)
  List.iter (fun file_name -> Plugin_manager.load_plugin file_name) !Config.abs_int_plugins;
  if log log_phase then fprintf logf "@]"

let initialise_logic llmod =
  if log log_phase then
    fprintf logf "@.@[<2>Loading logic and specs@\n";
  let load_logic_rules_from_file fn =
    let l1,l2,cn = Load_logic.load_logic fn in
    { empty_logic with seq_rules=l1; rw_rules=l2; consdecl=cn} in
  let logic = load_logic_rules_from_file !Lstar_config.logic_file_name in
  let abduct_logic = load_logic_rules_from_file !Lstar_config.abductrules_file_name in
  let abs_rules = load_logic_rules_from_file !Lstar_config.absrules_file_name in
  let spec_list = Load.import_flatten
    Cli_utils.specs_dirs            
    !Lstar_config.spec_file_name
    Logic_parser.spec_file Logic_lexer.token in
  if log log_phase then
    fprintf logf "@.@[<2>Generating logic for the module";
  let (module_logic, module_abduct_logic) = Rulegen.logic_of_module llmod in
  let logic = add_logic logic module_logic in
  let abduct_logic = add_logic abduct_logic module_abduct_logic in
  dump_into_file "logic_rules.txt"
    (Debug.pp_list pp_sequent_rule) logic.seq_rules;
  dump_into_file "rewrite_rules.txt"
    (Debug.pp_list pp_rewrite_rule) logic.rw_rules;
  if !Lstar_config.abduction_flag then
    dump_into_file "abduct_rules.txt"
      (Debug.pp_list pp_sequent_rule) abduct_logic.seq_rules;
  if log log_phase then fprintf logf "@]@\n";
  (logic,abduct_logic,abs_rules,spec_list)

(** run LStar *)
let go () =
  Lstar_config.parse_args ();
  let (wait_pid,llmod) = initialise_llvm () in
  initialise_corestar llmod;
  let (logic,abduct_logic,abs_rules,spec_list) = initialise_logic llmod in
  verify_module logic abduct_logic abs_rules spec_list llmod;
  Symexec.pp_dotty_transition_system ();
  Llvm.dispose_module llmod;
  match wait_pid with
  | Some(pid) -> ignore (Unix.waitpid [] pid);
  | None -> ()

(** toplevel *)
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
  go ()
