open Debug
open Format

let load_logic_rules_from_file fn =
  let l1,l2,cn = Load_logic.load_logic fn in
  {Psyntax.empty_logic with Psyntax.seq_rules=l1; Psyntax.rw_rules=l2; Psyntax.consdecl=cn}

let main () =
  Lstar_config.parse_args ();

  if log log_phase then
    fprintf logf "@[Loading bitcode program %s.@." !Lstar_config.program_file_name;
  let ic = Llvm.create_context () in
  let imbuf = Llvm.MemoryBuffer.of_file !Lstar_config.program_file_name in
  let im = Llvm_bitreader.parse_bitcode ic imbuf in

  if !Lstar_config.optimise_bc then (
    if log log_phase then
      fprintf logf "@[Running LLVM passes on bitcode.@.";
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
    ignore (Llvm.PassManager.run_module im pm)
  );

  let fname = Filename.concat !Config.outdir !Lstar_config.program_base_name in
  if log log_phase then
    fprintf logf "@[Outputting analysed bitcode in %s.@." fname;
  ignore (Llvm_bitwriter.write_bitcode_file im fname);

  let llvm_dis_pid =
    if !Lstar_config.output_ll <> "" then (
      let llname = Filename.concat !Config.outdir !Lstar_config.output_ll in
      if log log_phase then
	fprintf logf "@[Outputting ASCII version in %s.@." llname;
      Some(Unix.create_process "llvm-dis-3.1" [|"llvm-dis-3.1";
						"-o"; llname;
						fname|] Unix.stdin Unix.stdout Unix.stderr)
    ) else None in

  if log log_phase then
    fprintf logf "@[Setting up coreStar.@.";
  let signals = (if Sys.os_type="Win32" then [] else [Sys.sigint; Sys.sigquit; Sys.sigterm]) in
  List.iter
    (fun s ->  Sys.set_signal s (Sys.Signal_handle (fun x -> Symexec.pp_dotty_transition_system (); exit x)))
    signals;
  if !Config.smt_run then Smt.smt_init();
  (* Load abstract interpretation plugins *)
  List.iter (fun file_name -> Plugin_manager.load_plugin file_name) !Config.abs_int_plugins;       

  if log log_phase then
    fprintf logf "@[Loading logic.@.";
  let logic = load_logic_rules_from_file !Lstar_config.logic_file_name in
  let abduct_logic = load_logic_rules_from_file !Lstar_config.abductrules_file_name in
  let abs_rules = load_logic_rules_from_file !Lstar_config.absrules_file_name in

  if log log_phase then
    fprintf logf "@[Loading specs.@.";
  let spec_list = Load.import_flatten
    Cli_utils.specs_dirs            
    !Lstar_config.spec_file_name
    Logic_parser.spec_file Logic_lexer.token in

  if log log_phase then
    fprintf logf "@[Verifying.@.";
  let verdict = Verify_llvm.go logic abduct_logic abs_rules spec_list im in
  print_string ("\nmama says "^(if verdict then "yes" else "no")^"\n");
  Symexec.pp_dotty_transition_system ();
  Llvm.dispose_module im;
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
