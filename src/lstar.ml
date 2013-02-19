open Debug
open Format
open Lstar_config

let load_logic_rules_from_file fn =
  let l1,l2,cn = Load_logic.load_logic fn in
  {Psyntax.empty_logic with Psyntax.seq_rules=l1; Psyntax.rw_rules=l2; Psyntax.consdecl=cn}

let main () =
  parse_args ();

  if log log_phase then
    fprintf logf "@[Loading bitcode program.@.";
  let ic = Llvm.create_context () in
  let imbuf = Llvm.MemoryBuffer.of_file !program_file_name in
  let im = Llvm_bitreader.parse_bitcode ic imbuf in

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
  let logic = load_logic_rules_from_file !logic_file_name in
  let abduct_logic = load_logic_rules_from_file !abductrules_file_name in
  let abs_rules = load_logic_rules_from_file !absrules_file_name in

  if log log_phase then
    fprintf logf "@[Loading specs.@.";
  let spec_list = Load.import_flatten
    Cli_utils.specs_dirs            
    !spec_file_name
    Logic_parser.spec_file Logic_lexer.token in

  if log log_phase then
    fprintf logf "@[Verifying.@.";
  let verdict = Verify_llvm.go logic abduct_logic abs_rules spec_list im in
  print_string ("\nmama says "^(if verdict then "yes" else "no")^"\n");
  Symexec.pp_dotty_transition_system ();
  Llvm.dispose_module im

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
