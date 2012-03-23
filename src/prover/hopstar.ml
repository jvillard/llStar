open Printf
open Format
open Llvm
open Syntax.LLVMsyntax
open Cfg_core
(*open Verif_llvm*)

let program_file_name = ref ""
let logic_file_name = ref "logic"
let spec_file_name = ref "specs"
let absrules_file_name = ref "abs"

let set_logic_file_name n =
  logic_file_name := n

let set_spec_file_name n =
  spec_file_name := n

let set_absrules_file_name n =
  absrules_file_name := n

let set_program_file_name n =
  program_file_name := n

let arg_list = Config.args_default @ [
  ("-f", Arg.String(set_program_file_name ), "program file name");
  ("-l", Arg.String(set_logic_file_name ), "logic file name");
  ("-s", Arg.String(set_spec_file_name ), "spec file name");
  ("-a", Arg.String(set_absrules_file_name ), "abstraction rules file name");
]

let main () =
  let usage_msg = "Usage: -l <logic_file_name>  "^
    "-a <abstraction_file_name>  -s <spec_file_name>  "^
    "-f <class_file_program>" in
  Arg.parse
    arg_list
    (fun s -> Format.eprintf "WARNING: Ignored argument %s.@." s)
    usage_msg;

  if !program_file_name="" then
    failwith ("Program file name not specified. Can't continue....\n"^usage_msg^"\n");

  let ic = create_context () in
  let imbuf = MemoryBuffer.of_file !program_file_name in
  let im = Llvm_bitreader.parse_bitcode ic imbuf in
  let ist = SlotTracker.create_of_module im in

  Llvm_pretty_printer.travel_module ist im;
  let coqim = Llvm2coq.translate_module false ist im in
  (* Coq_pretty_printer.travel_module coqim; *)

  if !logic_file_name="" && not !Config.specs_template_mode then
    eprintf "@\nLogic file name not specified. Can't continue....\n %s \n" usage_msg
  else if !spec_file_name="" && not !Config.specs_template_mode then
    eprintf "@\nSpecification file name not specified. Can't continue....\n %s \n" usage_msg
  else if !absrules_file_name="" && not !Config.specs_template_mode then
    eprintf "@\nAbstraction rules file name not specified. Can't continue....\n %s \n" usage_msg
  else (
    let signals = (if Sys.os_type="Win32" then [] else [Sys.sigint; Sys.sigquit; Sys.sigterm]) in
    List.iter
      (fun s ->  Sys.set_signal s (Sys.Signal_handle (fun x -> Symexec.pp_dotty_transition_system (); exit x)))
      signals;
    if !Config.smt_run then Smt.smt_init();
      (* Load abstract interpretation plugins *)
    List.iter (fun file_name -> Plugin_manager.load_plugin file_name) !Config.abs_int_plugins;       

    let l1,l2,cn = Load_logic.load_logic !logic_file_name in
    let logic = {Psyntax.empty_logic with Psyntax.seq_rules=l1; Psyntax.rw_rules=l2; Psyntax.consdecl=cn} in

    let l1,l2,cn = Load_logic.load_abstractions !absrules_file_name in
    let abs_rules = {Psyntax.empty_logic with Psyntax.seq_rules=l1; Psyntax.rw_rules=l2; Psyntax.consdecl=cn} in

    let spec_list = [] in

    let verdict = Verif_llvm.verif_module logic abs_rules spec_list coqim in
    print_string ("mama says "^(if verdict then "yes" else "no")^"\n");
    Symexec.pp_dotty_transition_system ());
    
  SlotTracker.dispose ist;
  dispose_module im


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
