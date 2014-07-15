open Format
(* coreStar modules *)
open Corestar_std
open Config
open Debug
open Core
(* llStar modules *)
open Llutils

let verify_module q m =
  if log log_phase then
    fprintf logf "Translating module to coreStar.@.";
  let q = Llfunction.question_of_llmodule q m in
  if log log_phase then
    fprintf logf "Verifying module. The mission starts now.@.";
  let result = Symexec.verify q in
  fprintf logf "@.@[Mama says %s@." (if result then "yes" else "no")

let initialise_llvm () =
  if log log_phase then
    fprintf logf "@[<2>Loading bitcode program %s@\n" !Llstar_config.bitcode_file_name;
  let ic = Llvm.create_context () in
  let imbuf = Llvm.MemoryBuffer.of_file !Llstar_config.bitcode_file_name in
  let llmod = Llvm_bitreader.parse_bitcode ic imbuf in
  if !Llstar_config.optimise_bc then (
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
  let fname = !Llstar_config.bitcode_base_name in
  if log log_phase then
    fprintf logf "@[Analysed bitcode in %s@]@\n" fname;
  ignore (Llvm_bitwriter.write_bitcode_file llmod fname);
  let llvm_dis_pid =
    if !Llstar_config.output_ll <> "" then (
      let llname = !Llstar_config.output_ll in
      if log log_phase then
	fprintf logf "@[ASCII version in %s@]@\n" llname;
      Some(Unix.create_process "llvm-dis-3.4" [|"llvm-dis-3.4";
						"-o"; llname;
						fname|] Unix.stdin Unix.stdout Unix.stderr)
    ) else None in
  llmodule := Some llmod;
  llcontext := Llvm.module_context llmod;
  lltarget := Llvm_target.DataLayout.of_string (Llvm.data_layout llmod);
  Llexpression.declare_struct_types_in_llmodule llmod;
  if log log_phase then fprintf logf "@]";
  (llvm_dis_pid,llmod)

let initialise_corestar llmod =
  if log log_phase then
    fprintf logf "@.@[<2>Setting up coreStar@\n";
  set_source_name llmod;
  (* TODO: restore
  let signals = (if Sys.os_type="Win32" then [] else [Sys.sigint; Sys.sigquit; Sys.sigterm]) in
  List.iter
    (fun s ->  Sys.set_signal s (Sys.Signal_handle (fun x -> Symexec.pp_dotty_transition_system (); exit x)))
    signals;*)
  (* Load abstract interpretation plugins *)
  (* TODO: restore
     List.iter (fun file_name -> Plugin_manager.load_plugin file_name) !Config.abs_int_plugins; *)
  if log log_phase then fprintf logf "@]"

let initialise_logic llmod =
  if log log_phase then
    fprintf logf "@.@[<2>Loading logic and specs@\n";
  let question_of_entries question xs =
    let add_abstraction r q =
      let q_rules = q.Core.q_rules in
      let abstraction = q_rules.Core.abstraction in
      let abstraction = abstraction @ [r] in
      let q_rules = { q_rules with Core.abstraction } in
      { q with Core.q_rules } in
    let add_calculus r q =
      let q_rules = q.Core.q_rules in
      let calculus = q_rules.Core.calculus in
      let calculus = calculus @ [r] in
      let q_rules = { q_rules with Core.calculus } in
      { q with Core.q_rules } in
    let f (q,n) = function
      | ParserAst.AbstractionRule r -> (add_abstraction r q, n)
      | ParserAst.CalculusRule r -> (add_calculus r q, n)
      | ParserAst.Procedure p ->
	({ q with Core.q_procs = q.Core.q_procs @ [p] }, n)
      | ParserAst.NodeDecl nd -> (q, n @ [nd]) in
    List.fold_right (flip f) xs (question,[]) in
  let path = System.getenv_dirlist (System.getenv "COREPATH") in
  let parse fn =
    System.parse_file Logic_parser.file Logic_lexer.token fn "core" in
  let load_file q fn =
    let xs = Load.load ~path parse fn in
    question_of_entries q xs in
  let q = CoreOps.empty_ast_question in
  let (q,nodes) = load_file q !Llstar_config.star_file_name in
  let rules =
    if !Llstar_config.auto_gen_struct_logic then
      (if log log_phase then
	  fprintf logf "@.@[<2>Generating logic for the module";
       Rulegen.add_rules_of_module nodes q.q_rules llmod
      ) else q.q_rules in
  let q = { q with
    q_rules = rules;
    q_infer = !Llstar_config.abduction_flag;
    q_name = "to be or not to be safe?" } in
  (* TODO: restore *)
  (* dump_into_file "logic_rules.txt" *)
  (*   (Debug.pp_list pp_sequent_rule) logic.seq_rules; *)
  (* dump_into_file "rewrite_rules.txt" *)
  (*   (Debug.pp_list pp_rewrite_rule) logic.rw_rules; *)
  (* if !Llstar_config.abduction_flag then *)
  (*   dump_into_file "abduct_rules.txt" *)
  (*     (Debug.pp_list pp_sequent_rule) abduct_logic.seq_rules; *)
  if log log_phase then fprintf logf "@]@\n";
  q

let finish_execution _ =
  prof_phase "shutdown";
  Prover.print_stats ();
  Smt.print_stats ();
  prof_print_stats ();
  printf "@}@}@?"; eprintf "@?";
  exit 0

(** run llStar *)
let go () =
  Llstar_config.parse_args ();
  let (wait_pid,llmod) = initialise_llvm () in
  initialise_corestar llmod;
  let q = initialise_logic llmod in
  verify_module q llmod;
  Llvm.dispose_module llmod;
  match wait_pid with
  | Some(pid) -> ignore (Unix.waitpid [] pid);
  | None -> ()

(** toplevel *)
let () =
  ignore (Sys.signal Sys.sigint (Sys.Signal_handle finish_execution));
  printf "@[@{<html>@{<head>@{<css>@}@{<encoding>@}@}@{<body>"; eprintf "@[";
  Config.verbosity := 3;
  go ();
  finish_execution 0
