(* known bug: lstar cannot analyse a file named /. *)
let impossible_file_name = "/."
let default_logic_file = "logic"
let default_spec_file = "specs"
let default_absrules_file = "abs"
let default_abductrules_file = "../../logic/abduct.logic"

let outdir = ref (Sys.getcwd() ^ Filename.dir_sep ^ "_lstar")

let program_file_name = ref impossible_file_name
let program_base_name = ref ""
let output_ll = ref "/."
let logic_file_name = ref default_logic_file
let spec_file_name = ref default_spec_file
let absrules_file_name = ref default_absrules_file
let abductrules_file_name = ref default_abductrules_file

let optimise_bc = ref true
let auto_gen_list_logic = ref false
let abduction_flag = ref false

let set_filename fnref fn =
  if not (Sys.file_exists fn) then raise (Arg.Bad "File does not exist");
  fnref := fn

let set_bool bref b = bref := b

let arg_list = Config.args_default @ [
  ("-l", Arg.String(set_filename logic_file_name),
   "logic file name (default: "^default_logic_file^")");
  ("-s", Arg.String(set_filename spec_file_name),
   "spec file name (default: "^default_spec_file^")");
  ("-a", Arg.String(set_filename absrules_file_name),
   "abstraction rules file name (default: "^default_absrules_file^")");
  ("-abduct_file", Arg.String(set_filename abductrules_file_name),
   "abduction rules file name (default: "^default_abductrules_file^")");
  ("-abduct", Arg.Set(abduction_flag),
   "toggles abduction on");
  ("-lists", Arg.Set(auto_gen_list_logic),
   "toggles automatic list abstraction rules generation");
  ("-outdir", Arg.Set_string(outdir),
   "directory where to output LStar results");
  ("-outputll", Arg.Set_string(output_ll),
   "output ASCII bitcode to specified file (leave empty to disable) (default: [outdir]/[program_base_name].ll");
  ("-runopts", Arg.Bool(set_bool optimise_bc),
   "run some (hardcoded) LLVM optimisations on the bitcode to ease verification (default: true)");
]

let usage_msg = "Usage: lstar [options] source_file"

let set_program_file_name_once s =
  if !program_file_name != impossible_file_name then
    raise (Arg.Bad "More than one source file provided");
  set_filename program_file_name s;
  program_base_name := Filename.basename s;
  if (!output_ll = "/.") then
    output_ll := !program_base_name ^ ".ll"

(** parse command line arguments *)
let parse_args () =
  (* before parsing the arguments, try to guess some of them for coreStar *)
  if (System.getenv "JSTAR_SMT_PATH") = "" then
    Unix.putenv "JSTAR_SMT_PATH" "z3";
  if (System.getenv "JSTAR_SMT_ARGUMENTS") = "" then
    Unix.putenv "JSTAR_SMT_ARGUMENTS" "-in -smt2";

  Arg.parse arg_list set_program_file_name_once usage_msg;
  if !program_file_name = impossible_file_name then
    Arg.usage arg_list usage_msg;
  (* this is a race condition: the file could be removed between the
     time we check whether it exists or not and the time we check whether
     it's a directory or not... *)
  if not ((Sys.file_exists !outdir) && (Sys.is_directory !outdir)) then
    Unix.mkdir !outdir 0o755; (* perm = rwxr-xr-x *)
  (* set up a few coreStar config variables *)
  Config.outdir := !outdir;
  Symexec.file := Filename.basename (!program_file_name);
