(* known bug: lstar cannot analyse a file named /. *)
let impossible_file_name = "/."
let default_logic_files = ["logic"; "$prog$.logic"]
let default_spec_files = ["spec"; "$prog$.spec"]
let default_absrules_files = ["abs"; "$prog$.abs"]
let default_abductrules_files = ["abduct"; "$prog$.abduct"]
let devnull = "/dev/null"

let outdir = ref (Sys.getcwd() ^ Filename.dir_sep ^ "_lstar")

let bitcode_file_name = ref impossible_file_name
let bitcode_base_name = ref ""
let bitcode_chopped_name = ref ""
let output_ll = ref impossible_file_name
let logic_file_name = ref impossible_file_name
let spec_file_name = ref impossible_file_name
let absrules_file_name = ref impossible_file_name
let abductrules_file_name = ref impossible_file_name

let optimise_bc = ref true
let auto_gen_list_logic = ref false
let abduction_flag = ref false

let set_file_name fnref fn =
  if not (Sys.file_exists fn) then raise (Arg.Bad ("File "^fn^" does not exist"));
  fnref := fn

let prog_regexp = Str.regexp "\\$prog\\$"

(** must be called *after* bitcode_*_name have been set (normally
    after Arg.parse) *)
let set_file_name_with_defaults fnref defaults =
  let rec aux = function
    | [] -> fnref := devnull
    | defn::tl ->
      let fname = Str.global_replace prog_regexp !bitcode_chopped_name defn in
      try
	set_file_name fnref fname;
	if Config.log Config.log_phase then
	  Format.fprintf Debug.logf "@[Auto-picked %s@]@\n" fname;
      with Arg.Bad _ -> aux tl in
  if !fnref = impossible_file_name then aux defaults

let set_bool bref b = bref := b
  
let arg_list = Config.args_default @ [
  ("-l", Arg.String(set_file_name logic_file_name),
   "logic file name (default: "^List.hd default_logic_files^")");
  ("-s", Arg.String(set_file_name spec_file_name),
   "spec file name (default: "^List.hd default_spec_files^")");
  ("-a", Arg.String(set_file_name absrules_file_name),
   "abstraction rules file name (default: "^List.hd default_absrules_files^")");
  ("-abduct_file", Arg.String(set_file_name abductrules_file_name),
   "abduction rules file name (default: "^List.hd default_abductrules_files^")");
  ("-abduct", Arg.Set(abduction_flag),
   "toggles abduction on");
  ("-lists", Arg.Set(auto_gen_list_logic),
   "toggles automatic list abstraction rules generation");
  ("-outdir", Arg.Set_string(outdir),
   "directory where to output LStar results");
  ("-outputll", Arg.Set_string(output_ll),
   "output ASCII bitcode to specified file (leave empty to disable) (default: [outdir]/[bitcode_base_name].ll)");
  ("-runopts", Arg.Bool(set_bool optimise_bc),
   "run some (hardcoded) LLVM optimisations on the bitcode to ease verification (default: true)");
]

let usage_msg = "Usage: lstar [options] source_file"

let set_bitcode_file_name_once s =
  if !bitcode_file_name != impossible_file_name then
    raise (Arg.Bad "More than one source file provided");
  bitcode_base_name := Filename.basename s;
  let fname = Filename.concat (Sys.getcwd()) !bitcode_base_name in
  set_file_name bitcode_file_name fname;
  bitcode_chopped_name := (
    try Filename.chop_extension !bitcode_base_name
    with Invalid_argument _ -> !bitcode_base_name);
  if (!output_ll = "/.") then
    output_ll := !bitcode_chopped_name ^ ".ll"

(** parse command line arguments *)
let parse_args () =
  (* before parsing the arguments, try to guess some of them for coreStar *)
  if (System.getenv "JSTAR_SMT_PATH") = "" then
    Unix.putenv "JSTAR_SMT_PATH" "z3";
  if (System.getenv "JSTAR_SMT_ARGUMENTS") = "" then
    Unix.putenv "JSTAR_SMT_ARGUMENTS" "-in -smt2";

  Arg.parse arg_list set_bitcode_file_name_once usage_msg;
  if !bitcode_file_name = impossible_file_name then
    Arg.usage arg_list usage_msg;
  (* this is a race condition: the file could be removed between the
     time we check whether it exists or not and the time we check whether
     it's a directory or not... *)
  if not ((Sys.file_exists !outdir) && (Sys.is_directory !outdir)) then
    Unix.mkdir !outdir 0o755; (* perm = rwxr-xr-x *)
  (* set up a few coreStar config variables *)
  Config.outdir := !outdir;
  Config.source_file := !bitcode_file_name;
  Config.source_base_name := !bitcode_base_name;

  (* try to find the logic/abs/specs/... files if they haven't been given *)
  List.iter (fun (a,b) -> set_file_name_with_defaults a b)
    [(logic_file_name, default_logic_files);
     (absrules_file_name, default_absrules_files);
     (spec_file_name, default_spec_files);
     (abductrules_file_name, default_abductrules_files)]
