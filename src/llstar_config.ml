(* known bug: llstar cannot analyse a file named /. *)
let impossible_file_name = "/."
let devnull = "/dev/null"

let outdir = ref (Sys.getcwd() ^ Filename.dir_sep ^ "_llstar")

let bitcode_file_name = ref impossible_file_name
let bitcode_base_name = ref ""
let output_ll = ref impossible_file_name
let logic_file_name = ref impossible_file_name
let spec_file_name = ref impossible_file_name
let absrules_file_name = ref impossible_file_name
let abductrules_file_name = ref impossible_file_name

let optimise_bc = ref false
let auto_gen_struct_logic = ref true
let auto_gen_list_logic = ref false
let abduction_flag = ref false

let set_file_name fnref fn =
  if not (Sys.file_exists fn) then raise (Arg.Bad ("File "^fn^" does not exist"));
  fnref := fn

let prog_regexp = Str.regexp "\\$prog\\$"

(** must be called *after* bitcode_*_name have been set (normally
    after Arg.parse) *)
let guess_spec_and_rules_files filenameref suffix =
  let rec try_prefix name =
    let fname = if name = "" then suffix else name^"."^suffix in
    try
      set_file_name filenameref fname;
      if Debug.log Debug.log_phase then
	Format.fprintf Debug.logf "@[Auto-picked %s@]@\n" fname;
    with Arg.Bad _ ->
      (* no file found. We'll try name.suffix if we were trying name.bla.suffix,
	 otherwise just suffix, otherwise devnull *)
      if name = "" then set_file_name filenameref devnull
      else try try_prefix (Filename.chop_extension name)
	with Invalid_argument _ -> try_prefix "" in
  if !filenameref = impossible_file_name then try_prefix !bitcode_base_name

let set_bool bref b = bref := b
  
let arg_list = Config.args_default @ [
  ("-l", Arg.String(set_file_name logic_file_name),
   "logic file name (default: $SOURCE.logic or logic)");
  ("-s", Arg.String(set_file_name spec_file_name),
   "spec file name (default: $SOURCE.spec or spec)");
  ("-a", Arg.String(set_file_name absrules_file_name),
   "abstraction rules file name (default: $SOURCE.abs or abs)");
  ("-abduct_file", Arg.String(set_file_name abductrules_file_name),
   "abduction rules file name (default: $SOURCE.abduct or abduct)");
  ("-abduct", Arg.Set(abduction_flag),
   "toggles abduction on");
  ("-autostructs", Arg.Set(auto_gen_struct_logic),
   "enable automatic struct-folding/unfolding rules generation");
  ("-autolists", Arg.Set(auto_gen_list_logic),
   "enable automatic list abstraction rules generation");
  ("-noautostructs", Arg.Clear(auto_gen_struct_logic),
   "disable automatic struct-folding/unfolding rules generation");
  ("-noautolists", Arg.Clear(auto_gen_list_logic),
   "disable automatic list abstraction rules generation");
  ("-outdir", Arg.Set_string(outdir),
   "directory where to output llStar results");
  ("-outputll", Arg.Set_string(output_ll),
   "output ASCII bitcode to specified file (leave empty to disable) (default: [outdir]/[bitcode_base_name].ll)");
  ("-runopts", Arg.Bool(set_bool optimise_bc),
   "run a few (hardcoded) LLVM optimisation passes before verification (default: false)");
]

let usage_msg = "Usage: llstar [options] source_file"

let set_bitcode_file_name_once s =
  if !bitcode_file_name != impossible_file_name then
    raise (Arg.Bad "More than one source file provided");
  bitcode_base_name := Filename.basename s;
  set_file_name bitcode_file_name s;
  let bitcode_chopped_name =
    try Filename.chop_extension !bitcode_base_name
    with Invalid_argument _ -> !bitcode_base_name in
  if (!output_ll = impossible_file_name) then
    output_ll := bitcode_chopped_name ^ ".ll"

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
  (* TODO: restore *)
  (* Config.outdir := !outdir; *)
  (* Config.source_file := !bitcode_file_name; *)
  (* Config.source_base_name := !bitcode_base_name; *)

  (* try to find the logic/abs/specs/... files if they haven't been given *)
  List.iter (fun (a,suffix) -> guess_spec_and_rules_files a suffix)
    [(logic_file_name, "logic");
     (absrules_file_name, "abs");
     (spec_file_name, "spec");
     (abductrules_file_name, "abduct")]
