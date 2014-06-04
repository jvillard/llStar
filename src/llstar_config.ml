(* I hope this is impossible indeed *)
let impossible_file_name = "//"
let devnull = "/dev/null"

let outdir = ref (Sys.getcwd() ^ Filename.dir_sep ^ "_llstar")

let bitcode_file_name = ref impossible_file_name
let bitcode_base_name = ref ""
let output_ll = ref impossible_file_name
let star_file_name = ref impossible_file_name

let optimise_bc = ref false
let auto_gen_struct_logic = ref true
let auto_gen_list_logic = ref false
let abduction_flag = ref false

let set_file_name fnref fn =
  if not (Sys.file_exists fn) then raise (Arg.Bad ("File "^fn^" does not exist"));
  fnref := fn

(** must be called *after* bitcode_*_name have been set (normally
    after Arg.parse) *)
let guess_star_file () =
  let rec try_prefix name =
    let fname = if name = "" then
	Filename.concat (Filename.dirname !bitcode_file_name) "star"
      else name^".star" in
    try
      set_file_name star_file_name fname;
      if Debug.log Debug.log_phase then
	Format.fprintf Debug.logf "@[Auto-picked %s@]@\n" fname;
    with Arg.Bad _ ->
      (* no file found. We'll try name.star if we were trying name.bla.star,
	 otherwise just star, otherwise devnull *)
      if name = "" then set_file_name star_file_name devnull
      else try try_prefix (Filename.chop_extension name)
	with Invalid_argument _ -> try_prefix "" in
  if !star_file_name = impossible_file_name then try_prefix !bitcode_file_name

let set_bool bref b = bref := b
  
let arg_list = Config.args_default @ [
  ("-s", Arg.String(set_file_name star_file_name),
   "spec file name (default: $SOURCE.spec or spec)");
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
  ("-outputll", Arg.Set_string(output_ll),
   "output ASCII bitcode to specified file (leave empty to disable) (default: [bitcode_base_name].ll)");
  ("-runopts", Arg.Bool(set_bool optimise_bc),
   "run a few (hardcoded) LLVM optimisation passes before verification (default: false)");
]

let usage_msg = "Usage: llstar [options] source_file"

let set_bitcode_file_name_once s =
  if !bitcode_file_name != impossible_file_name then
    raise (Arg.Bad "More than one source file provided");
  set_file_name bitcode_file_name s;
  bitcode_base_name := Filename.basename s;
  let bitcode_chopped_name =
    try Filename.chop_extension !bitcode_base_name
    with Invalid_argument _ -> !bitcode_base_name in
  if (!output_ll = impossible_file_name) then
    output_ll := bitcode_chopped_name ^ ".ll"

(** parse command line arguments *)
let parse_args () =
  Arg.parse arg_list set_bitcode_file_name_once usage_msg;
  if !bitcode_file_name = impossible_file_name then
    Arg.usage arg_list usage_msg;
  (* set up a few coreStar config variables *)
  (* TODO: restore *)
  (* Config.source_file := !bitcode_file_name; *)
  (* Config.source_base_name := !bitcode_base_name; *)

  (* try to find the star file if they haven't been given *)
  guess_star_file ()
