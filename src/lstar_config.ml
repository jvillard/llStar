(* known bug: lstar cannot analyse a file named /. *)
let impossible_file_name = "/."
let default_logic_file = "logic"
let default_spec_file = "specs"
let default_absrules_file = "abs"
let default_abductrules_file = "../../logic/abduct.logic"

let program_file_name = ref impossible_file_name
let logic_file_name = ref default_logic_file
let spec_file_name = ref default_spec_file
let absrules_file_name = ref default_absrules_file
let abductrules_file_name = ref default_abductrules_file

let auto_gen_list_logic = ref false
let abduction_flag = ref false

let set_filename fnref fn =
  if not (Sys.file_exists fn) then raise (Arg.Bad "File does not exist");
  fnref := fn

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
]

let usage_msg = "Usage: lstar [options] source_file"

let set_program_file_name_once s =
  if !program_file_name = impossible_file_name then
    set_filename program_file_name s
  else
    raise (Arg.Bad "More than one source file provided")

let parse_args () =
  Arg.parse arg_list set_program_file_name_once usage_msg;
  if !program_file_name = impossible_file_name then
    Arg.usage arg_list usage_msg;
