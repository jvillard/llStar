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
  ("-f", Arg.String(set_program_file_name), "program file name");
  ("-l", Arg.String(set_logic_file_name), "logic file name");
  ("-s", Arg.String(set_spec_file_name), "spec file name");
  ("-a", Arg.String(set_absrules_file_name), "abstraction rules file name");
]

let usage_msg = "Usage: -l <logic_file_name>  "^
  "-a <abstraction_file_name>  -s <spec_file_name>  "^
  "-f <class_file_program> "^
  "-lists <bool>"
