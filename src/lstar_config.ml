let program_file_name = ref ""
let logic_file_name = ref "logic"
let spec_file_name = ref "specs"
let absrules_file_name = ref "abs"
let abductrules_file_name = ref "../../logic/abduct.logic"

let auto_gen_list_logic = ref false
let abduction_flag = ref false

let arg_list = Config.args_default @ [
  ("-f", Arg.Set_string(program_file_name), "program file name");
  ("-l", Arg.Set_string(logic_file_name), "logic file name");
  ("-s", Arg.Set_string(spec_file_name), "spec file name");
  ("-a", Arg.Set_string(absrules_file_name), "abstraction rules file name");
  ("-abduct_file", Arg.Set_string(abductrules_file_name), "abduction rules file name");
  ("-abduct", Arg.Set(abduction_flag), "toggles abduction on");
  ("-lists", Arg.Set(auto_gen_list_logic), "toggles automatic list abstraction rules generation");
]

let usage_msg = "Usage: -l <logic_file_name>  "^
  "-a <abstraction_file_name>  -s <spec_file_name>  "^
  "-f <bitcode_program> -abduct_file <abduction_file_name> -abduct -lists"
