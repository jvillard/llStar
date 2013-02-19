val outdir: string ref

val program_file_name: string ref
val logic_file_name: string ref
val spec_file_name: string ref
val absrules_file_name: string ref
val abductrules_file_name: string ref

val auto_gen_list_logic: bool ref
val abduction_flag: bool ref

(** parse command line arguments *)
val parse_args: unit -> unit
