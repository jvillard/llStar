(********************************************************
   This file is part of coreStar
        src/utils/config.mli
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   coreStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)


val log_specs : int
val log_phase : int
val log_load : int
val log_prove : int
val log_exec : int
val log_logic : int
val log_symb : int
val log_parse : int
val log_cfg : int
val log_smt : int
val log : int -> bool

val specs_template_mode : bool ref
val dotty_print : bool ref
val abs_int_join : unit -> bool
val solver_path : string ref
val smt_run : bool ref
val smt_custom_commands : string ref
val args_default : (string * Arg.spec * string) list
val eclipse_mode : unit -> bool
val abs_int_plugins : string list ref
val check_memleaks : bool ref
val outdir : string ref
