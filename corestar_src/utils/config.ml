(********************************************************
   This file is part of coreStar
        src/utils/config.ml
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   coreStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)


(* In this file we can put all global flags *)

(** Flag for empty creating specs template *)
let specs_template_mode = ref false

(** Flag to print heaps on every node in the cfg *)
let dotty_print = ref false


let log_exec = 1 lsl 0
let log_load = 1 lsl 1
let log_logic = 1 lsl 2
let log_phase = 1 lsl 3
let log_prove = 1 lsl 4
let log_specs = 1 lsl 5
let log_symb = 1 lsl 6
let log_parse = 1 lsl 7
let log_cfg = 1 lsl 8
let log_smt = 1 lsl 9

let log_active = ref(0)
  (* -1 means all, 0 means one, in general use lor *)

let log x = !log_active land x <> 0

let set_verbose () =
  log_active := !log_active lor log_phase lor log_prove

let smt_run = ref true 
let solver_path = ref ""
let smt_custom_commands = ref ""

let abs_int_join_ref = ref false
let abs_int_join() = !abs_int_join_ref

let eclipse_mode_ref = ref false
let eclipse_mode() = !eclipse_mode_ref

(** for compatibility *)
let set_debug_char (c : char) : unit = 
  match c with 
  | 'p' -> log_active := !log_active lor log_parse
  | 's' -> log_active := !log_active lor log_symb
  | 'c' -> log_active := !log_active lor log_cfg
  | 'm' -> log_active := !log_active lor log_smt
  | _ -> () 

let set_log = function
  | "exec" -> log_active := !log_active lor log_exec
  | "load" -> log_active := !log_active lor log_load
  | "logic" -> log_active := !log_active lor log_logic
  | "phase" -> log_active := !log_active lor log_phase
  | "prove" -> log_active := !log_active lor log_prove
  | "specs" -> log_active := !log_active lor log_specs
  | "symb" -> log_active := !log_active lor log_symb
  | "parse" -> log_active := !log_active lor log_parse
  | "cfg" -> log_active := !log_active lor log_cfg
  | "smt" -> log_active := !log_active lor log_smt
  | _ -> raise (Arg.Bad "No such logging mode.")

let clear_logs () =
  log_active := 0

let abs_int_plugins = ref []
let set_abs_int_plugins (comma_sep_lis : string) : unit = 
  abs_int_plugins := Str.split (Str.regexp ":") comma_sep_lis

let check_memleaks = ref true
let set_bool bref b = bref := b

let outdir = ref (Sys.getcwd())

let args_default = [
("-q", Arg.Unit(clear_logs), "Run in quiet mode" );
("-v", Arg.Unit(set_verbose), "Verbose proofs");
("-log", Arg.String(set_log), "Set log modes (exec,load,logic,phase,prove,specs,symb,parse,cfg,smt)");
("-nosmt", Arg.Clear(smt_run),"Don't use the SMT solver");
("-p", Arg.Set_string(solver_path), "SMT solver path");
("-b", Arg.Set_string(smt_custom_commands), "Background predicate");
("-ai", Arg.String(set_abs_int_plugins), "Colon separated list of AI plugins filenames");
("-join", Arg.Set(abs_int_join_ref), "On abstraction join heaps over their numeric part");
("-memleaks", Arg.Bool(set_bool check_memleaks), "Checks for memory leaks (default: true)");
("-d", Arg.String(String.iter set_debug_char), "Set debug modes (deprecated, see -log)");
]
