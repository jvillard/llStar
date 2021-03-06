(*** Uncomment this when llStar segfaults: it's most likely that the
     bindings are used incorrectly *)
(*
val type_of : Llvm.llvalue -> Llvm.lltype
val classify_value : Llvm.llvalue -> Llvm.ValueKind.t
val instr_opcode : Llvm.llvalue -> Llvm.Opcode.t
val operand : Llvm.llvalue -> int -> Llvm.llvalue
*)

val warn : string -> unit
val implement_this : string -> 'a

(** used to catch metadata information in values *)
exception MetaData of Llvm.llvalue

val llmodule : Llvm.llmodule option ref
val llcontext : Llvm.llcontext ref
val lltarget : Llvm_target.DataLayout.t ref
val get_llmodule : unit -> Llvm.llmodule

(* val classify_value : Llvm.llvalue -> Llvm.ValueKind.t *)

val lltype_of_name : string -> Llvm.lltype
(** gets names of named and unnamed variables *)
val value_id : Llvm.llvalue -> string
(** extract the location of instruction [instr] from the debug information *)
val location_of_instr : Llvm.llvalue -> Printing.source_location option
(** extract and record the original file name for module [m] *)
val set_source_name : Llvm.llmodule -> unit
(** extract a block's label *)
val label_of_bblock : Llvm.llbasicblock -> string
(** collects all the types referred to by the functions of module m *)
val collect_types_in_module : Llvm.llmodule -> Llvm.lltype list
(** dump things into files in the output directory *)
val dump_into_file : string -> (Format.formatter -> 'a -> unit) -> 'a -> unit

val string_of_struct : Llvm.lltype -> string

val mk_0 : 'b -> 'a list -> 'b
val mk_1 : ('a -> 'b) -> 'a list -> 'b
val mk_2 : ('a -> 'a -> 'b) -> 'a list -> 'b
val mk_3 : ('a -> 'a -> 'a -> 'b) -> 'a list -> 'b
