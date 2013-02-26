(** generates the logic and the abduction logic of module [m] *)
val logic_of_module : Llvm.llmodule -> Psyntax.logic * Psyntax.logic
