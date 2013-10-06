(** generates the logic and the abduction logic of module [m] *)
val add_logic_of_module : Psyntax.logic * Psyntax.logic -> Llvm.llmodule -> Psyntax.logic * Psyntax.logic
