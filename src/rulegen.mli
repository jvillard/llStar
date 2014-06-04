(** generates the logic and the abduction logic of module [m] *)
val add_rules_of_module : ParserAst.node_decl list -> Core.rules -> Llvm.llmodule -> Core.rules
