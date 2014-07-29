val struct_rule : string -> string -> Calculus.rule_schema -> Calculus.rule_schema list
val polymorphic_rule : string list -> Calculus.rule_schema -> Calculus.rule_schema list

(** generates the logic and the abduction logic of module [m] *)
val add_rules_of_module : Core.rules -> Llvm.llmodule -> Core.rules
