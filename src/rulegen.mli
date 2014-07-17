val gen_struct_rule : Z3.Expr.expr -> Z3.Expr.expr -> Calculus.rule_schema -> Calculus.rule_schema list

(** generates the logic and the abduction logic of module [m] *)
val add_rules_of_module : Core.rules -> Llvm.llmodule -> Core.rules
