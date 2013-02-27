val ret_arg : Psyntax.args
val args_int : int -> Psyntax.args
val args_int64 : int64 -> Psyntax.args
val args_num : string -> Psyntax.args
val args_num_0 : Psyntax.args
val args_num_1 : Psyntax.args
val mkPointer : Psyntax.term -> Psyntax.term -> Psyntax.term -> Psyntax.form
val mkArray : Psyntax.term -> Psyntax.term -> Psyntax.term -> Psyntax.term -> Psyntax.term -> Psyntax.term -> Psyntax.form
val args_sizeof : Llvm_target.TargetData.t -> Llvm.lltype -> Psyntax.args
val args_of_type : Llvm.lltype -> Psyntax.args
val args_of_int_const : Llvm.llvalue -> Psyntax.args
val args_of_const_expr : Llvm.llvalue -> Psyntax.args
val args_of_value : Llvm.llvalue -> Psyntax.args
val args_of_composite_value : string -> Llvm.llvalue -> Psyntax.args
val ppred_of_gep : Psyntax.term -> Llvm.lltype -> Psyntax.term -> Llvm.llvalue list -> Psyntax.form
