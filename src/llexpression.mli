val ret_arg : Psyntax.args
val numargs : Psyntax.args -> Psyntax.args
val numargs_of_str : string -> Psyntax.args
val numargs_of_int : int -> Psyntax.args
val numargs_of_int64 : int64 -> Psyntax.args
val bvargs : Psyntax.args -> Psyntax.args -> Psyntax.args
val bvargs_of_str : string -> string -> Psyntax.args
val bvargs_of_int : int -> int -> Psyntax.args
val bvargs_of_int64 : int -> int64 -> Psyntax.args
val bvargs64_of_int : int64 -> int -> Psyntax.args
val bvargs64_of_int64 : int64 -> int64 -> Psyntax.args
val mkUndef : Psyntax.args -> Psyntax.args
val mkUndef64 : int64 -> Psyntax.args
val mkPointer : Psyntax.term -> Psyntax.term -> Psyntax.term -> Psyntax.form
val mkArray : Psyntax.term -> Psyntax.term -> Psyntax.term -> Psyntax.term -> Psyntax.term -> Psyntax.term -> Psyntax.form
val args_sizeof : Llvm.lltype -> Psyntax.args
val args_of_type : Llvm.lltype -> Psyntax.args
val args_of_int_const : Llvm.llvalue -> Psyntax.args
val args_of_const_expr : Llvm.llvalue -> Psyntax.args
val args_of_value : Llvm.llvalue -> Psyntax.args
val args_of_composite_value : string -> Llvm.llvalue -> Psyntax.args
val ppred_of_gep : Psyntax.term -> Llvm.lltype -> Psyntax.term -> Llvm.llvalue list -> Psyntax.form
