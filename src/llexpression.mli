val ret_arg : Psyntax.args
val numargs : Psyntax.args -> Psyntax.args
val numargs_of_str : string -> Psyntax.args
val numargs_of_int : int -> Psyntax.args
val numargs_of_int64 : int64 -> Psyntax.args
val bvargs_of_int : int -> int -> Psyntax.args
val bvargs_of_int64 : int -> int64 -> Psyntax.args
val bvargs64_of_int : int64 -> int -> Psyntax.args
val bvargs64_of_int64 : int64 -> int64 -> Psyntax.args
val mkUndef : Psyntax.args -> Psyntax.args
val mkUndef64 : int64 -> Psyntax.args
val mkPointer : Psyntax.term -> Psyntax.term -> Psyntax.term -> Psyntax.form
val mkPadding : Psyntax.term -> Psyntax.term -> Psyntax.form
val mkJumpEnd : Psyntax.args
val mkJump : Psyntax.args -> Psyntax.args -> Psyntax.args
val mkEltptr : Psyntax.args -> Psyntax.args -> Psyntax.args -> Psyntax.args
val mkIntegerType : int -> Psyntax.args
val mkFloatType : string -> Psyntax.args
val mkVoidType : Psyntax.args
val mkLabelType : Psyntax.args
val mkNamedType : Psyntax.args -> Psyntax.args
val mkStructType : Psyntax.args list -> Psyntax.args
val mkFunctionType : Psyntax.args list -> Psyntax.args -> Psyntax.args
val mkPointerType : Psyntax.args -> Psyntax.args
val mkVectorType : Psyntax.args -> Psyntax.args
val mkArrayType : Psyntax.args -> Psyntax.args -> Psyntax.args
val mkMDType : Psyntax.args

val mkI8Type : Psyntax.args
val mkI32Type : Psyntax.args
val mkI64Type : Psyntax.args
val mkVoidPointerType : Psyntax.args -> Psyntax.args
val mkValConversion : Psyntax.args -> Psyntax.args -> Psyntax.args -> Psyntax.args

val args_sizeof : Llvm.lltype -> Psyntax.args
val args_of_type : Llvm.lltype -> Psyntax.args
val args_of_int_const : Llvm.llvalue -> Psyntax.args
val args_of_op : Llvm.Opcode.t -> Llvm.llvalue -> Psyntax.args
val args_of_const_expr : Llvm.llvalue -> Psyntax.args
val args_of_value : Llvm.llvalue -> Psyntax.args
val args_of_composite_value : string -> Llvm.llvalue -> Psyntax.args
