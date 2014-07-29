val change_sort_of_vars : Z3.Sort.sort -> Z3.Sort.sort -> Z3.Expr.expr -> Z3.Expr.expr

val declare_struct_types_in_llmodule : Llvm.llmodule -> unit

val bool_sort : Z3.Sort.sort
val int_sort : Z3.Sort.sort
val bv_sort : int -> Z3.Sort.sort
val void_sort : Z3.Sort.sort
val bblabel_sort : Z3.Sort.sort
val lltype_sort : Z3.Sort.sort
val jump_sort : Z3.Sort.sort
val md_sort : Z3.Sort.sort
val pointer_sort : Z3.Sort.sort
val size_sort : Z3.Sort.sort
val idx_sort : Z3.Sort.sort
val function_sort : Z3.Sort.sort list -> Z3.Sort.sort -> Z3.Sort.sort
val array_sort : Z3.Sort.sort -> Z3.Sort.sort
val vector_sort : int -> Z3.Sort.sort -> Z3.Sort.sort
val named_sort : string -> Z3.Sort.sort
val struct_sort : Llvm.lltype -> Z3.Sort.sort
val struct_as_fields_sort : Z3.Sort.sort list -> Z3.Sort.sort
val sort_of_lltype : Llvm.lltype -> Z3.Sort.sort

val mk_int_type : Z3.Expr.expr
val mk_bv_type : int -> Z3.Expr.expr (* bit-vectors are limited to "int" sizes *)
val mk_fp_type : string -> Z3.Expr.expr
val mk_void_type : Z3.Expr.expr
val mk_label_type : Z3.Expr.expr
val mk_named_type : string -> Z3.Expr.expr
val mk_struct_type : Z3.Expr.expr list -> Z3.Expr.expr
val mk_function_type : Z3.Expr.expr list -> Z3.Expr.expr -> Z3.Expr.expr
val mk_pointer_type : Z3.Expr.expr -> Z3.Expr.expr
val mk_vector_type : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
val mk_array_type : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
val mk_metadata_type : Z3.Expr.expr

val mk_i8_type : Z3.Expr.expr
val mk_i32_type : Z3.Expr.expr
val mk_i64_type : Z3.Expr.expr
val mk_void_pointer_type : Z3.Expr.expr -> Z3.Expr.expr

val mk_int : int -> Z3.Expr.expr
val mk_int64 : int64 -> Z3.Expr.expr
val mk_bv : int -> string -> Z3.Expr.expr
val mk_bv64 : int -> int64 -> Z3.Expr.expr
val mk_fp : string -> string -> string -> Z3.Expr.expr
val mk_void : Z3.Expr.expr
val mk_struct : Z3.Sort.sort -> Z3.Expr.expr list -> Z3.Expr.expr
val mk_struct_llt : Llvm.lltype -> Z3.Expr.expr list -> Z3.Expr.expr
val mk_struct_field : Z3.Sort.sort -> int -> Z3.Expr.expr -> Z3.Expr.expr
val mk_struct_field_llt : Llvm.lltype -> int -> Z3.Expr.expr -> Z3.Expr.expr
val mk_array_val : Z3.Expr.expr list -> Z3.Expr.expr
val mk_vector_val : Z3.Expr.expr list -> Z3.Expr.expr
val mk_null_ptr : Z3.Expr.expr
val mk_undef : Z3.Sort.sort -> Z3.Expr.expr

val mk_pointer : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
val mk_array : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
val mk_malloced : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
val mk_padding : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
val mk_jump_end : Z3.Expr.expr
val mk_jump : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
val mk_eltptr : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
val mk_sizeof : Z3.Expr.expr -> Z3.Expr.expr
val mk_bitcast : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr

val is_pointer : Z3.Expr.expr -> bool

val mk_pointer_size : Z3.Expr.expr
val mk_field_type : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
val mk_offset : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
val mk_exploded_struct : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr

val expr_of_sizeof : Llvm.lltype -> Z3.Expr.expr
val expr_of_lltype : Llvm.lltype -> Z3.Expr.expr
val expr_of_int_const : Llvm.llvalue -> Z3.Expr.expr
val expr_of_op : Llvm.Opcode.t -> Llvm.llvalue -> Z3.Expr.expr
val expr_of_const_expr : Llvm.llvalue -> Z3.Expr.expr
val expr_of_llvalue : Llvm.llvalue -> Z3.Expr.expr
val expr_of_composite_llvalue : Llvm.llvalue -> Z3.Expr.expr list

val pp_llexpr : Format.formatter -> Z3.Expr.expr -> unit
val string_of_llexpr : Z3.Expr.expr -> string
