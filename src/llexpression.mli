val bool_sort : Z3.Sort.sort
val int_sort : Z3.Sort.sort
val bv_sort : int -> Z3.Sort.sort
val void_sort : Z3.Sort.sort
val bblabel_sort : Z3.Sort.sort
val lltype_sort : Z3.Sort.sort
val llmem_sort : Z3.Sort.sort
val jump_sort : Z3.Sort.sort
val md_sort : Z3.Sort.sort
val pointer_sort : Z3.Sort.sort
val size_sort : Z3.Sort.sort
val function_sort : Z3.Sort.sort list -> Z3.Sort.sort -> Z3.Sort.sort
val array_sort : Z3.Sort.sort -> Z3.Sort.sort
val vector_sort : int -> Z3.Sort.sort -> Z3.Sort.sort
val named_sort : Z3.Expr.expr -> Z3.Sort.sort
val struct_sort : Llvm.lltype -> Z3.Sort.sort
val struct_as_fields_sort : Z3.Sort.sort list -> Z3.Sort.sort
val sort_of_lltype : Llvm.lltype -> Z3.Sort.sort

val mkIntType : Z3.Expr.expr
val mkBVType : int -> Z3.Expr.expr (* bit-vectors are limited to "int" sizes *)
val mkFPType : string -> Z3.Expr.expr
val mkVoidType : Z3.Expr.expr
val mkLabelType : Z3.Expr.expr
val mkNamedType : Z3.Expr.expr -> Z3.Expr.expr
val mkNamedType_s : string -> Z3.Expr.expr
val mkStructType : Z3.Expr.expr list -> Z3.Expr.expr
val mkFunctionType : Z3.Expr.expr list -> Z3.Expr.expr -> Z3.Expr.expr
val mkPointerType : Z3.Expr.expr -> Z3.Expr.expr
val mkVectorType : int -> Z3.Expr.expr -> Z3.Expr.expr
val mkArrayType : int -> Z3.Expr.expr -> Z3.Expr.expr
val mkMDType : Z3.Expr.expr

val mkI8Type : Z3.Expr.expr
val mkI32Type : Z3.Expr.expr
val mkI64Type : Z3.Expr.expr
val mkVoidPointerType : Z3.Expr.expr -> Z3.Expr.expr

val mkInt : int -> Z3.Expr.expr
val mkInt64 : int64 -> Z3.Expr.expr
val mkBV : int -> string -> Z3.Expr.expr
val mkBV64 : int -> int64 -> Z3.Expr.expr
val mkFP : string -> string -> string -> Z3.Expr.expr
val mkVoid : Z3.Expr.expr
val mkStruct : Z3.Sort.sort -> Z3.Expr.expr list -> Z3.Expr.expr
val mkStruct_llt : Llvm.lltype -> Z3.Expr.expr list -> Z3.Expr.expr
val mkNullPtr : Z3.Expr.expr
val mkUndef : Z3.Sort.sort -> Z3.Expr.expr

val as_llmem_cons : Z3.Sort.sort -> Z3.FuncDecl.func_decl
val as_llmem : Z3.Sort.sort -> Z3.Expr.expr -> Z3.Expr.expr

val mkPointer : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
val mkMalloced : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
val mkPadding : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
val mkJumpEnd : Z3.Expr.expr
val mkJump : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
val mkEltptr : Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr

val expr_of_sizeof : Llvm.lltype -> Z3.Expr.expr
val expr_of_lltype : Llvm.lltype -> Z3.Expr.expr
val expr_of_int_const : Llvm.llvalue -> Z3.Expr.expr
val expr_of_op : Llvm.Opcode.t -> Llvm.llvalue -> Z3.Expr.expr
val expr_of_const_expr : Llvm.llvalue -> Z3.Expr.expr
val expr_of_llvalue : Llvm.llvalue -> Z3.Expr.expr
val expr_of_composite_llvalue : Llvm.llvalue -> Z3.Expr.expr list

val pp_llexpr : Format.formatter -> Z3.Expr.expr -> unit
val string_of_llexpr : Z3.Expr.expr -> string

(*
val expand_named_type : Z3.Expr.expr -> Z3.Expr.expr
val check_same_type : Z3.Expr.expr -> Z3.Expr.expr -> bool
val check_same_type_list : Z3.Expr.expr list -> Z3.Expr.expr list -> bool
val check_type_of_expr : Z3.Expr.expr -> Z3.Expr.expr
val check_type_of_expr_list : Z3.Expr.expr list -> Z3.Expr.expr list
val check_wf_form_at : Z3.Expr.expr -> unit
val check_wf_form : Z3.Expr.expr -> unit
val check_wf_where : Z3.Expr.expr -> unit
val check_wf_sequent : Psyntax.psequent -> unit
val check_wf_sequent_rule : Psyntax.sequent_rule -> unit
val check_wf_rewrite_guard : Psyntax.rewrite_guard -> unit
val check_wf_rewrite_rule : Psyntax.rewrite_rule -> unit
val check_wf_logic : Psyntax.logic -> unit
val check_wf_spec : Spec.spec -> unit
val check_wf_funspec : Logic_spec.funspec -> unit
*)
