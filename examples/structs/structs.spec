import "../../specs/stdlib.spec";

/* test if implications hold with this empty function */
skip:
   {builtin_neq(x,numeric_const("0")) = y * y = bv_const("1", "1")}
   {x != numeric_const("0")}

main:
   {}
   {$ret_v1 = builtin_bvmul(bv_const("32", "32"),bv_const("32", "52"))}

f:
   {}
   {$ret_v1 = @parameter0:}

setint:
  {pointer(@parameter0:,sizeof(integer_type(numeric_const("32"))),_w)}
  {pointer(@parameter0:,sizeof(integer_type(numeric_const("32"))),bv_const("32", "0"))}

/* this function is modified by LLVM which does not appear to support passing
 * structs by value...
*/
yay_int: {pointer(@parameter0:,sizeof(pointer_type(named_type("struct.ij"))),_v)
      * pointer(_v,sizeof(named_type("struct.ij")),_w)}
  {pointer(@parameter0:,sizeof(pointer_type(named_type("struct.ij"))),_v)
      * pointer(_v,sizeof(named_type("struct.ij")),_w)}

setifield:
  {pointer(@parameter0:,sizeof(named_type("struct.oneint")),_v)}
  {pointer(@parameter0:,sizeof(named_type("struct.oneint")),bv_const("32", "0"))}

yay_yay_int:
  {pointer(@parameter0:,sizeof(named_type("struct.ij")),_v)}
  {pointer(@parameter0:,sizeof(named_type("struct.ij")),struct_struct.ij(bv_const("32", "0"),bv_const("32", "0")))}
