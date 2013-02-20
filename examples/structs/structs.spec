import "../../specs/stdlib.spec";

/* test if implications hold with this empty function */
skip:
   {builtin_neq(x,numeric_const("0")) = y * y = numeric_const("1")}
   {x != numeric_const("0")}

main:
   {}
   {$ret_v1 = builtin_mult(numeric_const("32"),numeric_const("52"))}

f:
   {@parameter0: = _a}
   {$ret_v1 = _a}

setint:
  {pointer(@parameter0:,sizeof(integer_type(numeric_const("32"))),_w)}
  {pointer(@parameter0:,sizeof(integer_type(numeric_const("32"))),numeric_const("0"))}

/* this function is modified by LLVM which does not appear to support passing
 * structs by value...
*/
yay_int: {pointer(@parameter0:,sizeof(pointer_type(named_type("struct.ij"))),_v)
      * pointer(_v,sizeof(named_type("struct.ij")),_w)}
  {pointer(@parameter0:,sizeof(pointer_type(named_type("struct.ij"))),_v)
      * pointer(_v,sizeof(named_type("struct.ij")),_w)}

setifield:
  {pointer(@parameter0:,sizeof(named_type("struct.oneint")),_v)}
  {pointer(@parameter0:,sizeof(named_type("struct.oneint")),numeric_const("0"))}

yay_yay_int:
  {pointer(@parameter0:,sizeof(named_type("struct.ij")),_v)}
  {pointer(@parameter0:,sizeof(named_type("struct.ij")),collate(numeric_const("0"),numeric_const("0")))}
