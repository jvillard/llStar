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

/* this function does *not* modify its argument, since its passed by value.
 The s in the post-condition is actually a variable allocated on the stack by LLVM.
 Nevertheless, we prove that the copy at s is set to 0.
*/
yay_int:
  {pointer(@parameter0:,pointer_type(named_type("struct.ij")),_v)
      * pointer(_v,sizeof(named_type("struct.ij")),_w)}
  {pointer(@parameter0:,pointer_type(named_type("struct.ij")),_v)
      * pointer(_v,sizeof(named_type("struct.ij")),_w)
      * pointer(s,sizeof(integer_type(numeric_const("32"))),numeric_const("0"))
      * pointer(builtin_plus(s,numeric_const("4")),sizeof(integer_type(numeric_const("32"))),numeric_const("0"))}

setifield:
  {pointer(@parameter0:,sizeof(named_type("struct.oneint")),_v)}
  {pointer(@parameter0:,sizeof(named_type("struct.oneint")),numeric_const("0"))}

yay_yay_int:
  {pointer(@parameter0:,sizeof(named_type("struct.ij")),_v)}
  {pointer(@parameter0:,sizeof(named_type("struct.ij")),collate(numeric_const("0"),numeric_const("0")))}
