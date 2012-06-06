import "../../specs/stdlib.spec";

main:
   {}
   {$ret_v1 = builtin_mult(numeric_const("32"),numeric_const("52"))}

f:
   {@parameter0: = _a}
   {$ret_v1 = _a}

setint:
  {pointer(i,pointer_type(integer_type()),_v)
      * pointer(_v,integer_type(),_w)}
  {pointer(i,pointer_type(integer_type()),_v)
      * pointer(_v,integer_type(),numeric_const("0"))}

yay_int:
  {pointer(s,pointer_type(named_type("struct.ij")),_v)
      * pointer(_v,named_type("struct.ij"),_w)}
  {pointer(s,pointer_type(named_type("struct.ij")),_v)
      * pointer(_v,pointer_type(integer_type()),numeric_const("0"))
      * pointer(builtin_plus(_v,numeric_const("1")),pointer_type(integer_type()),numeric_const("0"))}

setifield:
  {pointer(s,pointer_type(named_type("struct.oneint")),_v)
      * pointer(_v,named_type("struct.oneint"),_w)}
  {pointer(s,pointer_type(named_type("struct.oneint")),_v)
      * pointer(_v,pointer_type(integer_type()),numeric_const("0"))}

yay_yay_int:
  {pointer(s,pointer_type(named_type("struct.ij")),_v)
      * pointer(_v,named_type("struct.ij"),_w)}
  {pointer(s,pointer_type(named_type("struct.ij")),_v)
      * pointer(_v,pointer_type(integer_type()),numeric_const("0"))
      * pointer(builtin_plus(_v,numeric_const("1")),pointer_type(integer_type()),numeric_const("0"))}
