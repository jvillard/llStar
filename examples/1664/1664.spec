main:
   {}
   {$ret_v1 = builtin_bvmul(bv_const("32","32"),bv_const("32", "52"))}

/* this spec is equivalent to f: {}{$ret_v1 = @parameter0:} */
/*
f:
   {@parameter0: = _x}
   {$ret_v1 = _x * !type($ret_v1,integer_type(numeric_const("32")))}
*/
f:
   {}
   {$ret_v1 = @parameter0: * !type($ret_v1,integer_type(numeric_const("32")))}
