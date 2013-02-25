main:
   {}
   {$ret_v1 = builtin_mult(numeric_const("32"),numeric_const("52"))}

/* this spec is equivalent to f: {}{$ret_v1 = @parameter0:} */
f:
   {@parameter0: = _x}
   {$ret_v1 = _x}
