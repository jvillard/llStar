main:
   {}
   {$ret_v1 = numeric_const("1664")}

f:
   {@parameter0: = _a}
   {$ret_v1 = _a}

malloc:
  {}
  {malloc_block($ret_v1)}

free:
  {malloc_block(@parameter0:)}
  {}
