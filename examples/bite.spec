main:
   {}
   {$ret_v1 = numeric_const("1664")}

f:
   {@parameter0: = _a}
   {$ret_v1 = _a}

malloc:
  {}
  {field($ret_v1, "ptr", _x)}

free:
  {field(@parameter0:, "ptr", _x)}
  {}

