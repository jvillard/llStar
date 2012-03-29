main:
   {}
   {$ret_v1 = numeric_const("1664")}

f:
   {@parameter0: = _a}
   {$ret_v1 = _a}

malloc:
  {}
  {pointer($ret_v1, _x) * malloc($ret_v1,@parameter0:)}

free:
  {pointer(@parameter0:, _x) * malloc(@parameter0:,_s)}
  {}

