malloc:
  {}
  {($ret_v1 != NULL() * malloced($ret_v1,@parameter0:) * pointer($ret_v1,array_type(@parameter0:,integer_type(numeric_const("8"))),_v)) || $ret_v1 = NULL()}

__safe_malloc:
  {}
  {malloced($ret_v1,@parameter0:) * pointer($ret_v1,@parameter0:,_v)}

free:
  {malloced(@parameter0:,_s) * pointer(@parameter0:,array_type(_s,integer_type(numeric_const("8"))),_v)}
  {}
