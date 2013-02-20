malloc:
  {}
  {($ret_v1 != numeric_const("0") * malloced($ret_v1,@parameter0:) * pointer($ret_v1,@parameter0:,_v)) || $ret_v1 = numeric_const("0")}

__safe_malloc:
  {}
  {malloced($ret_v1,@parameter0:) * pointer($ret_v1,@parameter0:,_v)}

free:
  {malloced(@parameter0:,_sz) * pointer(@parameter0:,_sz,_v)}
  {}
