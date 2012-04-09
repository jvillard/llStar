malloc:
  {}
  {malloced($ret_v1,@parameter0:) * blob($ret_v1,@parameter0:)}

free:
  {malloced(@parameter0:,_sz) * blob(@parameter0:,_sz)}
  {}
