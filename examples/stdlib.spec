malloc:
  {}
  {malloc_blob($ret_v1,@parameter0:)}

free:
  {malloc_blob(@parameter0:,_sz)}
  {}
