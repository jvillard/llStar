access_nested_struct:
  {pointer(s, named_type("astruct"), _v) * !bvule(s,bvadd.64(s,sizeof(named_type("astruct"))))}
  {pointer(s, named_type("astruct"), _v)}
