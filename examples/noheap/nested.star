import "../../rules/llvm.logic";

access_nested_struct:
  {pointer(s, named_type("astruct"), _v)}
  {pointer(s, named_type("astruct"), _v)}
