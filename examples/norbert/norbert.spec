import "../../specs/stdlib.spec";

norbert:
  {a = val(numeric_const("8"),_x) * b = val(numeric_const("8"),_y)}
  {pointer($ret_v1,sizeof(named_type("node")),collate(a,b))
 || $ret_v1 = numeric_const("0")}
