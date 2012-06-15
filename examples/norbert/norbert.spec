import "../../specs/stdlib.spec";

norbert:
  {a = val(numeric_const("8"),_x) * b = val(numeric_const("8"),_y)}
  {pointer(r,sizeof(named_type("node")),struct(named_type("node"),a,b)) || r = numeric_const("0")}