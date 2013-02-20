import "../../specs/stdlib.spec";

norbert:
  {@parameter0: = val(numeric_const("8"),_x)
   * @parameter1: = val(numeric_const("8"),_y)}
  {pointer($ret_v1,sizeof(named_type("node")),collate(@parameter0:,@parameter1:)) * malloced($ret_v1,sizeof(named_type("node")))
   || $ret_v1 = numeric_const("0")}
