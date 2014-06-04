import "stdlib.spec";
import "../../rules/llvm.logic";

norbert:
  {}
  {pointer($ret_v1,named_type("node"),mk_node(@parameter0:,@parameter1:)) * malloced($ret_v1,sizeof(named_type("node")))
   || $ret_v1 = NULL()}

chew_slowly:
   {}
   {$ret_v1 = @parameter0:}
