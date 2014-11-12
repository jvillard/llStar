import "llvm.logic";
import "lseg.logic";

nodedecl: "listitem"("4") = sllnode
nodedecl: "struct.node"("1") = sllnode

import "stdlib.spec";

list_append:
  {lseg_ne("struct.node",@parameter0:,bv_const("64", "0")) * lseg("struct.node",@parameter1:,bv_const("64", "0"))}
  {lseg("struct.node",$ret_v1,bv_const("64", "0"))}
