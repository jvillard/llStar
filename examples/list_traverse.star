import "llvm.logic";
import "lseg.logic";

nodedecl: "listitem"("4") = sllnode
nodedecl: "struct.node"("1") = sllnode

import "stdlib.spec";

list_traverse:
  {lseg("struct.node",@parameter0:,bv_const("64", "0"))}
  {lseg("struct.node",@parameter0:,bv_const("64", "0"))}
