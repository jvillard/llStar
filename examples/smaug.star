import "llvm.logic";
import "lseg.logic";

nodedecl: "listitem"("4") = sllnode
nodedecl: "struct.node"("1") = sllnode

import "stdlib.spec";

smaug:
  {lseg("listitem",@parameter0:,bv_const("64", "0"))}
  {lseg("listitem",@parameter0:,bv_const("64", "0"))}


