import "llvm.logic";
import "lseg.logic";

nodedecl: "struct.node"("1") = sllnode
_Z8traverseP4node:
  {lseg("struct.node",@parameter0:,NULL())}
  {lseg("struct.node",@parameter0:,NULL())}

_Z6appendP4nodeS0_:
  {lseg_ne("struct.node",@parameter0:,NULL()) * lseg("struct.node",@parameter1:,NULL())}
  {lseg_ne("struct.node",@parameter0:,NULL())}
