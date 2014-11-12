import "llvm.logic";
import "lseg.logic";

nodedecl: "struct.node"("1") = sllnode

equiv bitcast_node_hint:
  pointer(?x, array_type(bv_const("64", "16"), integer_type("8")), ?v)
  * !bitcast(?x, pointer_type(named_type("struct.node")))
  <=> pointer(?x, named_type("struct.node"),_v)
import "stdlib.spec";
import "fortran.spec";

__linkedlist_MOD_list_dispose:
  {pointer(@parameter0:,pointer_type(named_type("struct.node")),_v)
* lseg("struct.node",_v,NULL())}
  {pointer(@parameter0:,pointer_type(named_type("struct.node")),_v)}
