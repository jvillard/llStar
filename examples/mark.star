import "../rules/llvm.logic";
/* import "../rules/dags_const_pointers.logic"; */

nodedecl dagnode: struct.node(1, 2);

procedure mark(i64 %d)
  {dag(lltype struct.node,i64 %d,int _d)}
  {dag(lltype struct.node,i64 %d,int _d)};
