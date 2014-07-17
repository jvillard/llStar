import "../rules/llvm.logic";
import "../rules/dags_const_pointers.logic";

equiv dagnode:
      dagnode(lltype ?t, i64 ?x, i64 ?l, i64 ?r)
      <=> pointer(i64 ?x, lltype struct.node, mk_struct.node(i64 _c, i64 ?l, i64 ?r))

procedure mark(i64 %d)
  {dag(lltype struct.node,i64 %d,int _d)}
  {dag(lltype struct.node,i64 %d,int _d)};
