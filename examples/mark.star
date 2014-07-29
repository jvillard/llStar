predicate dag (lltype, i64, int);
predicate dagnode(lltype,i64,i64,i64);

import "../rules/llvm.logic";
import "../rules/dags_const_pointers.logic";

rewrite dagnode:
   dagnode(lltype ?t, i64 ?x, i64 ?l, i64 ?r)
   -> pointer(i64 ?x, named("struct.node") { i32 _c, i64 ?l, i64 ?r })
;

procedure mark(i64 %x)
  {dag(lltype "struct.node",i64 %x,int _d)}
  {dag(lltype "struct.node",i64 %x,int _d)}
;
