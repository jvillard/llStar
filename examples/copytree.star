predicate dag (lltype, i64, int);
predicate dagnode(lltype,i64,i64,i64);
function int mdag(int, i64);
function int mnode(i64, i64);

import "../rules/llvm.logic";
import "../rules/dags_const_pointers.logic";
import "../specs/stdlib.spec";

rewrite dagnode:
   dagnode(lltype ?t, i64 ?x, i64 ?l, i64 ?r)
   -> pointer(i64 ?x, named("struct.node") { i32 _c, i64 ?l, i64 ?r })
;

rule tree_fold:
 bool ?f |  dagnode(lltype ?t,i64 ?x,i64 ?l,i64 ?r)
  * dag(lltype ?t,i64 ?l,int ?d) * dag(lltype ?t,i64 ?r,int ?d)
  * mdag(int ?d,i64 ?x) = mnode(i64 ?l,i64 ?r)
 * bool ?fl |- bool ?fr * dag(lltype ?t,i64 ?x,int ?dd)
if
 bool ?f * dag(lltype ?t,i64 ?x,int ?d) | bool ?fl |- bool ?fr * int ?d = int ?dd
;


procedure copytree(i64 %x) returns (i64 %z)
  {dag(lltype "struct.node",i64 %x,int _d)}
  {dag(lltype "struct.node",i64 %x,int _d) * dag(lltype "struct.node", i64 %z, int _dd)}
;
