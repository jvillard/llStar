predicate dag(lltype, i64);
predicate dagnode(lltype, i64, i64, i64);
predicate tree(lltype, i64);
predicate treenode(lltype, i64, i64, i64);

predicate here(label);
predicate at(label, bool);

import "../rules/llvm.logic";
import "../specs/safe_stdlib.spec";

rewrite dagnode:
   dagnode(lltype ?t, i64 ?x, i64 ?l, i64 ?r)
   -> pointer(i64 ?x, named("struct.node") { i32 _c, i64 ?l, i64 ?r })
;

rewrite treenode:
   treenode(lltype ?t, i64 ?x, i64 ?l, i64 ?r)
   -> pointer(i64 ?x, named("struct.node") { i32 _c, i64 ?l, i64 ?r })
;

rule nobacktrack tree_unfold_right:
 bool ?f | bool ?fl * i64 ?x != NULL()
           |-
           bool ?fr * tree(lltype ?t, i64 ?x)
if
 bool ?f | bool ?fl
           |-
           bool ?fr *
           treenode(lltype ?t, i64 ?x, i64 _l, i64 _r) *
           tree(lltype ?t, i64 _l) *
           tree(lltype ?t, i64 _r)
;

rule nobacktrack remove_tree:
  bool ?f | bool ?fl * tree(lltype ?t, i64 ?x)
            |-
            bool ?fr * tree(lltype ?t, i64 ?x)
if
 bool ?f * tree(lltype ?t, i64 ?x) | bool ?fl |- bool ?fr
;

rewrite dag_nil:
  dag(lltype ?t, NULL()) -> emp
;

rewrite tree_nil:
  tree(lltype ?t, NULL()) -> emp
;

rewrite dag_unfold_under_at:
  !at(label ?l, dag(lltype ?t, i64 ?x)) * i64 ?x != NULL() * bool ?f
  ->
     !at(label ?l, dag(lltype ?t, i64 ?x)) *
     !at(label ?l,
         here(label _node) *
         here(label _a) *
         here(label _b) *
         here(label _c)) *
     !at(label _node, dagnode(lltype ?t, i64 ?x, i64 _l, i64 _r)) *
     !at(label _u, here(label _a) * here(label _b)) *
     !at(label _v, here(label _b) * here(label _c)) *
     !at(label _u, dag(lltype ?t, i64 _l)) * 
     !at(label _v, dag(lltype ?t, i64 _r)) *
     bool ?f
;

rule nobacktrack dag_fold:
 bool ?f | here(label ?node) * 
           here(label ?a) * 
           here(label ?b) *
           here(label ?c) *
           !at(label ?node, dagnode(lltype ?t, i64 ?x, i64 ?l, i64 ?r)) *
           !at(label ?u, here(label ?a) * here(label ?b)) *
           !at(label ?v, here(label ?b) * here(label ?c)) *
           !at(label ?u, dag(lltype ?t, i64 ?l)) * 
           !at(label ?v, dag(lltype ?t, i64 ?r)) *
           bool ?fl
           |-
           bool ?fr *
           dag(lltype ?t, i64 ?x)
if
 bool ?f * dag(lltype ?t, i64 ?x) | bool ?fl |- bool ?fr
;

rewrite at_label_eq:
 !at(label ?a, here(label ?b)) -> label ?a = label ?b
;

rule nobacktrack inline_at_labels2:
 bool ?f | here(label ?l) *
           !at(label ?l, here(label ?a) * here(label ?b)) * bool ?fl
           |-
           bool ?fr
if
 bool ?f | here(label ?a) *
           here(label ?b) *
           !at(label ?l, here(label ?a) * here(label ?b)) *
           bool ?fl
           |-
           bool ?fr
;

rule nobacktrack inline_at_labels4:
 bool ?f | here(label ?l) *
           !at(label ?l, here(label ?a) * here(label ?b) * here(label ?c) * here(label ?d)) *
           bool ?fl
           |-
           bool ?fr
if
 bool ?f | here(label ?a) *
           here(label ?b) *
           here(label ?c) *
           here(label ?d) *
           !at(label ?l, here(label ?a) * here(label ?b) * here(label ?c) * here(label ?d)) *
           bool ?fl
           |-
           bool ?fr
;

rule nobacktrack pointsto_in_at:
 bool ?f | here(label ?l) *
           !at(label ?l, pointer(i64 ?x, 'a ?y) * bool ?p) *
           bool ?fl
           |-
           pointer(i64 ?x, 'a ?z) *
           bool ?fr
if
 bool ?f * pointer(i64 ?x, 'a ?y) |
           here(label _ll) *
           !at(label _ll, bool ?p) *
           !at(label ?l, pointer(i64 ?x, 'a ?y) * here(label _ll)) *
           bool ?fl
           |-
           'a ?y = 'a ?z *
           bool ?fr
;

rule nobacktrack pointsto_outside_at:
 bool ?f | pointer(i64 ?x, 'a ?y) *
           !at(label ?l, pointer(i64 ?x, 'a ?y) * bool ?p) *
           bool ?fl
           |-
           here(label ?l) *
           bool ?fr
if
 bool ?f | bool ?fl *
           !at(label _lll, bool ?p)
           |-
           here(label _lll) *
           bool ?fr
;


rule nobacktrack label_emp:
  bool ?f | !at(label ^l, emp) * bool ?fl |- here(label ^l) * bool ?fr
if
 bool ?f * here(label ^l) | !at(label ^l, emp) * bool ?fl |- bool ?fr
;

rule match_at:
 bool ?f | !at(label ^l, bool ?p) * bool ?fl |- !at(label ^u, bool ?p) * bool ?fr
if
 bool ?f | !at(label ^l, bool ?p) * bool ?fl |- label ^u = label ^l * bool ?fr
;

rule nobacktrack fold_at2:
 bool ?f | !at(label ^l, here(label ^l1) * here(label ^l2)) * bool ?fl
           |- here(label ^l) * bool ?fr
if
 bool ?f | !at(label ^l, here(label ^l1) * here(label ^l2)) * bool ?fl
           |- here(label ^l1) * here(label ^l2) * bool ?fr
;

rule nobacktrack fold_at4:
 bool ?f | !at(label ^l, here(label ^l1) * here(label ^l2) * here(label ^l3) * here(label ^l4)) * 
           bool ?fl
           |-
           here(label ^l) *
           bool ?fr
if
 bool ?f | !at(label ^l, here(label ^l1) * here(label ^l2) * here(label ^l3) * here(label ^l4)) * 
           bool ?fl
           |-
           here(label ^l1) *
           here(label ^l2) *
           here(label ^l3) *
           here(label ^l4) *
           bool ?fr
;

rule nobacktrack equal_fresh:
 bool ?f | bool ?fl |- ('a ?a = 'a ^u) * bool ?fr
with
 fresh 'a ^u in bool ?fl;
 fresh 'a ^u in bool ?f
if
 bool ?f | bool ?fl * 'a ?a = 'a ^u |- bool ?fr
;

rule nobacktrack equal_label:
 bool ?f | bool ?fl |- (label ?a = label ^u) * bool ?fr
with
 fresh label ^u in bool ?fl;
 fresh label ^u in bool ?f
if
 bool ?f | bool ?fl * label ?a = label ^u |- bool ?fr
;

rule nobacktrack remove_label:
 bool ?f | here(label ?l) * bool ?fl |- here(label ?l) * bool ?fr
if
 bool ?f * here(label ?l) | bool ?fl |- bool ?fr
;

procedure copytree(i64 %x) returns (i64 %z)
  {here(label _l) * !at(label _l, dag(lltype "struct.node",i64 %x))}
  {here(label _l) * tree(lltype "struct.node", i64 %z)}
;
