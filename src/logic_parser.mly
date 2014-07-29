/********************************************************
 * Original file part of coreStar, distributed under a BSD
 * license LICENSE_coreStar.txt
 ********************************************************/

%{ (* header *)

open Corestar_std
open Format

module C = Core

open Llutils
open Llexpression

let pp_pos f pos =
  let line = pos.Lexing.pos_lnum in
  let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
  fprintf f "%d:%d" line column

let message prefix text =
  let start_pos = Parsing.symbol_start_pos () in
  let end_pos = Parsing.symbol_end_pos () in
  let fn = from_option "<unknown file>" !Load.file_name in
  eprintf "@{<b>%s: %s:(%a)-(%a):@} %s@\n"
    prefix fn pp_pos start_pos pp_pos end_pos text

let parse_error = message "E"
let parse_warning = message "W"

let z3_ctx = Syntax.z3_ctx

let ops = Hashtbl.create 0

let () = List.iter (fun (n, f) -> Hashtbl.add ops n f) 
  [
    (* spatial predicates *)
    ("NULL", mk_0 mk_null_ptr);
    ("pointer", mk_2 mk_pointer);
    ("array", mk_3 mk_array);
    ("bitcast", mk_2 mk_bitcast);
    ("pad", mk_2 mk_padding);
    (* eltptr *)
    ("jump", mk_2 mk_jump);
    ("jump_end", mk_0 mk_jump_end);
    ("eltptr", mk_3 mk_eltptr);
    (* types *)
    ("sizeof", mk_1 mk_sizeof);
    ("pointer_type", mk_1 mk_pointer_type);
    ("array_type", mk_2 mk_array_type);
    (* struct rule generators *)
    ("ptr_size", mk_0 mk_pointer_size);
    ("offset", mk_2 mk_offset);
    ("field_type", mk_2 mk_field_type);
    ("exploded_struct", mk_2 mk_exploded_struct);
    (* boolean *)
    ("not", mk_1 (Z3.Boolean.mk_not z3_ctx));
    ("ite", mk_3 (Z3.Boolean.mk_ite z3_ctx));
    (* bitvector operations *)
    ("bvudiv", mk_2 (Z3.BitVector.mk_udiv z3_ctx));
    ("bvsdiv", mk_2 (Z3.BitVector.mk_sdiv z3_ctx));
    ("bvmul", mk_2 (Z3.BitVector.mk_mul z3_ctx));
    ("bvsub", mk_2 (Z3.BitVector.mk_sub z3_ctx));
    ("bvadd", mk_2 (Z3.BitVector.mk_add z3_ctx));
    ("bvsle", mk_2 (Z3.BitVector.mk_sle z3_ctx));
    ("bvule", mk_2 (Z3.BitVector.mk_ule z3_ctx));
    ("bvslt", mk_2 (Z3.BitVector.mk_slt z3_ctx));
    ("bvult", mk_2 (Z3.BitVector.mk_ult z3_ctx));
    ("bvsge", mk_2 (Z3.BitVector.mk_sge z3_ctx));
    ("bvuge", mk_2 (Z3.BitVector.mk_uge z3_ctx));
    ("bvsgt", mk_2 (Z3.BitVector.mk_sgt z3_ctx));
    ("bvugt", mk_2 (Z3.BitVector.mk_ugt z3_ctx));
  (* TODO: many other bitvector operations (all of BitVector.mk_* ) *)
  ]

let register_op = Hashtbl.add ops
let lookup_op op args =
  try (Hashtbl.find ops op) args
  with Not_found ->
    prerr_endline (Printf.sprintf "Undeclared operation %s" op);
    assert false

%} /* declarations */

/* ============================================================= */
/* tokens */
%token <int> LLBVTYPE
%token <string> IDENTIFIER
%token <string> INTEGER
%token <string> LIDENTIFIER
%token <string> PGIDENTIFIER
%token <string> PLIDENTIFIER
%token <string> PUREIDENTIFIER
%token <string> STRING_CONSTANT
%token <string> TPIDENTIFIER
%token <string> VPIDENTIFIER
%token ABDUCT
%token ABSRULE
%token BAR
%token BIMP
%token BOOL
%token BVADD
%token BVMUL
%token BVSDIV
%token BVSGE
%token BVSGT
%token BVSLE
%token BVSLT
%token BVSUB
%token BVUDIV
%token BVUGE
%token BVUGT
%token BVULE
%token BVULT
%token CMP_GE
%token CMP_GT
%token CMP_LE
%token CMP_LT
%token COLON
%token COMMA
%token CROSS
%token DASHV
%token EMP
%token EOF
%token EQUALS
%token EQUIVRULE
%token FALSE
%token FRESH
%token FUNCTION
%token GLOBAL
%token IF
%token IMPORT
%token IN
%token INCONSISTENT
%token INT
%token LLDOUBLE
%token LLDOUBLE
%token LLFLOAT
%token LLHALF
%token LLJUMP
%token LLNAMED
%token LLTYPE
%token LLVOID
%token L_BRACE
%token L_BRACKET
%token L_LTBRACE
%token L_PAREN
%token NO_BACKTRACK
%token NOT_EQUALS
%token OROR
%token PREDICATE
%token PRIORITY
%token PROCEDURE
%token PURECHECK
%token RETURNS
%token REWRITERULE
%token RULE
%token R_BRACE
%token R_BRACEGT
%token R_BRACKET
%token R_PAREN
%token RIGHTARROW
%token SEMICOLON
%token STAR
%token TRUE
%token VDASH
%token WAND
%token WITH
%token WITHOUT


/* === associativity and precedence === */

%right OROR
%right STAR
%right BVUDIV BVSDIV BVSUB BVADD BVMUL
%right RIGHTARROW

/* entry points */
%start file
%type <ParserAst.entry Load.entry list> file

%% /* rules */

/* Identifiers and constants */

variable:
  | sort PGIDENTIFIER { Syntax.mk_pgvar $1 $2 }
  | sort PLIDENTIFIER { Syntax.mk_plvar $1 $2 }
  | sort LIDENTIFIER { Syntax.mk_lvar $1 $2 }
  | sort TPIDENTIFIER { Syntax.mk_tpat $1 $2 }
  | sort VPIDENTIFIER { Syntax.mk_vpat $1 $2 }
  | sort IDENTIFIER { Z3.Expr.mk_const_s z3_ctx $2 $1 }
;
variable_list_ne:
  |  variable    { [$1] }
  |  variable COMMA variable_list_ne  { $1 :: $3 }
;
variable_list:
  |  {[]}
  | variable_list_ne { $1 }
;

identifier_list_ne:
  | IDENTIFIER { [$1] }
  | IDENTIFIER COMMA identifier_list_ne  { $1 :: $3 }
;

/* none implemented for now, which ocamlyacc doesn't like */
/*
binop:
;
*/

cmpop:
  | BVSLE { Z3.BitVector.mk_sle z3_ctx }
  | BVULE { Z3.BitVector.mk_ule z3_ctx }
  | BVSLT { Z3.BitVector.mk_slt z3_ctx }
  | BVULT { Z3.BitVector.mk_ult z3_ctx }
  | BVSGE { Z3.BitVector.mk_sge z3_ctx }
  | BVUGE { Z3.BitVector.mk_uge z3_ctx }
  | BVSGT { Z3.BitVector.mk_sgt z3_ctx }
  | BVUGT { Z3.BitVector.mk_ugt z3_ctx }
;


/* Expressions */

sort:
  | BOOL { bool_sort }
  | INT { int_sort }
  | LLJUMP { jump_sort }
  | LLVOID { void_sort }
  | LLBVTYPE { bv_sort $1 }
  | LLTYPE { lltype_sort }
  | LLNAMED L_PAREN STRING_CONSTANT R_PAREN { sort_of_lltype (lltype_of_name $3) }
  | L_LTBRACE sort_list R_BRACEGT { struct_as_fields_sort $2 }
  | L_BRACE sort_list R_BRACE { struct_as_fields_sort $2 }
  | L_BRACKET term CROSS sort R_BRACKET { array_sort $4 }
  | sort STAR { pointer_sort }
  | IDENTIFIER { Z3.Sort.mk_uninterpreted_s z3_ctx $1 }
  | sort RIGHTARROW sort { Z3.Z3Array.mk_sort z3_ctx $1 $3 }
  | L_PAREN sort R_PAREN { $2 }
;
sort_list_ne:
  | sort COMMA sort_list_ne  {  $1::$3 }
  | sort { [$1] }
;
sort_list:
  | sort_list_ne { $1 }
  | /*empty*/  { [] }
;


/* TODO: type-checking */
atomic_term:
  | variable { $1 }
  | LLTYPE LLBVTYPE { mk_bv_type $2 }
  | LLTYPE LLHALF { mk_fp_type "half" }
  | LLTYPE LLFLOAT { mk_fp_type "float" }
  | LLTYPE LLDOUBLE { mk_fp_type "double" }
  | LLTYPE STRING_CONSTANT { mk_named_type $2 }
  | LLBVTYPE INTEGER { mk_bv $1 $2  }
  | INT INTEGER { mk_int (int_of_string $2)  }
  | LLTYPE L_BRACKET term CROSS term R_BRACKET { mk_array_type $3 $5 }
  | sort L_LTBRACE term_list R_BRACEGT { mk_struct $1 $3 }
  | sort L_BRACE term_list R_BRACE { mk_struct $1 $3 }
  | L_BRACKET term_list R_BRACKET { mk_array_val $2 }
  | PUREIDENTIFIER L_PAREN term_list R_PAREN { lookup_op $1 $3 }
  | IDENTIFIER L_PAREN term_list R_PAREN { lookup_op $1 $3 }
  | EMP  { Syntax.mk_emp }
  | FALSE { Syntax.mk_false }
  | L_PAREN term R_PAREN { $2 }
;
term:
  | atomic_term { $1 }
  | term STAR term { Syntax.mk_star $1 $3 }
  | term OROR term { Syntax.mk_or $1 $3 }
  | term BVUDIV term { Z3.BitVector.mk_udiv z3_ctx $1 $3 }
  | term BVSDIV term { Z3.BitVector.mk_sdiv z3_ctx $1 $3 }
  | term BVSUB term { Z3.BitVector.mk_sub z3_ctx $1 $3 }
  | term BVADD term { Z3.BitVector.mk_add z3_ctx $1 $3 }
  | term BVMUL term { Z3.BitVector.mk_mul z3_ctx $1 $3 }
  | atomic_term NOT_EQUALS atomic_term { Syntax.mk_distinct [$1; $3] }
  | atomic_term EQUALS atomic_term { Syntax.mk_eq $1 $3 }
  | atomic_term cmpop atomic_term { $2 $1 $3 }
/*  | atomic_term binop atomic_term { $2 $1 $3 } */
;
term_list_ne:
  | term {$1::[]}
  | term COMMA term_list_ne { $1::$3 }
;
term_list:
  | /*empty*/  {[]}
  | term_list_ne {$1}
;

/* Specifications */

modifies:
  | /* empty */ { [] }
  | L_PAREN variable_list R_PAREN { $2 }
;

triple:
  | L_BRACE term R_BRACE modifies L_BRACE term R_BRACE
    { { Core.pre = $2; modifies = $4; post = $6 } }
;

spec:
  | /* empty */ { C.TripleSet.create 0 }
  | spec triple { C.TripleSet.add $1 $2; $1 }
  | spec BVADD triple { C.TripleSet.add $1 $3; $1 }
;

/* Rules */

sequent_rule:
  | RULE rule_flags IDENTIFIER sort_vars struct_vars COLON sequent
      with_clause
    IF sequent_list SEMICOLON
    { let s = Calculus.Sequent_rule
        { Calculus.seq_name = $3
        ; seq_pure_check = fst $8
        ; seq_fresh_in_expr = snd $8
        ; seq_goal_pattern = $7
        ; seq_subgoal_pattern = $10
        ; seq_priority = fst $2
        ; seq_flags = snd $2 } in
      let srs =
        match $5 with
        | None -> [s]
        | Some (st,i) -> Rulegen.struct_rule st i s in
      srs >>= Rulegen.polymorphic_rule $4 }
;

rewrite_rule:
  | REWRITERULE IDENTIFIER sort_vars struct_vars COLON
      term RIGHTARROW term SEMICOLON
    { let rw = Calculus.Rewrite_rule
        { Calculus.rw_name = $2
        ; rw_from_pattern = $6
        ; rw_to_pattern = $8 } in
      let rrs = match $4 with
        | None -> [rw]
        | Some (st,i) -> Rulegen.struct_rule st i rw in
      rrs >>= Rulegen.polymorphic_rule $3 }
;

equiv_rule:
  | EQUIVRULE rule_flags IDENTIFIER sort_vars struct_vars COLON
      term BIMP term SEMICOLON
      { let seqs = Calculus.mk_equiv_rule $3 (fst $2) (snd $2) $7 $9 in
        let srs = match $5 with
          | None -> seqs
          | Some (st,i) -> seqs >>= (Rulegen.struct_rule st i) in
        srs >>= Rulegen.polymorphic_rule $4 }
;

rule_flag:
  | NO_BACKTRACK { (None,Calculus.rule_no_backtrack) }
  | ABDUCT { (None,Calculus.rule_abduct) }
  | INCONSISTENT { (None,Calculus.rule_inconsistency) }
  | PRIORITY L_PAREN INTEGER R_PAREN { (Some (int_of_string $3), 0) }
;
rule_flags:
  | /* empty */ { (Calculus.default_rule_priority,0) }
  | rule_flags rule_flag { (from_option (fst $1) (fst $2), snd $1 lor snd $2) }
;

struct_vars:
  | /* empty */ { None }
  | L_BRACE IDENTIFIER COMMA IDENTIFIER R_BRACE
      { Some ($2, $4) }
;

sort_vars:
   | /* empty */ { [] }
   | L_BRACKET identifier_list_ne R_BRACKET { $2 }
;

sequent:
  | term BAR term VDASH term
    { { Calculus.frame = $1
      ; hypothesis = $3
      ; conclusion = $5 } }
;
sequent_list:
  | /* empty */ { [] }
  | sequent_list_ne { $1 }
;
sequent_list_ne:
  | sequent { [ $1 ] }
  | sequent COMMA sequent_list_ne { $1 :: $3 }
;

sidecondition:
  | PURECHECK term { (Some $2, None) }
  | FRESH variable IN term { (None,  Some ($2,$4)) }
;
sidecondition_list_ne:
  | sidecondition { (option [] (fun x -> [x]) (fst $1),
		     option [] (fun x -> [x]) (snd $1)) }
  | sidecondition SEMICOLON sidecondition_list_ne
      { (option (fst $3) (flip ListH.cons (fst $3)) (fst $1),
	 option (snd $3) (flip ListH.cons (snd $3)) (snd $1)) }
;
with_clause:
  | /* empty */ { ([],[]) }
  | WITH sidecondition_list_ne { $2 }
;

/* Input files */

proc_args:
  | /* empty */ { [] }
  | L_PAREN variable_list R_PAREN { $2 }
;

proc_rets:
  | /* empty */ { [] }
  | RETURNS L_PAREN variable_list R_PAREN { $3 }

procedure:
  | PROCEDURE IDENTIFIER proc_args proc_rets spec SEMICOLON
    { { C.proc_name = $2
      ; proc_spec = $5
      ; proc_ok = true
      ; proc_body = None
      ; proc_args = $3
      ; proc_rets = $4
      ; proc_rules = { C.calculus = []; abstraction = [] } } }
;

/* type declarations */
pred_decl:
   | PREDICATE IDENTIFIER sort_vars L_PAREN sort_list R_PAREN SEMICOLON
    { let op args =
	(* TODO: check [args] sorts against $5 *)
	let arg_sorts = List.map Z3.Expr.get_sort args in
	let f = Z3.FuncDecl.mk_func_decl_s z3_ctx $2 arg_sorts bool_sort in
	Z3.FuncDecl.apply f args in
      register_op $2 op }
;

func_decl:
   | FUNCTION sort IDENTIFIER sort_vars L_PAREN sort_list R_PAREN SEMICOLON
    { let op args =
	(* TODO: check [args] sorts against $5 *)
	let arg_sorts = List.map Z3.Expr.get_sort args in
	let f = Z3.FuncDecl.mk_func_decl_s z3_ctx $3 arg_sorts $2 in
	Z3.FuncDecl.apply f args in
      register_op $3 op }
;

import_entry:
  | IMPORT STRING_CONSTANT SEMICOLON  { $2 }
;

normal_entry:
  | procedure { [ParserAst.Procedure $1] }
  | sequent_rule { List.map (fun x -> ParserAst.CalculusRule x) $1 }
  | equiv_rule { List.map (fun x -> ParserAst.CalculusRule x) $1 }
  | rewrite_rule { List.map (fun x -> ParserAst.CalculusRule x) $1 }
  | pred_decl { [] }
  | func_decl { [] }
;

entry:
  | import_entry { [Load.ImportEntry $1] }
  | normal_entry { List.map (fun x -> Load.NormalEntry x) $1 }
;

entry_list:
  | /* empty */ { [] }
  | entry entry_list { $1 @ $2 }
;

file:
  | entry_list EOF { $1 }
;

%% (* trailer *)
