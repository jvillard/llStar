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
let mk_0 f = function
  | [] -> f
  | _ -> assert false
let mk_1 f = function
  | [a] -> f a
  | _ -> assert false
let mk_2 f = function
  | [a; b] -> f a b
  | _ -> assert false
let mk_3 f = function
  | [a; b; c] -> f a b c
  | _ -> assert false

let ops = [
  (* spatial predicates *)
  ("pointer", mk_3 mk_pointer);
  ("malloced", mk_2 mk_malloced);
  ("NULL", mk_0 mk_null_ptr);
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
  ("exploded_struct", mk_3 mk_exploded_struct);
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

let hops = Hashtbl.create (List.length ops)

let () = List.iter (uncurry (Hashtbl.add hops)) ops

let register_op = Hashtbl.add hops
let lookup_op op args =
  try (Hashtbl.find hops op) args
  with Not_found ->
    if op = "as" then
      match args with
      | [e] -> let s = Z3.Expr.get_sort e in as_llmem s e
      | _ -> assert false
    else begin
      prerr_endline (Printf.sprintf "Undeclared operation %s" op);
      assert false
    end

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
%token GLOBAL
%token IF
%token IMPORT
%token IN
%token INCONSISTENT
%token LLDOUBLE
%token LLDOUBLE
%token LLFLOAT
%token LLHALF
%token LLJUMP
%token LLMEM
%token LLNAMED
%token LLTYPE
%token LLVOID
%token L_BRACE
%token L_BRACKET
%token L_LTBRACE
%token L_PAREN
%token NO_BACKTRACK
%token NODEDECL
%token NOT_EQUALS
%token OROR
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
;
variable_list_ne:
  |  variable    { [$1] }
  |  variable COMMA variable_list_ne  { $1 :: $3 }
;
variable_list:
  |  {[]}
  | variable_list_ne { $1 }
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
  | LLJUMP { jump_sort }
  | LLVOID { void_sort }
  | LLBVTYPE { bv_sort $1 }
  | LLTYPE { lltype_sort }
  | LLMEM { llmem_sort }
  | LLNAMED L_PAREN STRING_CONSTANT R_PAREN { sort_of_lltype (lltype_of_name $3) }
  | L_LTBRACE sort_list R_BRACEGT { struct_as_fields_sort $2 }
  | L_BRACE sort_list R_BRACE { struct_as_fields_sort $2 }
  | L_BRACKET term CROSS sort R_BRACKET { array_sort $4 }
  | sort STAR { pointer_sort }
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
  | LLTYPE TPIDENTIFIER { Syntax.mk_tpat lltype_sort $2 }
  | LLTYPE LLBVTYPE { mk_bv_type $2 }
  | LLTYPE LLHALF { mk_fp_type "half" }
  | LLTYPE LLFLOAT { mk_fp_type "float" }
  | LLTYPE LLDOUBLE { mk_fp_type "double" }
  | LLBVTYPE INTEGER { mk_bv $1 $2  }
  | LLTYPE LLNAMED L_PAREN STRING_CONSTANT R_PAREN { mk_named_type $4 }
  | LLTYPE L_BRACKET term CROSS term R_BRACKET { mk_array_type $3 $5 }
  | sort L_LTBRACE term_list R_BRACEGT { mk_struct $1 $3 }
  | sort L_BRACE term_list R_BRACE { mk_struct $1 $3 }
  | sort L_BRACKET term_list R_BRACKET { mk_array $1 $3 }
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
  | RULE rule_flags IDENTIFIER struct_vars COLON sequent
      with_clause
    IF sequent_list SEMICOLON
    { let seq = Calculus.Sequent_rule
        { Calculus.seq_name = $3
        ; seq_pure_check = fst $7
        ; seq_fresh_in_expr = snd $7
        ; seq_goal_pattern = $6
        ; seq_subgoal_pattern = $9
        ; seq_priority = fst $2
        ; seq_flags = snd $2 } in
    match $4 with
    | None -> [seq]
    | Some (st,i) -> Rulegen.gen_struct_rule st i seq }
;

rewrite_rule:
  | REWRITERULE IDENTIFIER struct_vars COLON
      atomic_term EQUALS atomic_term SEMICOLON
    { let rw =
        { Calculus.rw_name = $2
        ; rw_from_pattern = $5
        ; rw_to_pattern = $7 } in
      [rw] }
;

equiv_rule:
  | EQUIVRULE rule_flags IDENTIFIER struct_vars COLON
      term BIMP term SEMICOLON
      { let seqs = Calculus.mk_equiv_rule $3 (fst $2) (snd $2) $6 $8 in
        match $4 with
        | None -> seqs
        | Some (st,i) -> seqs >>= (Rulegen.gen_struct_rule st i) }
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
  | L_PAREN variable COMMA variable R_PAREN { Some ($2, $4) }
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

node_decl:
  | NODEDECL COLON STRING_CONSTANT L_PAREN int_list R_PAREN
      EQUALS IDENTIFIER SEMICOLON
      { { ParserAst.struct_name = $3
	; node_fields = $5
	; node_name = $8 } }
;

int_ne_list:
  | INTEGER { [int_of_string $1] }
  | INTEGER COMMA int_ne_list { int_of_string $1::$3 }
;

int_list:
  | /* empty */ { [] }
  | int_ne_list { $1 }
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

import_entry:
  | IMPORT STRING_CONSTANT SEMICOLON  { $2 }
;

/* FIXME: ugly list hack */
normal_entry:
  | procedure { [ParserAst.Procedure $1] }
  | GLOBAL variable_list_ne SEMICOLON { [ParserAst.Global $2] }
  | sequent_rule { List.map (fun x -> ParserAst.CalculusRule x) $1 }
  | equiv_rule { List.map (fun x -> ParserAst.CalculusRule x) $1 }
  | rewrite_rule { List.map (fun x -> ParserAst.CalculusRule (Calculus.Rewrite_rule x)) $1 }
  | node_decl { [ParserAst.NodeDecl $1] }
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
