/********************************************************
 * Original file part of coreStar, distributed under a BSD
 * license LICENSE_coreStar.txt
 ********************************************************/

%{ (* header *)

open Corestar_std
open Format

module C = Core

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

let mk_2 f = function
  | [a; b] -> f a b
  | _ -> assert false

let mk_3 f = function
  | [a; b; c] -> f a b c
  | _ -> assert false

let ops = [
  (* spatial predicates *)
  ("pointer", mk_3 mkPointer);
  ("malloced", mk_2 mkMalloced);
  ("NULL", mk_0 mkNullPtr);
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
%token ABDUCTION
%token ABSRULE
%token ABSTRACT
%token AND
%token ASSIGN
%token AXIOMS
%token BANG
%token BIMP
%token BOOL
%token BVUDIV
%token BVSDIV
%token BVSUB
%token BVADD
%token BVMUL
%token BVSLE
%token BVULE
%token BVSLT
%token BVULT
%token BVSGE
%token BVUGE
%token BVSGT
%token BVUGT
%token CMP_LE
%token CMP_LT
%token CMP_GE
%token CMP_GT
%token COLON
%token COLON_EQUALS
%token COMMA
%token CONSTRUCTOR
%token CROSS
%token DASHV
%token DOT
%token EMP
%token END
%token EOF
%token EQUALS
%token EQUIV
%token FALSE
%token FRAME
%token GARBAGE
%token GLOBAL
%token GOTO
%token <string> IDENTIFIER
%token IF
%token IMP
%token IMPLICATION
%token IMPORT
%token INCONSISTENCY
%token <string> INTEGER
%token L_BRACE
%token L_LTBRACE
%token L_BRACKET
%token L_PAREN
%token LABEL
%token LEADSTO
%token <string> LIDENTIFIER
%token LLTYPE
%token LLMEM
%token LLJUMP
%token LLVOID
%token LLHALF
%token LLFLOAT
%token LLDOUBLE
%token LLNAMED
%token LLDOUBLE
%token <int> LLBVTYPE
%token STAR
%token NOP
%token NOT_EQUALS
%token NOTIN
%token NOTINCONTEXT
%token <string> PGIDENTIFIER
%token <string> PLIDENTIFIER
%token POINTSTO
%token PROCEDURE
%token PUREGUARD
%token <string> PUREIDENTIFIER
%token OROR
%token QUESTIONMARK
%token QUOTE
%token R_BRACE
%token R_BRACEGT
%token R_BRACKET
%token R_PAREN
%token REWRITERULE
%token RULE
%token SEMICOLON
%token SLASH
%token <string> STRING_CONSTANT
%token <string> TPIDENTIFIER
%token TRUE
%token TYPE
%token VDASH
%token <string> VPIDENTIFIER
%token WAND
%token WHERE
%token WITH
%token WITHOUT


/* === associativity and precedence === */

%left OROR
%left STAR

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
identifier:
  | IDENTIFIER { $1 }
;

binop:
  | BVUDIV { Z3.BitVector.mk_udiv z3_ctx }
  | BVSDIV { Z3.BitVector.mk_sdiv z3_ctx }
  | BVSUB { Z3.BitVector.mk_sub z3_ctx }
  | BVADD { Z3.BitVector.mk_add z3_ctx }
  | BVMUL { Z3.BitVector.mk_mul z3_ctx }
;

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
  | LLTYPE { lltype_sort }
  | LLJUMP { jump_sort }
  | LLVOID { void_sort }
  | LLBVTYPE { bv_sort $1 }
  | LLMEM { llmem_sort }
  | LLNAMED L_PAREN variable R_PAREN { named_sort $3 }
  | L_LTBRACE sort_list R_BRACEGT { struct_as_fields_sort $2 }
  | L_BRACE sort_list R_BRACE { struct_as_fields_sort $2 }
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
term:
  | variable { $1 }
  | LLTYPE LLVOID { mkVoidType }
  | LLTYPE LLBVTYPE { mkBVType $2 }
  | LLTYPE LLHALF { mkFPType "half" }
  | LLTYPE LLFLOAT { mkFPType "float" }
  | LLTYPE LLDOUBLE { mkFPType "double" }
  | LLTYPE LLNAMED L_PAREN variable R_PAREN { mkNamedType $4 }
  | LLBVTYPE INTEGER { mkBV $1 $2  }
  | sort L_LTBRACE term_list R_BRACEGT { mkStruct $1 $3 }
  | sort L_BRACE term_list R_BRACE { mkStruct $1 $3 }
  | identifier L_PAREN term_list R_PAREN { lookup_op $1 $3 }
  | term binop term { $2 $1 $3 }
  | L_PAREN term R_PAREN { $2 }
;
term_list_ne:
  | term {$1::[]}
  | term COMMA term_list_ne { $1::$3 }
;
term_list:
  | /*empty*/  {[]}
  | term_list_ne {$1}
;

/* Formulae */

formula:
  | /*empty*/  { Syntax.mk_emp }
  | EMP  { Syntax.mk_emp }
  | FALSE { Syntax.mk_false }
  | term POINTSTO term term { mkPointer $1 $3 $4 }
  | PUREIDENTIFIER L_PAREN term_list R_PAREN { lookup_op $1 $3 }
  | identifier L_PAREN term_list R_PAREN { lookup_op $1 $3 }
  | formula STAR formula { Syntax.mk_star $1 $3 }
  | formula OROR formula { Syntax.mk_or $1 $3 }
  | term NOT_EQUALS term { Syntax.mk_distinct [$1; $3] }
  | term EQUALS term { Syntax.mk_eq $1 $3 }
  | term cmpop term { $2 $1 $3 }
  | L_PAREN formula R_PAREN { $2 }
  | BOOL TPIDENTIFIER { Syntax.mk_bool_tpat $2 }
;

/* Specifications */

modifies:
  | /* empty */ { [] }
  | L_PAREN variable_list R_PAREN { $2 }
;

/* FIXME: rubbish syntax */
in_vars:
  | /* empty */ { [] }
  | L_BRACKET variable_list R_BRACKET { $2 }
;

/* FIXME: rubbish syntax */
out_vars:
  | /* empty */ { [] }
  | SLASH variable_list_ne SLASH { $2 }
;

/* FIXME: rubbish syntax */
triple:
  | L_BRACE formula R_BRACE modifies L_BRACE out_vars formula R_BRACE in_vars
    { { Core.pre = $2; modifies = $4; post = $7; out_vars = $6; in_vars = $9 } }
;

spec:
  | /* empty */ { C.TripleSet.create 0 }
  | spec triple { C.TripleSet.add $1 $2; $1 }
  | spec BVADD triple { C.TripleSet.add $1 $3; $1 }
;

/* Rules */

calculus_rule:
  | RULE IDENTIFIER COLON sequent
    calculus_sidecondition
    IF sequent_list SEMICOLON
    { { Calculus.schema_name = $2
      ; side_condition = $5
      ; goal_pattern = $4
      ; subgoal_pattern = $7 } }
;

sequent:
  | formula VDASH formula
    { { Calculus.frame = Syntax.mk_emp
      ; hypothesis = $1
      ; conclusion = $3 } }
;

calculus_sidecondition:
  | /* empty for now, TODO */ { Syntax.mk_emp }
;

sequent_list:
  | /* empty */ { [] }
  | sequent_list_ne { $1 }
;

sequent_list_ne:
  | sequent { [ $1 ] }
  | sequent COMMA sequent_list_ne { $1 :: $3 }
;

/* Input files */

proc_lhs:
  | L_PAREN variable_list R_PAREN COLON_EQUALS { $2 }
  | /* empty */  { [] }
;

proc_args:
  | /* empty */ { [] }
  | L_PAREN variable_list R_PAREN { $2 }
;
procedure:
  | PROCEDURE proc_lhs IDENTIFIER proc_args COLON spec
    { { C.proc_name = $3
      ; proc_spec = $6
      ; proc_ok = true
      ; proc_body = None
      ; proc_params = $4
      ; proc_rets = $2
      ; proc_rules = { C.calculus = []; abstraction = [] } } }
;

import_entry:
  | IMPORT STRING_CONSTANT SEMICOLON  { $2 }
;

normal_entry:
  | procedure { ParserAst.Procedure $1 }
  | GLOBAL variable_list_ne SEMICOLON { ParserAst.Global $2 }
  | calculus_rule { ParserAst.CalculusRule $1 }
;

entry:
  | import_entry { Load.ImportEntry $1 }
  | normal_entry { Load.NormalEntry $1 }
;

entry_list:
  | /* empty */ { [] }
  | entry entry_list { $1 :: $2 }
;

file:
  | entry_list EOF { $1 }
;

%% (* trailer *)
