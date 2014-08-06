(********************************************************
   This file is part of coreStar
        src/parsing/lexer.mll
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   coreStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)

{

open Lexing
open Logic_parser

type error =
  | Illegal_character of char
  | Unterminated_comment
  | Unterminated_literal

exception Error of error * Lexing.lexbuf

let nest_start_pos = ref []
let nest x = nest_start_pos := x.lex_curr_p :: !nest_start_pos
let unnest x = nest_start_pos := List.tl !nest_start_pos; !nest_start_pos <> []

let string_of_position p =
  let r = Buffer.create 10 in
  if p.pos_fname <> "" then begin
    Buffer.add_string r p.pos_fname; Buffer.add_char r ':'
  end;
  Printf.bprintf r "%d:%d" p.pos_lnum (p.pos_cnum - p.pos_bol);
  Buffer.contents r

let error_message e lb =
  match e with
    Illegal_character c ->
      Printf.sprintf "%s: illegal character: %s\n"
        (string_of_position lb.lex_curr_p) (Char.escaped c)
  | Unterminated_comment ->
      Printf.sprintf "%s: unterminated comment\n"
        (string_of_position (List.hd !nest_start_pos))
  | Unterminated_literal ->
      Printf.sprintf "%s: unterminated literal\n"
        (string_of_position (List.hd !nest_start_pos))
  
(* [kwd_or_else d s] is the token corresponding to [s] if there is one,
  or the default [d] otherwise. *)
let kwd_or_else = 
  let keyword_table = Hashtbl.create 53 in
  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok) [
    "abduct", ABDUCT;
    "abstraction", ABSRULE;
    "bool", BOOL;
    "emp", EMP;
    "fresh", FRESH;
    "False", FALSE;
    "function", FUNCTION;
    "global", GLOBAL;
    "if", IF;
    "import", IMPORT;
    "in", IN;
    "inconsistent", INCONSISTENT;
    "int", INT;
    "nobacktrack", NO_BACKTRACK;
    "predicate", PREDICATE;
    "priority", PRIORITY;
    "procedure", PROCEDURE;
    "purecheck", PURECHECK;
    "returns", RETURNS;
    "rewrite", REWRITERULE;
    "rule", RULE;
    "True", TRUE;
    "with", WITH;
    "without", WITHOUT;
    (* LLVM stuff *)
    "void", LLVOID;
    "half", LLHALF;
    "float", LLFLOAT;
    "double", LLDOUBLE;
    "lltype", LLTYPE;
    "named", LLNAMED;
    "lljump", LLJUMP;

  ];
  fun d s ->
  try Hashtbl.find keyword_table s with Not_found -> d

let strip_first_char s = String.sub s 1 (String.length s - 1)
}


(* ====================================================================== *)

let  dec_digit = ['0' - '9']

let  not_cr_lf = [ ^ '\010' '\013']

let  alpha_char = ['a' - 'z'] | ['A' - 'Z']
  
let  simple_id_char = alpha_char | dec_digit | '_' | '.' | '$' | '%'

let  first_id_char = alpha_char | '_' | '$'

let  string_char = ['\000' - '\033'] | ['\035' - '\091'] | ['\093' - '\127']   
let  string_char_no_sharp = [^'#']

let  line_comment = "//" not_cr_lf*

let  blank = (' ' | '\009')+

let  ignored_helper = (blank | line_comment)+

let  newline = ('\013' | '\010' | "\010\013")

let  at_identifier = '@' (simple_id_char | ':')*

let identifier = alpha_char simple_id_char*
let polysort_identifier = '\'' alpha_char simple_id_char*

(* regexp taken from LLVM language reference manual *)
let lllidentifier = '%' (['a'-'z''A'-'Z'] | '$' | '.' | '_')(['a'-'z''A'-'Z''0'-'9'] | '$' | '.' | '_')*
let llgidentifier = '@' (['a'-'z''A'-'Z'] | '$' | '.' | '_')(['a'-'z''A'-'Z''0'-'9'] | '$' | '.' | '_')*
let lllescidentifier = '%' '"' [^'"']+ '"'
let llgescidentifier = '@' '"' [^'"']+ '"' 

let llinteger_type = 'i' ['1'-'9']['0'-'9']*

let lidentifier = '_' simple_id_char*
let tpidentifier = '?' simple_id_char*
let vpidentifier = '^' simple_id_char*
let pureidentifier = '!' simple_id_char*

let integer = ['0'-'9']+

rule token = parse
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | "/*" { nest lexbuf; comment lexbuf; token lexbuf } 
  | ignored_helper  { token lexbuf }
  | "|" { BAR }
  | "<=>" { BIMP }  
  | "+" { BVADD }
  | "**" { BVMUL }
  | "/s" { BVSDIV }
  | ">=s" { BVSGE }
  | ">s" { BVSGT }
  | "<=s" { BVSLE }
  | "<s" { BVSLT }
  | "-" { BVSUB }
  | "/u" { BVUDIV }
  | ">=u" { BVUGE }
  | ">u" { BVUGT }
  | "<=u" { BVULE }
  | "<u" { BVULT }
  | ">=" { CMP_GE }
  | ">" { CMP_GT }
  | "<=" { CMP_LE }
  | "<" { CMP_LT }
  | ":" { COLON }
  | "," { COMMA }
  | " x " { CROSS }
  | "=" { EQUALS }
  | "{" { L_BRACE }
  | "[" { L_BRACKET }
  | "<{" { L_LTBRACE }
  | "(" { L_PAREN }
  | "!=" { NOT_EQUALS }
  | "||" { OROR }
  | "}" { R_BRACE }
  | "}>" { R_BRACEGT }
  | "]" { R_BRACKET }
  | ")" { R_PAREN }
  | ";" { SEMICOLON }
  | "*" { STAR }
  | "|-" { VDASH }
  | "->" { RIGHTARROW }
  | eof { EOF }

  | llinteger_type as s { LLBVTYPE (int_of_string (strip_first_char s)) }
  | lllidentifier as s { PLIDENTIFIER (strip_first_char s) }
  | lllescidentifier as s { PLIDENTIFIER (strip_first_char s) }
  | llgidentifier as s { PGIDENTIFIER (strip_first_char s) }
  | llgescidentifier as s { PGIDENTIFIER (strip_first_char s) }
  | lidentifier as s { LIDENTIFIER (strip_first_char s) }
  | tpidentifier as s { TPIDENTIFIER (strip_first_char s) }
  | vpidentifier as s { VPIDENTIFIER (strip_first_char s) }
  | pureidentifier as s { PUREIDENTIFIER (strip_first_char s) }
  | identifier as s { kwd_or_else (IDENTIFIER s) s }
  | polysort_identifier as s { kwd_or_else (POLYSORT_IDENTIFIER s) s }
  | '#' (string_char_no_sharp* as s) '#' { IDENTIFIER s }
  (* Lexing integers and strings according to SMT-LIB 2.0. *)
  | integer as s { INTEGER s }
  | '"' { nest lexbuf; STRING_CONSTANT (lex_string (Buffer.create 0) lexbuf) }

  | _ { failwith (error_message (Illegal_character ((Lexing.lexeme lexbuf).[0])) lexbuf)}
and comment = parse
  | "/*"  { nest lexbuf; comment lexbuf }
  | "*/"  { if unnest lexbuf then comment lexbuf }
  | newline  { Lexing.new_line lexbuf; comment lexbuf }
  | eof      { failwith (error_message Unterminated_comment lexbuf)}
  | _     { comment lexbuf; }
and lex_string b = parse
  | "\\\\" { Buffer.add_char b '\\'; lex_string b lexbuf }
  | "\\\"" { Buffer.add_char b '"'; lex_string b lexbuf }
  | '"' { let r = unnest lexbuf in assert (not r); Buffer.contents b }
  | eof { failwith (error_message Unterminated_literal lexbuf) }
  | _ as c { Buffer.add_char b c; lex_string b lexbuf }


(* ====================================================================== *)

{ (* trailer *)
}
