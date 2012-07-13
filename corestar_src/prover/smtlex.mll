(********************************************************
   This file is part of coreStar 
	src/prover/smtlex.mll
   Release 
        $Release$
   Version 
        $Rev$
   $Copyright$
   
   coreStar is distributed under a BSD license,  see, 
      LICENSE.txt
 ********************************************************)


{
  open Format
  open Smtparse
}

let  quote = '\''

let  escapable_char = '\\' | ' ' | quote | '.' | '#' | '\"' | 'n' | 't' | 'r' | 'b' | 'f'
let  escape_char = '\\' escapable_char 

let  string_char = escape_char | ['\000' - '\033'] | ['\035' - '\091'] | ['\093' - '\127']   

let string_constant = '"' string_char* '"'

rule token = parse
  | [' ' '\t' '\n' '\r']
                     { token lexbuf }     (* skip blanks *)
  | '('              { LPAREN }
  | ')'              { RPAREN }
  | "unsupported"    { UNSUPPORTED }
  | "error"          { ERROR }
  | "sat"            { SAT }
  | "unsat"          { UNSAT }
  | "unknown"        { UNKNOWN }
  | string_constant  { STRING_CONSTANT (Lexing.lexeme lexbuf) }
  | eof              { raise End_of_file }
  | _ as c           { eprintf "@[offending character %x@." (Char.code c); assert false }
