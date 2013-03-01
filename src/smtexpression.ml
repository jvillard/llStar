(*** Smtexpression: from LLVM datatypes to SMT-LIB ones *)

open Format
(* LLVM modules *)
open Llvm
open Llvm_target
open TypeKind
(* coreStar modules *)
open Smt
(* LStar modules *)
open Llutils


let rec sexp_of_lltype t = match (classify_type t) with
  | Void
  | Float
  | Half
  | Double
  | X86fp80
  | Fp128
  | Ppc_fp128
  | Label -> "Int"
  | Integer -> Printf.sprintf "(_ BitVec %d)" (integer_bitwidth t)
  | Function ->
    let ret_type = return_type t in
    let par_types = param_types t in
    Printf.sprintf "(%s) %s"
      (sexp_of_lltype_array par_types) (sexp_of_lltype ret_type)
  | Struct -> string_of_struct t
  | Array -> "Int" (* TODO: arrays *)
  | Pointer -> Printf.sprintf "(_ BitVec %Ld)" (size_in_bits !lltarget t)
  | Vector -> "Int" (* TODO: vectors *)
  | Metadata -> "Int" (* probably never get there? assert false? *)
and sexp_of_lltype_array ta =
  Array.fold_left (fun s st -> s^" "^st) "" (Array.map sexp_of_lltype ta)

(** builds SMT-LIB declaration of the record datatype associated to a struct *)
let declare_struct_type t =
  let name = string_of_struct t in
  let elts = struct_element_types t in
  let elt_sexps = Array.mapi (fun i t ->
    Printf.sprintf "(%s-fld%d %s)" name i (sexp_of_lltype t)) elts in
  let fields = Array.fold_left (fun s st -> s^" "^st) "" elt_sexps in
  let decl =
    Printf.sprintf "(declare-datatypes () ((%s (mk-%s %s))))" name name fields in
  smt_declare_datatype name decl
