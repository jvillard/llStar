(*** Smtexpression: from LLVM datatypes to SMT-LIB ones *)

open Format
(* LLVM modules *)
open Llvm
open Llvm_target.DataLayout
open TypeKind
(* coreStar modules *)
open Smt
open Smtsyntax
(* llStar modules *)
open Llutils


let rec sexp_of_lltype t = match (classify_type t) with
  | Void
  | Float
  | Half
  | Double
  | X86fp80
  | X86_mmx
  | Fp128
  | Ppc_fp128
  | Label -> "Int"
  | Integer -> Printf.sprintf "(_ BitVec %d)" (integer_bitwidth t)
  | Function ->
    let ret_type = return_type t in
    let par_types = param_types t in
    Printf.sprintf "(%s) %s"
      (sexp_of_lltype_array par_types) (sexp_of_lltype ret_type)
  | Struct -> id_munge (string_of_struct t)
  | Array -> "Int" (* TODO: arrays *)
  | Pointer -> Printf.sprintf "(_ BitVec %Ld)" (size_in_bits t !lltarget)
  | Vector -> "Int" (* TODO: vectors *)
  | Metadata -> "Int" (* probably never get there? assert false? *)
and sexp_of_lltype_array ta =
  Array.fold_left (fun s st -> s^" "^st) "" (Array.map sexp_of_lltype ta)

let smtname_of_struct t =
  id_munge (string_of_struct t)

let smtconstr_of_struct s =
  id_munge ("mk_"^s)

let smtfield_of_struct s i =
  id_munge (Printf.sprintf "%s_fld%d" s i)

(** builds SMT-LIB declarations for the record datatype associated to a struct *)
let declare_struct_type t =
  let struct_name = smtname_of_struct t in
  let struct_constr = smtconstr_of_struct (string_of_struct t) in
  let elts = struct_element_types t in
  let field_constr i = smtfield_of_struct (string_of_struct t) i in
  let elt_sexps = Array.mapi (fun i t ->
    Printf.sprintf "(%s %s)" (field_constr i) (sexp_of_lltype t)) elts in
  let fields = Array.fold_left (fun s st -> s^" "^st) "" elt_sexps in
  let decl = Printf.sprintf "(declare-datatypes () ((%s (%s %s))))"
      struct_name struct_constr fields in
  smt_declare_datatype struct_name decl;
  let match_native_struct f name args =
    if id_munge name = struct_constr then (struct_constr, args)
    else f name args in
  add_native_op match_native_struct;
  Array.iteri (fun i t ->
    let fldcons = field_constr i in
    let match_native_field_op f name args =
      if id_munge name = fldcons then (fldcons, args)
      else f name args in
    add_native_op match_native_field_op)
    elts
