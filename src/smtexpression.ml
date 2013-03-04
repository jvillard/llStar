(*** Smtexpression: from LLVM datatypes to SMT-LIB ones *)

open Format
(* LLVM modules *)
open Llvm
open Llvm_target
open TypeKind
(* coreStar modules *)
open Smt
open Smtsyntax
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

let rec smttype_of_lltype t = match (classify_type t) with
  | Void
  | Float
  | Half
  | Double
  | X86fp80
  | Fp128
  | Ppc_fp128
  | Label -> SType_int
  | Integer -> SType_bv (string_of_int (integer_bitwidth t))
  | Function ->
    let ret_type = return_type t in
    let par_smttypes_array = Array.map smttype_of_lltype (param_types t) in
    let par_smttypes =  Array.to_list par_smttypes_array in
    SType_fun ([par_smttypes, smttype_of_lltype ret_type])
  | Struct -> SType_type (string_of_struct t)
  | Array -> SType_int (* TODO: arrays *)
  | Pointer -> SType_bv (Int64.to_string (size_in_bits !lltarget t))
  | Vector -> SType_int (* TODO: vectors *)
  | Metadata -> SType_int (* probably never get there? assert false? *)
  

let smtname_of_struct t =
  id_munge (string_of_struct t)

let smtconstr_of_struct t =
  id_munge ("mk_"^(string_of_struct t))

let smtfield_of_struct t i =
  id_munge (Printf.sprintf "%s-fld%d" (string_of_struct t) i)

(** builds SMT-LIB declarations for the record datatype associated to a struct *)
let declare_struct_type t =
  let struct_name = smtname_of_struct t in
  let struct_t = SType_type struct_name in
  let struct_constr = smtconstr_of_struct t in
  let elts = struct_element_types t in
  let field_constr i = smtfield_of_struct t i in
  let elt_sexps = Array.mapi (fun i t ->
    Printf.sprintf "(%s %s)" (field_constr i) (sexp_of_lltype t)) elts in
  let fields = Array.fold_left (fun s st -> s^" "^st) "" elt_sexps in
  let decl = Printf.sprintf "(declare-datatypes () ((%s (%s %s))))"
      struct_name struct_constr fields in
  smt_declare_datatype struct_name decl;
  let elts_t = Array.to_list
    (Array.mapi (fun i t ->
      let field_t = smttype_of_lltype t in
      add_native_op (field_constr i) (field_constr i) (SType_fun [([struct_t],field_t)]);
      smttype_of_lltype t) elts) in
  add_native_op ("mk_"^(string_of_struct t)) struct_constr (SType_fun [(elts_t, struct_t)])
