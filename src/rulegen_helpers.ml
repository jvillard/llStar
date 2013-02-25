(*** Rulegen_helpers: predicates for bitcode structures *)

(* LLVM modules *)
open Llvm
(* coreStar modules *)
open Psyntax
(* LStar modules *)
open Llexpression
open Llutils

let mk_simple_seq_rule name (pre_lhs,pre_rhs) (post_lhs, post_rhs) =
  let conclusion = (mkEmpty, post_lhs, post_rhs, mkEmpty) in
  let premise = (mkEmpty, pre_lhs, pre_rhs, mkEmpty) in
  (conclusion, [[premise]], name, ([], []), [])

let gen_seq_rules_of_equiv name (equiv_left, equiv_right) =
  mk_simple_seq_rule (name^"_left") (equiv_right, mkEmpty) (equiv_left, mkEmpty)::
  mk_simple_seq_rule (name^"_right") (mkEmpty, equiv_right) (mkEmpty, equiv_left)::[]

(** the physical offset inside the struct, as a logical expression *)
let offset_of_field struct_t i =
  let offset = Llvm_target.offset_of_element !lltarget struct_t i in
  Arg_op("numeric_const", [Arg_string(Int64.to_string offset)])

let offset_of_field_end struct_t i =
  let offset = Llvm_target.offset_of_element !lltarget struct_t i in
  let field_type = Array.get (struct_element_types struct_t) i in
  let field_size = Llvm_target.store_size !lltarget field_type in
  let field_end = Int64.add offset field_size in
  Arg_op("numeric_const", [Arg_string(Int64.to_string field_end)])

(* a few definitions to make the rule definitions more readable *)
let args_sizeof_field struct_t i =
  args_sizeof !lltarget (Array.get (struct_element_types struct_t) i)

let mk_padding_of_field struct_t i root =
  let offset = Llvm_target.offset_of_element !lltarget struct_t i in
  let next_offset =
    if i = Array.length (struct_element_types struct_t) - 1 then
      Llvm_target.store_size !lltarget struct_t
    else Llvm_target.offset_of_element !lltarget struct_t (i+1) in
  let elt_size = Llvm_target.store_size !lltarget
    (Array.get (struct_element_types struct_t) i) in
  let pad_size = Int64.sub next_offset (Int64.add offset elt_size) in
  if pad_size = Int64.zero then None
  else
    let offset = Arg_string (Int64.to_string (Int64.add offset elt_size)) in
    let pad_addr = Arg_op("builtin_plus", [root; offset]) in
    Some (mkSPred ("padding", [pad_addr; Arg_string(Int64.to_string pad_size)]))

let mk_field_pointer struct_t i root value =
  if i = 0 then
    mkPointer root (args_sizeof_field struct_t i) value
  else
    let offset = Arg_op ("builtin_plus", [root; offset_of_field struct_t i]) in
    mkPointer offset (args_sizeof_field struct_t i) value

let mk_padded_field_pointer struct_t i root value =
  let field_pointer = mk_field_pointer struct_t i root value in
  match mk_padding_of_field struct_t i root with
    | None -> field_pointer
    | Some pad -> mkStar field_pointer pad

let mk_unfolded_struct struct_t root fields_values =
  let mk_field i subelt_t =
    let v = Array.get fields_values i in
    mk_padded_field_pointer struct_t i root v in
  let pointers = Array.mapi mk_field (struct_element_types struct_t) in
  Array.fold_left (fun f p -> mkStar f p) mkEmpty pointers
