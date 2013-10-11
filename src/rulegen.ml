(*** Rulegen: generate rules for the module's types and recursive data structures *)
(* This assumes that the pointer size is 64! TODO: handle any size! *)


open Format
(* LLVM modules *)
open Llvm
open TypeKind
(* coreStar modules *)
open Config
open Psyntax
(* llStar modules *)
open Llexpression
open Llutils


let mk_psequent matched lhs rhs abd = (matched,lhs,rhs,abd)
let mk_sequent_rule name premises_list conclusion without wheres =
  (conclusion, premises_list, name, without, wheres)
let mk_equiv_rule name guard lhs rhs without =
  List.map (function SeqRule s -> s | _ -> assert false)
    (expand_equiv_rules [EquivRule (name, guard, lhs, rhs, without)])

(** Make a rule with a single premise, no side conditions

    | [pre_lhs]  |- [pre_rhs]
    --------------------------
    | [post_lhs] |- [post_rhs]
*)
let mk_simple_sequent_rule name (pre_lhs,pre_rhs) (post_lhs, post_rhs) =
  let conclusion = (mkEmpty, post_lhs, post_rhs, mkEmpty) in
  let premise = (mkEmpty, pre_lhs, pre_rhs, mkEmpty) in
  (conclusion, [[premise]], name, ([], []), [])

let mk_simple_equiv_rule name equiv_left equiv_right =
  mk_equiv_rule name mkEmpty equiv_left equiv_right mkEmpty


(*** helper functions to define predicates for structs *)

let bits_of_bytes x =
  Int64.mul (Int64.of_int 8) x

let type_of_field struct_t i = Array.get (struct_element_types struct_t) i

(** the physical offset inside the struct, in bytes *)
let offset64_of_field struct_t i =
  Llvm_target.offset_of_element !lltarget struct_t i

let args_offset_of_field struct_t i =
  bvargs_of_int64 64 (offset64_of_field struct_t i)

let alloc_size64_of_field struct_t i =
  Llvm_target.abi_size !lltarget (type_of_field struct_t i)

let args_alloc_sizeof_field struct_t i =
  bvargs_of_int64 64 (alloc_size64_of_field struct_t i)

let bitsize64_of_field struct_t i =
  Llvm_target.size_in_bits !lltarget (type_of_field struct_t i)

let args_bitsize_of_field struct_t i =
  bvargs_of_int64 64 (bitsize64_of_field struct_t i)

(** what bit number starts element [i] of [struct_t] *)
let bitoffset64_of_field struct_t i =
  let bitsize_of_struct = Llvm_target.size_in_bits !lltarget struct_t in
  let bitoffset_of_field = bits_of_bytes (offset64_of_field struct_t i) in
  Int64.sub (Int64.sub bitsize_of_struct bitoffset_of_field) Int64.one

let string_args_bitoffset_of_field struct_t i =
  Arg_string (Int64.to_string (bitoffset64_of_field struct_t i))

(** what bit number ends element [i] of [struct_t] *)
let bitoffset64_of_field_end struct_t i =
  Int64.add (Int64.sub (bitoffset64_of_field struct_t i) (bitsize64_of_field struct_t i)) Int64.one

let string_args_bitoffset_of_field_end struct_t i =
  Arg_string (Int64.to_string (bitoffset64_of_field_end struct_t i))

let args_type_field struct_t i =
  args_of_type (type_of_field struct_t i)

let padsize64_of_field struct_t i =
  let offset = offset64_of_field struct_t i in
  let next_offset =
    if i = Array.length (struct_element_types struct_t) - 1 then
      Llvm_target.abi_size !lltarget struct_t
    else offset64_of_field struct_t (i+1) in
  let total_size = Int64.sub next_offset offset in
  let alloc_size = Llvm_target.abi_size !lltarget (type_of_field struct_t i) in
  Int64.sub total_size alloc_size

let args_padsize_of_field struct_t i =
  bvargs_of_int64 64 (padsize64_of_field struct_t i)

let is_single_struct t = match classify_type t with
  | Struct -> (Array.length (struct_element_types t) = 1) && (padsize64_of_field t 0 = Int64.zero)
  | _ -> false
    

let mk_struct_val_of_fields struct_t fields_values =
  let struct_constructor = "mk_"^(string_of_struct struct_t) in
  Arg_op (struct_constructor, Array.to_list fields_values)

(** extracts the value of field [i] from value [base_val] of type [struct_t]  *)
let mk_field_val_of_struct_val struct_t base_val i =
  let field_pred = (string_of_struct struct_t) ^ "_fld" ^ (string_of_int i)  in
  Arg_op (field_pred, [base_val])

let rec mk_struct_val_of_fields_descend_in_singletons struct_t fields_values =
  let mk_singletons i subelt_t =
    if is_single_struct subelt_t then
      mk_struct_val_of_fields_descend_in_singletons subelt_t (Array.of_list [Array.get fields_values i])
    else Array.get fields_values i in
  let struct_constructor = "mk_"^(string_of_struct struct_t) in
  Arg_op (struct_constructor, Array.to_list (Array.mapi mk_singletons (struct_element_types struct_t)))

let rec mk_field_val_of_struct_val_descend_in_singletons struct_t base_val i =
  let subelt_t = Array.get (struct_element_types struct_t) i in
  let field_pred = (string_of_struct struct_t) ^ "_fld" ^ (string_of_int i)  in
  let v = Arg_op (field_pred, [base_val]) in
  if is_single_struct subelt_t then
    mk_field_val_of_struct_val_descend_in_singletons subelt_t v 0
  else v

let mk_padding_of_field struct_t i root =
  let pad64_size = padsize64_of_field struct_t i in
  if pad64_size = Int64.zero then None
  else
    let elt_offset = offset64_of_field struct_t i in
    let pad_offset = Int64.add (alloc_size64_of_field struct_t i) elt_offset in
    let pad_addr = if pad_offset = Int64.zero then root
      else let args_off = bvargs_of_int64 64 pad_offset in
	   Arg_op("bvadd.64", [root; args_off]) in
    let args_pad_size = args_padsize_of_field struct_t i in
    Some (mkPadding pad_addr args_pad_size)

let mk_field_offset struct_t i root =
  if i = 0 then root
  else Arg_op ("bvadd.64", [root; args_offset_of_field struct_t i])

let mk_field_pointer struct_t i root value =
  (* alternative where we the address is the syntactic eltptr expression *)
  (* let addr = mkEltptr root (args_of_type struct_t) (mkJump (bvargs_of_int 64 i) mkJumpEnd) in *)
  (* mkPointer addr (args_type_field struct_t i) value *)
  mkPointer (mk_field_offset struct_t i root) (args_type_field struct_t i) value

let mk_padded_field_pointer struct_t i root value =
  let field_pointer = mk_field_pointer struct_t i root value in
  match mk_padding_of_field struct_t i root with
    | None -> field_pointer
    | Some pad -> mkStar field_pointer pad

let rec mk_unfolded_struct_descend_in_singletons struct_t root fields_values =
  let mk_field i subelt_t =
    let v = Array.get fields_values i in
    if is_single_struct subelt_t then
      let single_field_val = Array.of_list [v] in
      mk_unfolded_struct_descend_in_singletons subelt_t (mk_field_offset struct_t i root) single_field_val
    else
      mk_padded_field_pointer struct_t i root v in
  let pointers = Array.mapi mk_field (struct_element_types struct_t) in
  Array.fold_left (fun f p -> mkStar f p) mkEmpty pointers

let mk_unfolded_struct struct_t root fields_values =
  let mk_field i subelt_t =
    let v = Array.get fields_values i in
    mk_padded_field_pointer struct_t i root v in
  let pointers = Array.mapi mk_field (struct_element_types struct_t) in
  Array.fold_left (fun f p -> mkStar f p) mkEmpty pointers

(*** Scalar rules *)

(* assumes that [t] is an int or struct type *)
let sizeof_logic_of_type t =
  let args_t = args_of_type t in
  let args_size = args_sizeof t in
  let rewrite_rule =
    { function_name = "sizeof";
      arguments=[args_t];
      result=args_size;
      guard={without_form=[];rewrite_where=[];if_form=[]};
      rewrite_name="sizeof_"^(string_of_lltype t);
      saturate=false} in
  let logic = { empty_logic with rw_rules = [rewrite_rule]; } in
  (logic, logic)


(*** Structure rules *)

(* assumes that [t] is a struct type *)
let eltptr_logic_of_type t =
  let args_struct_t = args_of_type t in
  let root_var = Arg_var (Vars.AnyVar (0, "x")) in
  let jump_var = Arg_var (Vars.AnyVar (0, "j")) in
  let t_var = Arg_var (Vars.AnyVar (0, "t")) in
  let v_var = Arg_var (Vars.AnyVar (0, "v")) in
  let subelt_eltptr_rules i subelt_type =
    let jump = mkJump (bvargs_of_int 64 i) jump_var in
    let orig_eltptr = mkEltptr root_var args_struct_t jump in
    let new_root =
      if i = 0 then root_var
      else Arg_op ("bvadd.64", [root_var; args_offset_of_field t i]) in
    let result = mkEltptr new_root (args_of_type subelt_type) jump_var in
    mk_simple_equiv_rule ("eltptr_"^(string_of_struct t)^(string_of_int i))
      (mkPointer orig_eltptr t_var v_var)
      (mkPointer result t_var v_var) in
  let eltptr_rules = List.flatten (Array.to_list
    (Array.mapi subelt_eltptr_rules (struct_element_types t))) in
  let logic = { empty_logic with seq_rules = eltptr_rules; } in
  (logic, logic)

(* assumes that [t] is a struct type *)
let struct_value_logic_of_type t =
  Smtexpression.declare_struct_type t;
  (empty_logic, empty_logic)

(** assumes that [t] is a struct type *)
let struct_field_value_logic_of_type t =
  (* the name of the predicate indicating a value of type [t] *)
  let val_field_pred_name i = (string_of_struct t) ^ "_field" ^ (string_of_int i)  in
  let root = Arg_var (Vars.AnyVar (0, "x")) in
  let value_rule i =
    { function_name = val_field_pred_name i;
      arguments = [root];
      result = mk_field_val_of_struct_val t root i;
      guard={without_form=[];rewrite_where=[];if_form=[]};
      rewrite_name=val_field_pred_name i;
      saturate=false} in
  let logic = { empty_logic with
    rw_rules = Array.to_list (Array.mapi (fun i _ -> value_rule i) (struct_element_types t)); } in
  (logic, logic)


(** unfolds a struct pointer on the lhs when the pointer on the rhs is exactly one of the fields of [t] *)
(* this is not enough for nested structs or devious pointer casting, but it is
    efficient to try that first as it will be a common case *)
(* assumes that [t] is a struct type *)
let unfold_logic_of_type t =
  let x_var = Arg_var (Vars.AnyVar (0, "x")) in
  let v_var = Arg_var (Vars.AnyVar (0, "v")) in
  let w_var = Arg_var (Vars.AnyVar (0, "w")) in
  let mk_field_rule1 i subelt_type =
    (* take a struct with value { v_1, ..., v_n } and one of its fields on the rhs and unfold it *)
    (* apply this rule before the next as it avoids cluttering of folding of unfolding of ... of struct values *)
    let target_pointer = mk_field_pointer t i x_var w_var in
    let field_base_values =
      Array.mapi (fun i _ -> Arg_var (Vars.AnyVar (0, "v"^(string_of_int i)))) (struct_element_types t) in
    let struct_pointer = mkPointer x_var (args_of_type t) (mk_struct_val_of_fields_descend_in_singletons t field_base_values) in
    mk_simple_sequent_rule ((string_of_struct t)^"_field_"^(string_of_int i))
      (mk_unfolded_struct_descend_in_singletons t x_var field_base_values, target_pointer)
      (struct_pointer, target_pointer) in
  let mk_field_rule2 i subelt_type =
    (* take a struct with value v and one of its fields on the rhs and unfold it *)
    let target_pointer = mk_field_pointer t i x_var w_var in
    let struct_pointer = mkPointer x_var (args_of_type t) v_var in
    let field_struct_derived_values =
      Array.mapi (fun i _ -> mk_field_val_of_struct_val_descend_in_singletons t v_var i) (struct_element_types t) in
    mk_simple_sequent_rule ((string_of_struct t)^"_field_"^(string_of_int i))
      (mk_unfolded_struct_descend_in_singletons t x_var field_struct_derived_values, target_pointer)
      (struct_pointer, target_pointer) in
  let field_rules =
    let rules_array1 = Array.mapi mk_field_rule1 (struct_element_types t) in
    let rules_array2 = Array.mapi mk_field_rule2 (struct_element_types t) in
    (Array.to_list rules_array1)@(Array.to_list rules_array2) in
  let logic = { empty_logic with seq_rules = field_rules; } in
  (logic, logic)

(** unfolds a struct pointer on the lhs when the pointer on the rhs is inside of the fields of [t] *)
(* assumes that [t] is a struct type *)
let arith_unfold_logic_of_type t =
  let field_ranged_values base_val =
    Array.mapi (fun i _ -> mk_field_val_of_struct_val_descend_in_singletons t base_val i) (struct_element_types t) in
  let x_var = Arg_var (Vars.AnyVar (0, "x")) in
  let v_var = Arg_var (Vars.AnyVar (0, "v")) in
  let y_var = Arg_var (Vars.AnyVar (0, "y")) in
  let t_var = Arg_var (Vars.AnyVar (0, "t")) in
  let w_var = Arg_var (Vars.AnyVar (0, "w")) in
  (* the whole struct *)
  let struct_pointer = mkPointer x_var (args_of_type t) v_var in
  let mk_field_rule i subelt_type =
    let target_pointer = mkPointer y_var t_var w_var in
    let offset_end_of_field = bvargs_of_int64 64 (Int64.add (alloc_size64_of_field t i) (offset64_of_field t i)) in
    mk_sequent_rule ((string_of_struct t)^"_inside_field_"^(string_of_int i))
      [[(mkEmpty, mk_unfolded_struct_descend_in_singletons t x_var (field_ranged_values v_var), target_pointer, mkEmpty)]]
      (mkEmpty, struct_pointer, target_pointer, mkEmpty)
      (mkEmpty, mkEmpty)
      [PureGuard (mkPPred ("bvule", [Arg_op("bvadd.64", [x_var; args_offset_of_field t i]); y_var]));
       PureGuard (mkPPred ("bvule", [Arg_op("bvadd.64", [y_var; Arg_op("sizeof", [t_var])]);
				     Arg_op("bvadd.64", [x_var; offset_end_of_field])]))] in
  let field_rules =
    let rules_array = Array.mapi mk_field_rule (struct_element_types t) in
    Array.to_list rules_array in
  let logic = { empty_logic with seq_rules = field_rules; } in
  (logic, logic)

(* assumes that [t] is a struct type *)
let fold_logic_of_type t =
  let field_values =
    Array.mapi (fun i _ -> Arg_var (Vars.AnyVar (0, "v"^(string_of_int i))))
      (struct_element_types t) in
  let collate_field_values = mk_struct_val_of_fields_descend_in_singletons t field_values in
  let x_var = Arg_var (Vars.AnyVar (0, "x")) in
  let unfolded_struct = mk_unfolded_struct_descend_in_singletons t x_var field_values in
  let rules =
    if is_single_struct t then
      let v_var = Arg_var (Vars.AnyVar (0, "v")) in
      let struct_pointer = mkPointer x_var (args_of_type t) v_var in
      mk_simple_equiv_rule ("collate_"^(string_of_struct t))
      	(struct_pointer)
      	(unfolded_struct)
    else
      mk_simple_equiv_rule ("collate_"^(string_of_struct t))
	(unfolded_struct)
	(mkPointer x_var (args_of_type t) collate_field_values) in
  let logic = { empty_logic with seq_rules = rules; } in
  (logic, logic)

(* assumes that [t] is a struct type *)
let bytearray_to_struct_conversions t =
  let x_avar = Vars.AnyVar (0, "x") in
  let y_avar = Vars.AnyVar (0, "y") in
  let j_avar = Vars.AnyVar (0, "j") in
  let v_evar = Vars.EVar (0, "v") in
  let v_avar = Vars.AnyVar (0, "v") in
  let argsv z = Arg_var z in

  let array_t = Llvm.array_type (Llvm.i8_type !llcontext) (Int64.to_int (Llvm_target.abi_size !lltarget t)) in
  let array_type = args_of_type array_t in
  let struct_type = args_of_type t in
  let mk_struct_pointer root value = mkPointer root struct_type value in
  let mk_bytearray_pointer root value = mkPointer root array_type value in

  let array_field_rules =
    let array_val = argsv v_avar in
    let struct_val = mkValConversion array_type struct_type array_val in
    let structform = mk_struct_pointer (argsv x_avar) struct_val in
    let arrayform = mk_bytearray_pointer (argsv x_avar) array_val in
    let eltptr = mkEQ (argsv y_avar,
		       mkEltptr (argsv x_avar) (mkPointerType struct_type) (argsv j_avar)) in
    mk_simple_equiv_rule ("fold_bytearray_on_"^(string_of_struct t)^"_field_access") (mkStar eltptr arrayform) structform in
  let array_field_rules_free_value =
    let free_val = argsv v_evar in
    let structform = mk_struct_pointer (argsv x_avar) free_val in
    let arrayform = mk_bytearray_pointer (argsv x_avar) free_val in
    let eltptr = mkEQ (argsv y_avar,
		       mkEltptr (argsv x_avar) (mkPointerType struct_type) (argsv j_avar)) in
    mk_sequent_rule ("fold_bytearray_on_"^(string_of_struct t)^"_field_access_free_val")
      [[(mkEmpty, structform, mkEmpty, mkEmpty)]]
      (mkEmpty, mkStar eltptr arrayform, mkEmpty, mkEmpty)
      ([], [])
      [NotInContext (Var (VarSet.singleton v_evar))] in
  let rules = array_field_rules_free_value::array_field_rules in
  let logic = { empty_logic with seq_rules = rules; } in
  (logic, logic)

let mkNode node_name struct_name root fields = mkSPred (node_name, (Arg_string struct_name)::root::fields)

(* assumes that [t] is a struct type *)
let concretise_node_logic_of_type t struct_name node_name node_fields = 
  let x_var = Arg_var (Vars.AnyVar (0, "x")) in
  let n_vars = List.map (fun nf -> Arg_var (Vars.AnyVar (0, "n"^(string_of_int nf)))) node_fields in
  let y_var = Arg_var (Vars.AnyVar (0, "y")) in
  let t_var = Arg_var (Vars.AnyVar (0, "t")) in
  let v_var = Arg_var (Vars.AnyVar (0, "v")) in
  let struct_field_values =
    let field_values =
      Array.mapi (fun i _ ->
	if List.mem i node_fields then Arg_var (Vars.AnyVar (0, "n"^(string_of_int i)))
	else Arg_var (Vars.freshe ())) (struct_element_types t) in
    mk_struct_val_of_fields t field_values in
  let struct_pointer = mkPointer x_var (args_of_type t) struct_field_values in
  let node = mkNode node_name struct_name x_var n_vars in
  let target_pointer = mkPointer y_var t_var v_var in
  let side_condition = [PureGuard (mkPPred ("bvule", [x_var; y_var]));
			PureGuard (mkPPred ("bvule", [Arg_op("bvadd.64", [y_var; Arg_op("sizeof", [t_var])]);
						      Arg_op("bvadd.64", [x_var; args_sizeof t])]))] in
  let concretise_rule =
    mk_sequent_rule ("concretise_"^(string_of_struct t))
      [[(mkEmpty, struct_pointer, target_pointer, mkEmpty)]]
      (mkEmpty, node, target_pointer, mkEmpty)
      (mkEmpty, mkEmpty)
      side_condition in
  let logic = { empty_logic with seq_rules = [concretise_rule] } in
  (logic, logic)
  

(* assumes that [t] is a struct type *)
let rollup_node_logic_of_type t struct_name node_name node_fields =
  let x_var = Arg_var (Vars.AnyVar (0, "x")) in
  let n_vars = List.map (fun nf -> Arg_var (Vars.AnyVar (0, "n"^(string_of_int nf)))) node_fields in
  let struct_field_values =
    let field_values =
      Array.mapi (fun i _ ->
	if List.mem i node_fields then Arg_var (Vars.AnyVar (0, "n"^(string_of_int i)))
	else Arg_var (Vars.AnyVar (0, "v"^(string_of_int i)))) (struct_element_types t) in
    mk_struct_val_of_fields t field_values in
  let struct_pointer = mkPointer x_var (args_of_type t) struct_field_values in
  let node = mkNode node_name struct_name x_var n_vars in
  let rollup_rule_known_next =
    mk_simple_equiv_rule ("rollup_"^(string_of_struct t)) struct_pointer node in
  let v_var = Arg_var (Vars.AnyVar (0, "v")) in
  let struct_pointer = mkPointer x_var (args_of_type t) v_var in
  let n_vals = List.map (fun nf -> mk_field_val_of_struct_val t v_var nf) node_fields in
  let node = mkNode node_name struct_name x_var n_vals in
  let rollup_rule_compute_next =
    mk_simple_equiv_rule ("rollup_"^(string_of_struct t)^"_compute_next") struct_pointer node in
  let logic = { empty_logic with seq_rules = rollup_rule_known_next@rollup_rule_compute_next } in
  (logic, logic)


let remove_pointer_arith_same_root_same_size =
  let x_var = Arg_var (Vars.AnyVar (0, "x")) in
  let t_var = Arg_var (Vars.AnyVar (0, "t")) in
  let v_var = Arg_var (Vars.AnyVar (0, "v")) in
  let y_var = Arg_var (Vars.AnyVar (0, "y")) in
  let w_var = Arg_var (Vars.AnyVar (0, "w")) in
  let src_pointer = mkPointer x_var t_var v_var in
  let target_pointer = mkPointer y_var t_var w_var in
  let eq_x_y = mkEQ(x_var, y_var) in
  let rule =
    mk_sequent_rule "remove_pointer_arith_same_root_same_size"
      [[(src_pointer, eq_x_y, mkEQ(v_var, w_var), mkEmpty)]]
      (mkEmpty, src_pointer, target_pointer, mkEmpty)
      (mkEmpty, mkEmpty)
      [PureGuard eq_x_y] in
  let logic = { empty_logic with seq_rules = [rule] } in
  (logic, logic)

let add_logic_pair (l1,m1) (l2,m2) = (add_logic l1 l2, add_logic m1 m2)

let gen_node_logics node_logic logic_generator t = match struct_name t with
  | None ->
    (* nodes are necessarily named for now *)
    (empty_logic, empty_logic)
  | Some name ->
      try
	let (struct_name, node_name, fields) = List.find (fun (n, _, _) -> n = name) node_logic in
	let num_fields = List.map int_of_string fields in
	logic_generator t struct_name node_name num_fields
      with
      | Not_found -> (empty_logic, empty_logic)
      | Failure "int_of_string" -> failwith "Parse error: fields in node declarations must be integers, got something else"

let nullptr_logic =
  let ptr_bitsize =
    Llvm_target.size_in_bits !lltarget (pointer_type (i8_type !llcontext)) in
  let nullptr_rw =
    { function_name = "NULL";
      arguments=[];
      result=bvargs64_of_int ptr_bitsize 0;
      guard={without_form=[];rewrite_where=[];if_form=[]};
      rewrite_name="nullptr";
      saturate=false} in
  let logic = { empty_logic with rw_rules = [nullptr_rw] } in
  (logic, logic)

let sizeof_ptr_logic =
  let ptr_bitsize =
    Llvm_target.size_in_bits !lltarget (pointer_type (i8_type !llcontext)) in
  let sizeof_ptr_rw =
    { function_name = "sizeof";
      arguments=[mkPointerType (Arg_var (Vars.AnyVar (0, "t")))];
      result=bvargs64_of_int ptr_bitsize (Llvm_target.pointer_size !lltarget);
      guard={without_form=[];rewrite_where=[];if_form=[]};
      rewrite_name="sizeof_pointer_type";
      saturate=false} in
  let logic = { empty_logic with rw_rules = [sizeof_ptr_rw] } in
  (logic, logic)

type rule_apply =
| ApplyOnce of (Psyntax.logic * Psyntax.logic)
| ApplyAtType of (lltype -> Psyntax.logic * Psyntax.logic) * (lltype -> bool)

(** generates the logic and the abduction logic of module [m] *)
let add_logic_of_module node_logic base_logic m =
  let int_struct_filter t = match classify_type t with
    | Integer
    | Struct -> true
    | _ -> false in
  let struct_filter t = match classify_type t with
    | Struct -> true
    | _ -> false in
  (** pairs of rule generation functions and a filter that checks they
      are applied only to certain types *)
  let rule_generators =
    ApplyOnce base_logic
    ::ApplyOnce nullptr_logic
    ::ApplyOnce sizeof_ptr_logic
    ::ApplyAtType (sizeof_logic_of_type, int_struct_filter)
    ::ApplyAtType (eltptr_logic_of_type, struct_filter)
    ::ApplyAtType (unfold_logic_of_type, struct_filter)
    ::ApplyAtType (struct_value_logic_of_type, struct_filter)
    ::ApplyAtType (bytearray_to_struct_conversions, struct_filter)
    ::ApplyAtType (gen_node_logics node_logic concretise_node_logic_of_type, struct_filter)
    ::ApplyOnce remove_pointer_arith_same_root_same_size
    ::ApplyAtType (arith_unfold_logic_of_type, struct_filter)
    ::ApplyAtType (gen_node_logics node_logic rollup_node_logic_of_type, struct_filter)
    ::ApplyAtType (fold_logic_of_type, struct_filter)
    ::[] in 
  let all_types = collect_types_in_module m in
  let apply_generator log = function
    | ApplyOnce g -> add_logic_pair log g
    | ApplyAtType (g, f) ->
      let f_types = List.filter f all_types in
      List.fold_left add_logic_pair log (List.map g f_types) in
  List.fold_left apply_generator (empty_logic, empty_logic) rule_generators
