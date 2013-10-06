(*** Rulegen: generate rules for the module's types and recursive data structures *)
(* This assumes that the pointer size is 64! TODO: handle any size! *)


open Format
(* LLVM modules *)
open Llvm
open TypeKind
(* coreStar modules *)
open Config
open Psyntax
(* LStar modules *)
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

let mk_field_pointer struct_t i root value =
  (* alternative where we the address is the syntactic eltptr expression *)
  (* let addr = mkEltptr root (args_of_type struct_t) (mkJump (bvargs_of_int 64 i) mkJumpEnd) in *)
  (* mkPointer addr (args_type_field struct_t i) value *)
  if i = 0 then
    mkPointer root (args_type_field struct_t i) value
  else
    let offset = Arg_op ("bvadd.64", [root; args_offset_of_field struct_t i]) in
    mkPointer offset (args_type_field struct_t i) value

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

let mk_struct_val_of_fields struct_t fields_values =
  let struct_constructor = "mk_"^(string_of_struct struct_t) in
  Arg_op (struct_constructor, Array.to_list fields_values)

(** extracts the value of field [i] from value [base_val] of type [struct_t]  *)
let mk_field_val_of_struct_val struct_t base_val i =
  let field_pred = (string_of_struct struct_t) ^ "_fld" ^ (string_of_int i)  in
  Arg_op (field_pred, [base_val])

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
  let field_ranged_values base_val =
    Array.mapi (fun i _ -> mk_field_val_of_struct_val t base_val i) (struct_element_types t) in
  let x_var = Arg_var (Vars.AnyVar (0, "x")) in
  let v_var = Arg_var (Vars.AnyVar (0, "v")) in
  let w_var = Arg_var (Vars.AnyVar (0, "w")) in
  (* the whole struct *)
  let struct_pointer = mkPointer x_var (args_of_type t) v_var in
  let mk_field_rule i subelt_type =
    let target_pointer = mk_field_pointer t i x_var w_var in
    mk_simple_sequent_rule ((string_of_struct t)^"_field_"^(string_of_int i))
      (mk_unfolded_struct t x_var (field_ranged_values v_var), target_pointer)
      (struct_pointer, target_pointer) in
  let field_rules =
    let rules_array = Array.mapi mk_field_rule (struct_element_types t) in
    Array.to_list rules_array in
  let logic = { empty_logic with seq_rules = field_rules; } in
  (logic, logic)

(** unfolds a struct pointer on the lhs when the pointer on the rhs is inside of the fields of [t] *)
(* assumes that [t] is a struct type *)
let arith_unfold_logic_of_type t =
  let field_ranged_values base_val =
    Array.mapi (fun i _ -> mk_field_val_of_struct_val t base_val i) (struct_element_types t) in
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
      [[(mkEmpty, mk_unfolded_struct t x_var (field_ranged_values v_var), target_pointer, mkEmpty)]]
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
  let collate_field_values = mk_struct_val_of_fields t field_values in
  let x_var = Arg_var (Vars.AnyVar (0, "x")) in
  let rules =
    mk_simple_equiv_rule ("collate_"^(string_of_struct t))
      (mk_unfolded_struct t x_var field_values)
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
  let w_avar = Vars.AnyVar (0, "w") in
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

(* assumes that [t] is a struct type *)
let node_logic_of_struct t name rec_field =
  let fresh_field_values =
    Array.map (fun _ -> Arg_var (Vars.freshe ())) (struct_element_types t) in

  let mk_rec_field root value = mk_field_pointer t rec_field root value in
  let mk_node ptr value = mkSPred ("node", [name; ptr; value]) in

  let i_var = Arg_var (Vars.AnyVar (0, "i")) in
  let n_var = Arg_var (Vars.AnyVar (0, "n")) in
  let n2_var = Arg_var (Vars.AnyVar (0, "n2")) in

  let mk_unfolded_struct_ root i value =
    let old_field_val = Array.get fresh_field_values i in
    Array.set fresh_field_values i value;
    let us = mk_unfolded_struct t root fresh_field_values in
    Array.set fresh_field_values i old_field_val;
    us in

  (** Expand node.

      If we have a node predicate on the LHS of an implication and one of the
      fields of the node object appears on the RHS, then we rewrite the node in terms
      of its constituent fields. TODO: and match the field! *)
  let mk_expand_node_rule i subelt_type =
    if i = rec_field then
      (mk_simple_sequent_rule "node_lookup_next"
	 (mk_unfolded_struct_ i_var rec_field n_var, mk_rec_field i_var n2_var)
	 (mk_node i_var n_var, mk_rec_field i_var n2_var))
    else
      (mk_simple_sequent_rule ("node_lookup_field_"^(string_of_int i))
	 (mk_unfolded_struct_ i_var rec_field n_var, mk_field_pointer t i i_var n2_var)
	 (mk_node i_var n_var, mk_field_pointer t i i_var n2_var)) in

  let expand_node_rules =
    let rules_array = Array.mapi mk_expand_node_rule (struct_element_types t) in
    Array.to_list rules_array in

  (** the list of rules that don't need to know about the structure of the nodes
      * the ones that are included in the distribution instead of generated
      * see rules/lseg.logic *)
  let symex_rules =
      expand_node_rules@
    (* Collapse to node.

       If all fields of a node object are present on the lhs, we collapse them to
       the node predicate. *)
      (mk_simple_sequent_rule "node_rollup_left"
	 (mk_node i_var n_var, mkEmpty)
	 (mk_unfolded_struct_ i_var rec_field n_var, mkEmpty))::
    (* Convert all nodes on LHS to singleton list segments. *)
      (* (mk_simple_sequent_rule "lseg_node_rollup_left" *)
      (* 	 (mk_lseg_ne i_var n_var, mkEmpty) *)
      (* 	 (mk_node i_var n_var, mkEmpty)):: *)
	[] in
  let symex_logic = {empty_logic with seq_rules = symex_rules; } in

  (* rules for abduction of list nodes *)

  (** abduce an entire node whenever we need to abduce any of the fields *)
  let mk_abduce_node_rule i subelt_type =
    if i = rec_field then
      let conclusion = (mkEmpty, mkEmpty, mk_rec_field i_var n_var, mkEmpty) in
      let premise = (mkEmpty,
		     mk_unfolded_struct_ i_var rec_field n_var,
		     mk_rec_field i_var n_var,
		     mk_node i_var n_var) in
      let without = mkPointer i_var (Arg_var (Vars.AnyVar (0, "s"))) (Arg_var (Vars.AnyVar (0, "v"))) in
      (conclusion, [[premise]], "abduce_node_next", (without, []), [])
    else
      let conclusion = (mkEmpty, mkEmpty, mk_field_pointer t i i_var n_var, mkEmpty) in
      let premise = (mkEmpty,
		     mk_unfolded_struct_ i_var i n_var,
		     mk_field_pointer t i i_var n_var,
		     mk_node i_var n2_var) in
      let without = mkPointer i_var (Arg_var (Vars.AnyVar (0, "s"))) (Arg_var (Vars.AnyVar (0, "v"))) in
      (conclusion, [[premise]], "abduce_node_field_"^(string_of_int i), (without, []), []) in

  let abduce_node_rules =
    let rules_array = Array.mapi mk_abduce_node_rule (struct_element_types t) in
    Array.to_list rules_array in

  let abduct_logic = { empty_logic with seq_rules = symex_rules@abduce_node_rules; } in
  (symex_logic, abduct_logic)

(* assumes that [t] is a struct type *)
let sllnode_logic_of_type t = match struct_name t with
  | None ->
    (* recursive structs are necessarily named *)
    (empty_logic, empty_logic)
  | Some name ->
    (* let's find the first recursive field, which we assume is the
       forward pointer of the linked list *)
    let numed_types = Array.mapi (fun i tt -> (i,tt)) (struct_element_types t) in
    let subelt_types = Array.to_list numed_types in
    try
      let points_to_struct typ = pointer_type t = typ in
      let (rec_field,_) =
	List.find (fun (i,subelt_t) -> points_to_struct subelt_t) subelt_types in
      node_logic_of_struct t (Arg_string name) rec_field
    with Not_found -> (empty_logic, empty_logic)


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

type rule_apply =
| ApplyOnce of (Psyntax.logic * Psyntax.logic)
| ApplyAtType of (lltype -> Psyntax.logic * Psyntax.logic) * (lltype -> bool)

(** generates the logic and the abduction logic of module [m] *)
let logic_of_module m =
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
    ApplyAtType (sizeof_logic_of_type, int_struct_filter)
    ::ApplyAtType (eltptr_logic_of_type, struct_filter)
    ::ApplyAtType (unfold_logic_of_type, struct_filter)
    ::ApplyAtType (struct_value_logic_of_type, struct_filter)
    ::ApplyAtType (bytearray_to_struct_conversions, struct_filter)
    ::(if !Lstar_config.auto_gen_list_logic then
	[ApplyAtType (sllnode_logic_of_type, struct_filter)]
      else [])
    @ApplyOnce remove_pointer_arith_same_root_same_size
    ::ApplyAtType (arith_unfold_logic_of_type, struct_filter)
    ::ApplyAtType (fold_logic_of_type, struct_filter)
    ::[] in 
  let add_logic_pair (l1,m1) (l2,m2) = (add_logic l1 l2, add_logic m1 m2) in
  let ptr_bitsize =
    Llvm_target.size_in_bits !lltarget (pointer_type (i8_type !llcontext)) in
  let nullptr_rw =
    { function_name = "NULL";
      arguments=[];
      result=bvargs64_of_int ptr_bitsize 0;
      guard={without_form=[];rewrite_where=[];if_form=[]};
      rewrite_name="nullptr";
      saturate=false} in
  let sizeof_ptr_rw =
    { function_name = "sizeof";
      arguments=[mkPointerType (Arg_var (Vars.AnyVar (0, "t")))];
      result=bvargs64_of_int ptr_bitsize (Llvm_target.pointer_size !lltarget);
      guard={without_form=[];rewrite_where=[];if_form=[]};
      rewrite_name="sizeof_pointer_type";
      saturate=false} in
  (* always include the above rewrite of NULL() and sizeof pointer_type *)
  let init_rw = [nullptr_rw; sizeof_ptr_rw] in
  let starting_logic =
    ({empty_logic with rw_rules = init_rw; },
     {empty_logic with rw_rules = init_rw; }) in
  let all_types = collect_types_in_module m in
  let apply_generator log = function
    | ApplyOnce g -> add_logic_pair log g
    | ApplyAtType (g, f) ->
      let f_types = List.filter f all_types in
      List.fold_left add_logic_pair log (List.map g f_types) in
  List.fold_left apply_generator starting_logic rule_generators

