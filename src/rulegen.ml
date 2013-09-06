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


(*** helper functions to define predicates for structs *)

(** Make a rule with a single premise, no side conditions

    | [pre_lhs]  |- [pre_rhs]
    --------------------------
    | [post_lhs] |- [post_rhs]
*)
let mk_simple_seq_rule name (pre_lhs,pre_rhs) (post_lhs, post_rhs) =
  let conclusion = (mkEmpty, post_lhs, post_rhs, mkEmpty) in
  let premise = (mkEmpty, pre_lhs, pre_rhs, mkEmpty) in
  (conclusion, [[premise]], name, ([], []), [])

let gen_seq_rules_of_equiv name (equiv_left, equiv_right) =
  mk_simple_seq_rule (name^"_left") (equiv_right, mkEmpty) (equiv_left, mkEmpty)::
  mk_simple_seq_rule (name^"_right") (mkEmpty, equiv_right) (mkEmpty, equiv_left)::[]

let bits_of_bytes x =
  Int64.mul (Int64.of_int 8) x

let type_of_field struct_t i = Array.get (struct_element_types struct_t) i

(** the physical offset inside the struct, in bytes *)
let offset64_of_field struct_t i =
  Llvm_target.offset_of_element !lltarget struct_t i

let args_offset_of_field struct_t i =
  bvargs_of_int64 64 (offset64_of_field struct_t i)

let store_size64_of_field struct_t i =
  Llvm_target.store_size !lltarget (type_of_field struct_t i)

let bitsize64_of_field struct_t i =
  Llvm_target.size_in_bits !lltarget (type_of_field struct_t i)

let args_bitsize_of_field struct_t i =
  bvargs_of_int64 64 (bitsize64_of_field struct_t i)

(** what bit number starts element [i] of [struct_t] *)
let bitoffset64_of_field struct_t i =
  bits_of_bytes (offset64_of_field struct_t i)

let args_bitoffset_of_field struct_t i =
  bvargs_of_int64 64 (bitoffset64_of_field struct_t i)

(** what bit number ends element [i] of [struct_t] *)
let bitoffset64_of_field_end struct_t i =
  Int64.sub (Int64.add (bitoffset64_of_field struct_t i) (bitsize64_of_field struct_t i)) Int64.one

let args_bitoffset_of_field_end struct_t i =
  bvargs_of_int64 64 (bitoffset64_of_field_end struct_t i)

let args_type_field struct_t i =
  args_of_type (type_of_field struct_t i)

let bitpadsize64_of_field struct_t i =
  let offset = offset64_of_field struct_t i in
  let next_offset =
    if i = Array.length (struct_element_types struct_t) - 1 then
      Llvm_target.store_size !lltarget struct_t
    else offset64_of_field struct_t (i+1) in
  let storage_size = bits_of_bytes (Int64.sub next_offset offset) in
  let elt_size = bitsize64_of_field struct_t i in
  Int64.sub storage_size elt_size

let args_bitpadsize_of_field struct_t i =
  bvargs_of_int64 64 (bitpadsize64_of_field struct_t i)

let mk_padding_of_field struct_t i root =
  let pad64_size = bitpadsize64_of_field struct_t i in
  if pad64_size = Int64.zero then None
  else
    let elt_offset = args_offset_of_field struct_t i in
    let elt_addr = Arg_op("bvadd.64", [root; elt_offset]) in
    let elt_size = args_bitsize_of_field struct_t i in
    let args_pad_size = args_bitpadsize_of_field struct_t i in
    Some (mkPadding elt_addr elt_size args_pad_size)

let mk_field_pointer struct_t i root value =
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
  let mk_field i subelt_t =
    let v = Array.get fields_values i in
    let fldsz = bitsize64_of_field struct_t i in
    let padsz = bitpadsize64_of_field struct_t i in
    if padsz = Int64.zero then (v, fldsz, None)
    else (v, fldsz, Some(bvargs64_of_int padsz 0, padsz)) in
  let fvalpad = Array.mapi mk_field (struct_element_types struct_t) in
  let rec concat_bv_list = function
    | [] -> (bvargs_of_int 0 0, Int64.zero)
    | [(bv, sz, None)] -> (bv, sz)
    | (bv, sz, None)::tl -> (* non-empty tail *)
      let (bvtl, sztl) = concat_bv_list tl in
      (Arg_op (Printf.sprintf "concat.%Ld.%Ld" sz sztl, [bv; bvtl]),
       Int64.add sz sztl)
    | (bv, sz, Some(bv_pad, sz_pad))::tl -> (* padding *)
      concat_bv_list ((bv, sz, None)::(bv_pad, sz_pad, None)::tl) in
  fst (concat_bv_list (Array.to_list fvalpad))


(*** Scalar rules *)

(** assumes that [t] is an int or struct type *)
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

(** assumes that [t] is a struct type *)
let eltptr_logic_of_type t =
  let args_struct_t = args_of_type t in
  let root_var = Arg_var (Vars.AnyVar (0, "x")) in
  let jump_var = Arg_var (Vars.AnyVar (0, "j")) in
  let subelt_eltptr_rules i subelt_type =
    let jump = Arg_op ("jump", [bvargs_of_int 64 i; jump_var]) in
    let arguments_left = [root_var; args_struct_t; jump] in
    let new_root =
      if i = 0 then root_var
      else Arg_op ("bvadd.64", [root_var; args_offset_of_field t i]) in
    let result = mkEltptr new_root (args_of_type subelt_type) jump_var in
    { function_name = "eltptr";
      arguments=arguments_left;
      result=result;
      guard={without_form=[];rewrite_where=[];if_form=[]};
      rewrite_name="eltptr_"^(string_of_struct t)^(string_of_int i);
      saturate=false} in
  let eltptr_rules = Array.to_list
    (Array.mapi subelt_eltptr_rules (struct_element_types t)) in
  let logic = { empty_logic with rw_rules = eltptr_rules; } in
  (logic, logic)

(** assumes that [t] is a struct type *)
let value_logic_of_type t =
  (* the name of the predicate indicating a value of type [t] *)
  let val_pred_name = (string_of_struct t) ^ "_val" in
  let field_values =
    Array.mapi (fun i _ -> Arg_var (Vars.AnyVar (0, "v"^(string_of_int i))))
      (struct_element_types t) in
  let val_expanded = mk_struct_val_of_fields t field_values in
  let value_rule =
    { function_name = val_pred_name;
      arguments = Array.to_list field_values;
      result=val_expanded;
      guard={without_form=[];rewrite_where=[];if_form=[]};
      rewrite_name=val_pred_name;
      saturate=false} in
  let logic = { empty_logic with rw_rules = [value_rule]; } in
  (logic, logic)


(*** struct rules *)

(** assumes that [t] is a struct type *)
let fold_unfold_logic_of_type t =
  let field_values =
    Array.mapi (fun i _ -> Arg_var (Vars.AnyVar (0, "v"^(string_of_int i))))
      (struct_element_types t) in
  
  let collate_field_values =
    mk_struct_val_of_fields t field_values in

  let bits_of_field base_val i =
    let field_begin = args_bitoffset_of_field t i in
    let field_end = args_bitoffset_of_field_end t i in
    Arg_op (Printf.sprintf "extract.%Ld" (Llvm_target.size_in_bits !lltarget t),
	    [field_end; field_begin; base_val]) in

  let field_ranged_values base_val =
    Array.mapi (fun i _ -> bits_of_field base_val i) (struct_element_types t) in

  let x_var = Arg_var (Vars.AnyVar (0, "x")) in
  let v_var = Arg_var (Vars.AnyVar (0, "v")) in
  let w_var = Arg_var (Vars.AnyVar (0, "w")) in

  let mk_struct_pointer root value =
    mkPointer root (args_of_type t) value in

  let mk_field_rule i subelt_type =
    let target_pointer = mk_field_pointer t i x_var w_var in
    mk_simple_seq_rule ((string_of_struct t)^"_field_"^(string_of_int i))
      (mk_unfolded_struct t x_var (field_ranged_values v_var), target_pointer)
      (mk_struct_pointer x_var v_var, target_pointer) in

  let field_rules =
    let rules_array = Array.mapi mk_field_rule (struct_element_types t) in
    Array.to_list rules_array in

  let rules =
    if Array.length (struct_element_types t) > 1 then
      mk_simple_seq_rule ("collate_"^(string_of_struct t))
	(mk_struct_pointer x_var collate_field_values, mk_struct_pointer x_var v_var)
	(mk_unfolded_struct t x_var field_values, mk_struct_pointer x_var v_var)::
	field_rules
    else [] in
  let logic = { empty_logic with seq_rules = rules; } in
  (logic, logic)


(** assumes that [t] is a struct type *)
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
      (mk_simple_seq_rule "node_lookup_next"
	 (mk_unfolded_struct_ i_var rec_field n_var, mk_rec_field i_var n2_var)
	 (mk_node i_var n_var, mk_rec_field i_var n2_var))
    else
      (mk_simple_seq_rule ("node_lookup_field_"^(string_of_int i))
	 (mk_unfolded_struct_ i_var rec_field n_var, mk_field_pointer t i i_var n2_var)
	 (mk_node i_var n_var, mk_field_pointer t i i_var n2_var)) in

  let expand_node_rules =
    let rules_array = Array.mapi mk_expand_node_rule (struct_element_types t) in
    Array.to_list rules_array in

  (** the list of rules that need to know about the structure of the nodes
      * the ones that don't are included in the distribution instead of generated
      * see logic/lseg.logic *)
  let symex_rules =
      expand_node_rules@
    (* Collapse to node.

       If all fields of a node object are present on the lhs, we collapse them to
       the node predicate. *)
      (mk_simple_seq_rule "node_rollup_left"
	 (mk_node i_var n_var, mkEmpty)
	 (mk_unfolded_struct_ i_var rec_field n_var, mkEmpty))::
    (* Convert all nodes on LHS to singleton list segments. *)
      (* (mk_simple_seq_rule "lseg_node_rollup_left" *)
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

(** assumes that [t] is a struct type *)
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
    (sizeof_logic_of_type,int_struct_filter)
    ::(eltptr_logic_of_type,struct_filter)
    ::(fold_unfold_logic_of_type,struct_filter)
    ::(value_logic_of_type,struct_filter)
    ::if !Lstar_config.auto_gen_list_logic then
	(sllnode_logic_of_type,struct_filter)::[]
      else [] in
  let add_logic_pair (l1,m1) (l2,m2) = (add_logic l1 l2, add_logic m1 m2) in
  let nullptr_rw =
    { function_name = "NULL";
      arguments=[];
      result=bvargs64_of_int (Llvm_target.size_in_bits !lltarget
				(pointer_type (i8_type !llcontext))) 0;
      guard={without_form=[];rewrite_where=[];if_form=[]};
      rewrite_name="nullptr";
      saturate=true} in
  (* always include the above rewrite of NULL() *)
  let starting_logic =
    ({empty_logic with rw_rules = [nullptr_rw]; },
     {empty_logic with rw_rules = [nullptr_rw]; }) in
  (** applies all rulegen functions that are compatible with [t] *)
  let logic_of_type t =
    List.fold_left
      (fun log (rulegen_fun, filter) ->
	if filter t then add_logic_pair log (rulegen_fun t)
	else log) (empty_logic, empty_logic) rule_generators in
  let all_types = collect_types_in_module m in
  let logics = List.map logic_of_type all_types in
  List.fold_left add_logic_pair starting_logic logics
