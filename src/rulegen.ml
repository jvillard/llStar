(*** Rulegen: generate rules for the module's types and recursive data structures *)

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
  bvargs_of_int64 64 offset

let offset_of_field_end struct_t i =
  let offset = Llvm_target.offset_of_element !lltarget struct_t i in
  let field_type = Array.get (struct_element_types struct_t) i in
  let field_size = Llvm_target.store_size !lltarget field_type in
  let field_end = Int64.add offset field_size in
  bvargs_of_int64 64 field_end

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
    let offset = bvargs_of_int64 64 (Int64.add offset elt_size) in
    let pad_addr = Arg_op("builtin_bvadd", [root; offset]) in
    Some (mkSPred ("padding", [pad_addr; Arg_string(Int64.to_string pad_size)]))

let mk_field_pointer struct_t i root value =
  if i = 0 then
    mkPointer root (args_sizeof_field struct_t i) value
  else
    let offset = Arg_op ("builtin_bvadd", [root; offset_of_field struct_t i]) in
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


(*** Scalar rules *)

(** assumes that [t] is an int or struct type *)
let sizeof_logic_of_type t =
  let args_t = args_of_type t in
  let args_size = args_sizeof !lltarget t in
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
let eltptr_logic_of_type t = match struct_name t with
  | None ->
    (* TODO: handle unnamed structs *)
    (empty_logic, empty_logic)
  | Some name ->
    let args_struct_t = args_of_type t in
    (** the physical offset inside the struct, as a logical expression *)
    let offset_of_field i =
      let offset = Llvm_target.offset_of_element !lltarget t i in
      bvargs_of_int64 64 offset in
    let x_var = Arg_var (Vars.AnyVar (0, "x")) in
    let root_var = Arg_var (Vars.AnyVar (0, "r")) in
    let jump_var = Arg_var (Vars.AnyVar (0, "j")) in
    let subelt_eltptr_rules i subelt_type =
      let jump = Arg_op ("jump", [bvargs_of_int 64 i; jump_var]) in
      let equiv_left =
	mkPPred ("eltptr", [x_var; args_struct_t; root_var; jump]) in
      let new_root =
	if i = 0 then root_var
	else Arg_op ("builtin_bvadd", [root_var; offset_of_field i]) in
      let equiv_right =
	mkPPred ("eltptr", [x_var; args_of_type subelt_type;
			    new_root; jump_var]) in
      gen_seq_rules_of_equiv ("eltptr_"^(string_of_struct t))
	(equiv_left, equiv_right) in
    let eltptr_rules =
      List.flatten 
	(Array.to_list
	   (Array.mapi subelt_eltptr_rules (struct_element_types t))) in
    let logic = { empty_logic with seq_rules = eltptr_rules; } in
    (logic, logic)


(*** struct rules *)

(** assumes that [t] is a struct type *)
let fold_unfold_logic_of_type t =
  let field_values =
    Array.mapi (fun i _ -> Arg_var (Vars.AnyVar (0, "v"^(string_of_int i))))
      (struct_element_types t) in
  
  let collate_field_values =
    let rec aux = function
      | [] -> assert false (* TODO: handle empty structs (sigh) *)
      | v::[] -> v
      | v1::v2::tl -> Arg_op ("collate", [v1; aux (v2::tl)]) in
    aux (Array.to_list field_values) in

  let field_ranged_values base_val =
    let range i =
      let b = offset_of_field t i in
      let e = offset_of_field_end t i in
      Arg_op ("rg", [b; e; base_val]) in
    Array.mapi (fun i _ -> range i) (struct_element_types t) in

  let x_var = Arg_var (Vars.AnyVar (0, "x")) in
  let v_var = Arg_var (Vars.AnyVar (0, "v")) in
  let w_var = Arg_var (Vars.AnyVar (0, "w")) in

  let mk_struct_pointer root value =
    mkPointer root (args_sizeof !lltarget t) value in

  let mk_field_rule i subelt_type =
    let b = offset_of_field t i in
    let e = offset_of_field_end t i in
    let field_value = Arg_op ("rg", [b; e; v_var]) in
    let target_pointer = mk_field_pointer t i x_var w_var in
    mk_simple_seq_rule ((string_of_struct t)^"_field_"^(string_of_int i))
      (mk_unfolded_struct t x_var (field_ranged_values v_var),
       pconjunction (mkEQ (w_var,field_value)) target_pointer)
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
  let mk_node ptr value = mkSPred ("node", [name;ptr; value]) in

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
