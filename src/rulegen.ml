(*** Rulegen: generate rules for the module's types and recursive data structures *)
(* This assumes that the pointer size is 64! TODO: handle any size! *)


open Format
(* LLVM modules *)
open Llvm
open TypeKind
(* coreStar modules *)
open Corestar_std
open Calculus
open Debug
(* llStar modules *)
open Llexpression
open Llutils

let z3_ctx = Syntax.z3_ctx

let frame_pat = Syntax.mk_bool_tpat "_frame"
let lhs_frame_pat = Syntax.mk_bool_tpat "_lhs"
let rhs_frame_pat = Syntax.mk_bool_tpat "_rhs"

let mk_simple_equiv_rule n p f lhs rhs =
  mk_equiv_rule n p f (Syntax.mk_star lhs lhs_frame_pat) (Syntax.mk_star rhs lhs_frame_pat)

let mk_simple_sequent lhs rhs =
  let frame_on e f =
    if Syntax.expr_equal e Syntax.mk_emp then f
    else Syntax.mk_star e f in
  { frame = frame_pat;
    hypothesis = frame_on lhs lhs_frame_pat;
    conclusion = frame_on rhs rhs_frame_pat }

(** Make a rule with a single premise, no side conditions

    | [pre_lhs]  |- [pre_rhs]
    --------------------------
    | [post_lhs] |- [post_rhs]
*)
let mk_simple_sequent_rule name p f (pre_lhs,pre_rhs) (post_lhs, post_rhs) =
  let r =
    { seq_name = name
    ; seq_pure_check = []
    ; seq_fresh_in_expr = []
    ; seq_goal_pattern = mk_simple_sequent post_lhs post_rhs
    ; seq_subgoal_pattern = [mk_simple_sequent pre_lhs pre_rhs]
    ; seq_priority = p
    ; seq_flags = f } in
  Sequent_rule r

(*** helper functions to define predicates for structs *)

let bits_of_bytes x =
  Int64.mul (Int64.of_int 8) x

let type_of_field struct_t i = Array.get (struct_element_types struct_t) i

(** the physical offset inside the struct, in bytes *)
let offset64_of_field struct_t i =
  Llvm_target.DataLayout.offset_of_element struct_t i !lltarget

let expr_offset_of_field struct_t i =
  mk_bv64 64 (offset64_of_field struct_t i)

let alloc_size64_of_field struct_t i =
  Llvm_target.DataLayout.abi_size (type_of_field struct_t i) !lltarget

let expr_alloc_sizeof_field struct_t i =
  mk_bv64 64 (alloc_size64_of_field struct_t i)

let bitsize64_of_field struct_t i =
  Llvm_target.DataLayout.size_in_bits (type_of_field struct_t i) !lltarget

let expr_bitsize_of_field struct_t i =
  mk_bv64 64 (bitsize64_of_field struct_t i)

(** what bit number starts element [i] of [struct_t] *)
let bitoffset64_of_field struct_t i =
  let bitsize_of_struct = Llvm_target.DataLayout.size_in_bits struct_t !lltarget in
  let bitoffset_of_field = bits_of_bytes (offset64_of_field struct_t i) in
  Int64.sub (Int64.sub bitsize_of_struct bitoffset_of_field) Int64.one

(** what bit number ends element [i] of [struct_t] *)
let bitoffset64_of_field_end struct_t i =
  Int64.add (Int64.sub (bitoffset64_of_field struct_t i) (bitsize64_of_field struct_t i)) Int64.one

let expr_type_field struct_t i =
  expr_of_lltype (type_of_field struct_t i)

let padsize64_of_field struct_t i =
  let offset = offset64_of_field struct_t i in
  let next_offset =
    if i = Array.length (struct_element_types struct_t) - 1 then
      Llvm_target.DataLayout.abi_size struct_t !lltarget
    else offset64_of_field struct_t (i+1) in
  let total_size = Int64.sub next_offset offset in
  let alloc_size = Llvm_target.DataLayout.abi_size (type_of_field struct_t i) !lltarget in
  Int64.sub total_size alloc_size

let expr_padsize_of_field struct_t i =
  mk_bv64 64 (padsize64_of_field struct_t i)

let is_single_struct t = match classify_type t with
  | Struct -> (Array.length (struct_element_types t) = 1) && (padsize64_of_field t 0 = Int64.zero)
  | _ -> false

let rec mk_struct_of_fields_descend_in_singletons struct_t mk_field =
  let mk_singletons i subelt_t =
    if is_single_struct subelt_t then
      mk_struct_of_fields_descend_in_singletons subelt_t (fun _ -> mk_field i)
    else mk_field i subelt_t in
  mk_struct_llt struct_t (Array.to_list (Array.mapi mk_singletons (struct_element_types struct_t)))

let rec mk_field_val_of_struct_val_descend_in_singletons struct_t base_val i =
  let subelt_t = Array.get (struct_element_types struct_t) i in
  let v = mk_struct_field_llt struct_t i base_val in
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
      else
        let expr_off = mk_bv64 64 pad_offset in
        Z3.BitVector.mk_add z3_ctx root expr_off in
    let expr_pad_size = expr_padsize_of_field struct_t i in
    Some (mk_padding pad_addr expr_pad_size)

let mk_field_offset struct_t i root =
  if i = 0 then root
  else Z3.BitVector.mk_add z3_ctx root (expr_offset_of_field struct_t i)

let mk_field_pointer struct_t i root value =
  (* alternative where we the address is the syntactic eltptr expression *)
  (* let addr = mkEltptr root (expr_of_lltype struct_t) (mkJump (bvexpr_of_int 64 i) mkJumpEnd) in *)
  (* mk_pointer addr (expr_type_field struct_t i) value *)
  mk_pointer (mk_field_offset struct_t i root) (expr_type_field struct_t i) value

let mk_padded_field_pointer struct_t i root value =
  let field_pointer = mk_field_pointer struct_t i root value in
  match mk_padding_of_field struct_t i root with
    | None -> field_pointer
    | Some pad -> Syntax.mk_star field_pointer pad

let rec mk_unfolded_struct_descend_in_singletons struct_t root mk_field =
  let mk_field_descend i subelt_t =
    if is_single_struct subelt_t then
      mk_unfolded_struct_descend_in_singletons subelt_t (mk_field_offset struct_t i root) (fun _ -> mk_field i)
    else
      mk_padded_field_pointer struct_t i root (mk_field i subelt_t) in
  let pointers = Array.mapi mk_field_descend (struct_element_types struct_t) in
  Syntax.mk_big_star (Array.to_list pointers)

let mk_unfolded_struct struct_t root mk_field =
  let mk_field_ptr i subelt_t =
    let v = mk_field i subelt_t in
    mk_padded_field_pointer struct_t i root v in
  let pointers = Array.mapi mk_field_ptr (struct_element_types struct_t) in
  Syntax.mk_big_star (Array.to_list pointers)

(*** Scalar rules *)

(* assumes that [t] is an int or struct type *)
let sizeof_logic_of_type t =
  let expr_sizeof = mk_sizeof (expr_of_lltype t) in
  let expr_size = expr_of_sizeof t in
  let r =
    { rw_name = "sizeof_"^(string_of_lltype t)
    ; rw_from_pattern = expr_sizeof
    ; rw_to_pattern = expr_size } in
  [Rewrite_rule r]

let read_as_type t =
  let s = sort_of_lltype t in
  let x = Syntax.mk_tpat s "x" in
  let y = Syntax.mk_tpat s "y" in
  let as_x = mk_as_llmem x in
  let as_y = mk_as_llmem y in
  let r =
    { rw_name = ("read_"^(string_of_lltype t))
    ; rw_from_pattern = Syntax.mk_eq as_x as_y
    ; rw_to_pattern = Syntax.mk_eq x y } in
  [Rewrite_rule r]

(*** Structure rules *)

let int_struct_filter t = match classify_type t with
  | Integer
  | Struct -> true
  | _ -> false
let value_filter t = match classify_type t with
  | Half
  | Float
  | Double
  | X86fp80
  | Fp128
  | Ppc_fp128
  | Integer
  | Struct
  | Array
  | Pointer
  | Vector
  | X86_mmx -> true
  | _ -> false
let struct_filter t = match classify_type t with
  | Struct -> true
  | _ -> false

let subst_struct st_var st e =
  let x = Syntax.mk_fresh_tpat (bv_sort 64) "x" in
  let v = Syntax.mk_fresh_tpat llmem_sort "v" in
  let exploded_pat = mk_exploded_struct x st_var v in
  let exploded_concrete =
    let sv = as_sort (struct_sort st) v in
    let mk_field i ft =
      mk_as_llmem (mk_field_val_of_struct_val_descend_in_singletons st sv i) in
    mk_unfolded_struct_descend_in_singletons st x mk_field in
  let r = { rw_name = "exploded_struct"
          ; rw_from_pattern = exploded_pat
          ; rw_to_pattern = exploded_concrete } in
  let e = Prover.rewrite_in_expr [] r e in
  Z3.Expr.substitute_one e st_var (expr_of_lltype st)

let subst_field st_var i_var st i field_t e =
  let fieldt_type = expr_of_lltype field_t in
  let fieldt_e = mk_field_type st_var i_var in
  let e = Z3.Expr.substitute_one e fieldt_e fieldt_type in
  let offset_bv = expr_offset_of_field st i in
  let offset_e = mk_offset st_var i_var in
  let e = Z3.Expr.substitute_one e offset_e offset_bv in
  let e = Z3.Expr.substitute_one e i_var (mk_bv 64 (string_of_int i)) in
  subst_struct st_var st e

let subst_in_sequent subst { frame; hypothesis; conclusion } =
  { frame = subst frame; hypothesis = subst hypothesis; conclusion = subst conclusion }

let gen_struct_seq_rule st_var i_var r =
  let field st i field_t =
    let subst = subst_field st_var i_var st i field_t in
    Sequent_rule { r with
      seq_name =
        Printf.sprintf "%s_%s_%d" r.seq_name (string_of_struct st) i
      ; seq_pure_check = List.map subst r.seq_pure_check
      ; seq_goal_pattern = subst_in_sequent subst r.seq_goal_pattern
      ; seq_subgoal_pattern = List.map (subst_in_sequent subst) r.seq_subgoal_pattern } in
  let struc st =
    Array.to_list (Array.mapi (field st) (struct_element_types st)) in
  let structs = List.filter struct_filter (collect_types_in_module (get_llmodule ())) in
  structs >>= struc

let gen_struct_rule st_var i_var = function
  | Sequent_rule r -> gen_struct_seq_rule st_var i_var r
  | Rewrite_rule r -> assert false

(* assumes that [t] is a struct type *)
let fold_logic_of_type t =
  let x_var = Syntax.mk_tpat pointer_sort "x" in
  let mk_field i ft =
    Syntax.mk_tpat (sort_of_lltype ft) ("v"^(string_of_int i)) in
  let collate_field_values = mk_struct_of_fields_descend_in_singletons t mk_field in
  let mk_llmem_field i ft =
    mk_as_llmem (mk_field i ft) in
  let struct_llmem = mk_as_llmem collate_field_values in
  let unfolded_struct = mk_unfolded_struct_descend_in_singletons t x_var mk_llmem_field in
  let struct_pointer = mk_pointer x_var (expr_of_lltype t) struct_llmem in
  mk_simple_equiv_rule ("collate_"^(string_of_struct t))
    default_rule_priority
    rule_no_backtrack
    unfolded_struct struct_pointer

(* assumes that [t] is a struct type *)
let bytearray_to_struct_conversions t =
  let x_var = Syntax.mk_tpat pointer_sort "x" in
  let v_var = Syntax.mk_tpat llmem_sort "v" in
  let y_var = Syntax.mk_tpat pointer_sort "y" in
  let j_var = Syntax.mk_tpat jump_sort "j" in
  let size = mk_bv64 64 (Llvm_target.DataLayout.abi_size t !lltarget) in
  let array_type = mk_array_type size mk_i8_type in
  let struct_type = expr_of_lltype t in
  let structform = mk_pointer x_var struct_type v_var in
  let arrayform = mk_pointer x_var array_type v_var in
  let eltptr = Syntax.mk_eq y_var
    (mk_eltptr x_var (mk_pointer_type struct_type) j_var) in
  mk_simple_equiv_rule ("fold_bytearray_on_"^(string_of_struct t)^"_field_access")
    default_rule_priority
    rule_no_backtrack
    (Syntax.mk_star eltptr arrayform) structform

let mk_node node_name struct_name root fields =
  let args_sort =
    Syntax.string_sort::pointer_sort::(List.map Z3.Expr.get_sort fields) in
  let node_fun =
    Z3.FuncDecl.mk_func_decl_s z3_ctx "node" args_sort Syntax.bool_sort in
  Z3.FuncDecl.apply node_fun (Syntax.mk_string_const struct_name::root::fields)

(** TODO: merge those and the corresponding functions for structure pointers *)
let unfold_logic_of_node t struct_name node_name node_fields =
  let x_var = Syntax.mk_tpat pointer_sort "x" in
  let w_var = Syntax.mk_tpat llmem_sort "w" in
  let mk_field_var i =
    let s = sort_of_lltype (type_of_field t i) in
    Syntax.mk_tpat s ("f"^(string_of_int i)) in
  let n_vars = List.map mk_field_var node_fields in
  let mk_field_rule i subelt_type =
    let target_pointer = mk_field_pointer t i x_var w_var in
    let mk_field i ft =
      let fs = sort_of_lltype ft in
      if List.mem i node_fields
      then mk_as_llmem (mk_field_var i)
      else mk_as_llmem (Syntax.mk_fresh_lvar fs ("f"^(string_of_int i))) in
    let node = mk_node node_name struct_name x_var n_vars in
    mk_simple_sequent_rule (node_name^"_field_"^(string_of_int i))
      default_rule_priority
      rule_no_backtrack
      (mk_unfolded_struct_descend_in_singletons t x_var mk_field, target_pointer)
      (node, target_pointer) in
  let rules_array = Array.mapi mk_field_rule (struct_element_types t) in
  Array.to_list rules_array

let arith_unfold_logic_of_node t struct_name node_name node_fields =
  let field_ranged_values base_val i ft =
    let fv = mk_field_val_of_struct_val_descend_in_singletons t base_val i in
    mk_as_llmem fv in
  let x_var = Syntax.mk_tpat pointer_sort "x" in
  let v_var = Syntax.mk_tpat llmem_sort "v" in
  let y_var = Syntax.mk_tpat pointer_sort "y" in
  let t_var = Syntax.mk_tpat lltype_sort "t" in
  let w_var = Syntax.mk_tpat llmem_sort "w" in
  (* the whole struct *)
  let struct_pointer = mk_pointer x_var (expr_of_lltype t) v_var in
  let mk_field_rule i subelt_type =
    let target_pointer = mk_pointer y_var t_var w_var in
    let offset_end_of_field =
      mk_bv64 64 (Int64.add (alloc_size64_of_field t i) (offset64_of_field t i)) in
    let x_plus_i = Z3.BitVector.mk_add z3_ctx x_var (expr_offset_of_field t i) in
    let y_plus_sz = Z3.BitVector.mk_add z3_ctx y_var (mk_sizeof t_var) in
    let x_plus_end = Z3.BitVector.mk_add z3_ctx x_var offset_end_of_field in
    { seq_name = node_name^"_inside_field_"^(string_of_int i)
    ; seq_pure_check =
        [Z3.BitVector.mk_ule z3_ctx x_plus_i y_var;
         Z3.BitVector.mk_ule z3_ctx y_plus_sz x_plus_end]
    ; seq_fresh_in_expr = []
    ; seq_goal_pattern = mk_simple_sequent struct_pointer target_pointer
    ; seq_subgoal_pattern =
        [mk_simple_sequent
            (mk_unfolded_struct_descend_in_singletons t x_var (field_ranged_values v_var))
            target_pointer]
    ; seq_priority = default_rule_priority
    ; seq_flags = rule_no_backtrack } in
  let rules_array = Array.mapi mk_field_rule (struct_element_types t) in
  List.map (fun r -> Sequent_rule r) (Array.to_list rules_array)

(* assumes that [t] is a struct type *)
let concretise_node_logic_of_type t struct_name node_name node_fields = 
  let x_var = Syntax.mk_tpat pointer_sort "x" in
  let y_var = Syntax.mk_tpat pointer_sort "y" in
  let t_var = Syntax.mk_tpat lltype_sort "t" in
  let v_var = Syntax.mk_tpat llmem_sort "v" in
  let mk_field_var i =
    let s = sort_of_lltype (type_of_field t i) in
    Syntax.mk_tpat s ("f"^(string_of_int i)) in
  let n_vars = List.map mk_field_var node_fields in
  let struct_field_values =
    let field_values =
      Array.mapi (fun i ft ->
        if List.mem i node_fields
        then mk_field_var i
        else Syntax.mk_fresh_lvar (sort_of_lltype ft) ("f"^(string_of_int i)))
        (struct_element_types t) in
    mk_as_llmem (mk_struct_llt t (Array.to_list field_values)) in
  let struct_pointer = mk_pointer x_var (expr_of_lltype t) struct_field_values in
  let node = mk_node node_name struct_name x_var n_vars in
  let target_pointer = mk_pointer y_var t_var v_var in
  let y_plus_sz = Z3.BitVector.mk_add z3_ctx y_var (mk_sizeof t_var) in
  let x_plus_szof = Z3.BitVector.mk_add z3_ctx x_var (expr_of_sizeof t) in
  [Sequent_rule { seq_name = "concretise_"^(string_of_struct t)
   ; seq_pure_check = 
      [Z3.BitVector.mk_ule z3_ctx x_var y_var;
       Z3.BitVector.mk_ule z3_ctx y_plus_sz x_plus_szof]
   ; seq_fresh_in_expr = []
   ; seq_goal_pattern = mk_simple_sequent node target_pointer
   ; seq_subgoal_pattern = [mk_simple_sequent struct_pointer target_pointer]
   ; seq_priority = default_rule_priority
   ; seq_flags = rule_no_backtrack }]

(* assumes that [t] is a struct type *)
let rollup_node_logic_of_type t struct_name node_name node_fields =
  let x_var = Syntax.mk_tpat pointer_sort "x" in
  let v_var = Syntax.mk_tpat llmem_sort "v" in
  let mk_field_var i =
    let s = sort_of_lltype (type_of_field t i) in
    Syntax.mk_tpat s ("f"^(string_of_int i)) in
  let n_vars = List.map mk_field_var node_fields in
  let struct_field_values =
    let field_values =
      Array.mapi (fun i ft ->
        if List.mem i node_fields
        then mk_field_var i
        else Syntax.mk_tpat (sort_of_lltype ft) ("v"^(string_of_int i)))
        (struct_element_types t) in
    mk_as_llmem (mk_struct_llt t (Array.to_list field_values)) in
  let struct_pointer = mk_pointer x_var (expr_of_lltype t) struct_field_values in
  let node = mk_node node_name struct_name x_var n_vars in
  let rollup_rule_known_next =
    mk_simple_equiv_rule ("rollup_"^(string_of_struct t))
      default_rule_priority
      rule_no_backtrack
      struct_pointer node in
  let struct_pointer = mk_pointer x_var (expr_of_lltype t) v_var in
  let n_vals = List.map (fun nf -> mk_struct_field_llt t nf v_var) node_fields in
  let node = mk_node node_name struct_name x_var n_vals in
  let rollup_rule_compute_next =
    mk_simple_equiv_rule ("rollup_"^(string_of_struct t)^"_compute_next")
      default_rule_priority
      rule_no_backtrack
      struct_pointer node in
  rollup_rule_known_next@rollup_rule_compute_next

(* assumes that [t] is a struct type *)
let malloc_logic_of_node t struct_name node_name node_fields =
  let x_var = Syntax.mk_tpat pointer_sort "x" in
  let s_var = Syntax.mk_tpat size_sort "s" in
  let mk_field_var i =
    let s = sort_of_lltype (type_of_field t i) in
    Syntax.mk_tpat s ("f"^(string_of_int i)) in
  let n_vars = List.map mk_field_var node_fields in
  let struct_field_values =
    let field_values =
      Array.mapi (fun i ft ->
        if List.mem i node_fields
        then mk_field_var i
        else Syntax.mk_tpat (sort_of_lltype ft) ("v"^(string_of_int i)))
        (struct_element_types t) in
    mk_as_llmem (mk_struct_llt t (Array.to_list field_values)) in
  let malloced_right = mk_malloced x_var s_var in
  let malloced_node = mk_malloced x_var (expr_of_sizeof t) in
  let struct_pointer = mk_pointer x_var (expr_of_lltype t) struct_field_values in
  let node = mk_node node_name struct_name x_var n_vars in
  let rule =
    mk_simple_sequent_rule ("malloced_"^node_name)
      default_rule_priority
      rule_no_backtrack
      (Syntax.mk_star malloced_node struct_pointer, malloced_right)
      (node, malloced_right) in
  [rule]

let remove_pointer_arith_same_root_same_size =
  let x_var = Syntax.mk_tpat pointer_sort "x" in
  let t_var = Syntax.mk_tpat lltype_sort "t" in
  let v_var = Syntax.mk_tpat llmem_sort "v" in
  let y_var = Syntax.mk_tpat pointer_sort "y" in
  let w_var = Syntax.mk_tpat llmem_sort "w" in
  let src_pointer = mk_pointer x_var t_var v_var in
  let target_pointer = mk_pointer y_var t_var w_var in
  [Sequent_rule
      { seq_name = "remove_pointer_arith_same_root_same_size"
      ; seq_pure_check = [Syntax.mk_eq x_var y_var]
      ; seq_fresh_in_expr = []
      ; seq_goal_pattern = mk_simple_sequent src_pointer target_pointer
      ; seq_subgoal_pattern =
	  [{ frame = Syntax.mk_star src_pointer frame_pat
	   ; hypothesis = Syntax.mk_star (Syntax.mk_eq x_var y_var) lhs_frame_pat
	   ; conclusion = Syntax.mk_star (Syntax.mk_eq v_var w_var) rhs_frame_pat }]
      ; seq_priority = default_rule_priority
      ; seq_flags = rule_no_backtrack }]

let gen_node_logics node_decls logic_generator t = match struct_name t with
  | None ->
    (* nodes are necessarily named for now *)
    []
  | Some name ->
      try
        let nd = List.find (fun nd -> nd.ParserAst.node_name = name) node_decls in
        let num_fields = nd.ParserAst.node_fields in
        logic_generator t name nd.ParserAst.node_name num_fields
      with Not_found -> []

type rule_apply =
| CalculusOnce of Calculus.t
| CalculusAtType of (lltype -> Calculus.t) * (lltype -> bool)

(** generates the logic and the abduction logic of module [m] *)
let add_rules_of_module node_decls base_logic m =
  (** pairs of rule generation functions and a filter that checks they
      are applied only to certain types *)
  let rule_generators =
    (* CalculusOnce nullptr_logic *)
    (* CalculusOnce sizeof_ptr_logic *)
    CalculusAtType (sizeof_logic_of_type, int_struct_filter)
    ::CalculusAtType (read_as_type, value_filter)
    (* ::CalculusAtType (eltptr_logic_of_type, struct_filter) *)
    ::CalculusAtType (gen_node_logics node_decls malloc_logic_of_node, struct_filter)
    (* ::CalculusAtType (unfold_logic_of_type, struct_filter) *)
    ::CalculusAtType (bytearray_to_struct_conversions, struct_filter)
    ::CalculusAtType (gen_node_logics node_decls unfold_logic_of_node, struct_filter)
    ::CalculusOnce remove_pointer_arith_same_root_same_size
    (* ::CalculusAtType (arith_unfold_logic_of_type, struct_filter) *)
    ::CalculusAtType (gen_node_logics node_decls concretise_node_logic_of_type, struct_filter)
    ::CalculusAtType (gen_node_logics node_decls rollup_node_logic_of_type, struct_filter)
    ::CalculusAtType (fold_logic_of_type, struct_filter)
    ::[] in 
  let all_types = collect_types_in_module m in
  let add_calculus log c = { log with Core.calculus = log.Core.calculus@c } in
  let apply_generator log = function
    | CalculusOnce g -> add_calculus log g
    | CalculusAtType (g, f) ->
      let f_types = List.filter f all_types in
      List.fold_left add_calculus log (List.map g f_types) in
  List.fold_left apply_generator base_logic rule_generators
