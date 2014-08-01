(*** Rulegen: generate rules for the module's types and recursive data structures *)
(* BUG: This assumes that the pointer size is 64! TODO: handle any size! *)


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

let mk_simple_sequent lhs rhs =
  let frame_on e f =
    if Syntax.expr_equal e Syntax.mk_emp then f
    else Syntax.mk_star [e; f] in
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

let rec mk_struct_of_fields struct_t mk_field =
  mk_struct_llt struct_t (Array.to_list (Array.mapi mk_field (struct_element_types struct_t)))

let rec mk_field_val_of_struct_val struct_t base_val i =
  mk_struct_field_llt struct_t i base_val

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
  (* alternative where the address is the syntactic eltptr expression *)
  (* let addr = mkEltptr root (expr_of_lltype struct_t) (mkJump (bvexpr_of_int 64 i) mkJumpEnd) in *)
  (* mk_pointer addr value *)
  mk_pointer (mk_field_offset struct_t i root) value

let mk_padded_field_pointer struct_t i root value =
  let field_pointer = mk_field_pointer struct_t i root value in
  match mk_padding_of_field struct_t i root with
    | None -> field_pointer
    | Some pad -> Syntax.mk_star [field_pointer; pad]

let mk_unfolded_struct struct_t root mk_field =
  let mk_field_ptr i subelt_t =
    let v = mk_field i subelt_t in
    mk_padded_field_pointer struct_t i root v in
  let pointers = Array.mapi mk_field_ptr (struct_element_types struct_t) in
  Syntax.mk_star (Array.to_list pointers)

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


(* assumes that [t] is a struct type *)
let field_functions_logic t =
  let t_e = expr_of_lltype t in
  let field i ft =
    let i_e = mk_bv 64 (string_of_int i) in
    let ft_abs = mk_field_type t_e i_e in
    let ft_conc = expr_of_lltype ft in
    (* TODO: these could be pushed directly to Z3 *)
    let ft_r =
      { rw_name = Printf.sprintf "field_type_%s_%d" (string_of_lltype t) i
      ; rw_from_pattern = ft_abs ; rw_to_pattern = ft_conc } in
    let offset_abs = mk_offset t_e i_e in
    let offset_conc = expr_offset_of_field t i in
    let offset_r =
      { rw_name = Printf.sprintf "offset_%s_%d" (string_of_lltype t) i
      ; rw_from_pattern = offset_abs ; rw_to_pattern = offset_conc } in
    [Rewrite_rule ft_r; Rewrite_rule offset_r] in
  List.flatten (Array.to_list (Array.mapi field (struct_element_types t)))

(* assumes that [st] is a struct type *)
let exploded_struct_logic st =
  let x = Syntax.mk_fresh_tpat pointer_sort "x" in
  let v = Syntax.mk_fresh_tpat (sort_of_lltype st) "v" in
  let exploded_pat = mk_exploded_struct x v in
  let exploded_concrete =
    let mk_field i ft = mk_field_val_of_struct_val st v i in
    mk_unfolded_struct st x mk_field in
  let r = { rw_name = "exploded_struct"
          ; rw_from_pattern = exploded_pat
          ; rw_to_pattern = exploded_concrete } in
  [Rewrite_rule r]

(* assumes that [t] is a struct type *)
(** rewrites st_fldi {e1, ..., en } -> ei *)
let struct_expr_simplifier_logic t =
  let mk_field i ft = 
    let s = Printf.sprintf "v_%d" i in
    Syntax.mk_tpat (sort_of_lltype ft) s in
  let st_val = mk_struct_of_fields t mk_field in
  let field i ft =
    let fld_expr = mk_struct_field_llt t i st_val in
    let r = mk_field i ft in
    Rewrite_rule
      { rw_name = Printf.sprintf "field_simpl_%s_%d" (string_of_lltype t) i
      ; rw_from_pattern = fld_expr ; rw_to_pattern = r } in
  Array.to_list (Array.mapi field (struct_element_types t))

(*** Structure rules *)

let int_struct_filter t = match classify_type t with
  | Integer
  | Struct -> true
  | _ -> false
let value_filter t = match classify_type t with
  | Half | Float | Double | X86fp80 | Fp128 | Ppc_fp128 | X86_mmx
  | Integer
  | Struct | Array | Vector | Pointer
    -> true
  | Void | Label | Metadata | Function -> false
let struct_filter t = match classify_type t with
  | Struct -> true
  | _ -> false

(*** setters and getters for rule names *)

let seq_get_name r = r.seq_name
let seq_set_name r s = { r with seq_name = s }
let rw_get_name r = r.rw_name
let rw_set_name r s = { r with rw_name = s }

(*** the substitutions of interest *)

let subst_lltype_expr t_var t e =
  Z3.Expr.substitute_one e t_var (expr_of_lltype t)

let st_rule get_name set_name subst_in_rule st i r =
  let st_sort = Z3.Sort.mk_uninterpreted_s z3_ctx st in
  let st_var = Z3.Expr.mk_const_s z3_ctx st lltype_sort in
  let i_var = Z3.Expr.mk_const_s z3_ctx i idx_sort in
  (* for each field index [i] and its type [field_t] in struct type [st] *)
  let field t i field_t =
    let qual_name = Printf.sprintf "%s_%s_%d" (get_name r) (string_of_struct t) i in
    let r = set_name r qual_name in
    let subst_vars = change_sort_of_vars st_sort (sort_of_lltype t) in
    let subst_i e = Z3.Expr.substitute_one e i_var (mk_bv 64 (string_of_int i)) in
    let subst_llt = subst_lltype_expr st_var t in
    (* careful with the order in the following *)
    let subst = subst_i @@ subst_llt @@ subst_vars in
    subst_in_rule subst r in
  (* for each struct type [t] in the module *)
  let struc t =
    Array.to_list (Array.mapi (field t) (struct_element_types t)) in
  let structs = List.filter struct_filter (collect_types_in_module (get_llmodule ())) in
  structs >>= struc

let struct_rule st i = function
  | Sequent_rule r -> List.map (fun u -> Sequent_rule u)
                               (st_rule seq_get_name seq_set_name
                                        CalculusOps.subst_in_sequent_schema
                                        st i r)
  | Rewrite_rule r -> List.map (fun u -> Rewrite_rule u)
                               (st_rule rw_get_name rw_set_name
                                        CalculusOps.subst_in_rewrite_schema
                                        st i r)

let rec poly_rule get_name set_name subst_in_rule ss r =
  let poly_rule_one s r =
    let var = Z3.Expr.mk_const_s z3_ctx s lltype_sort in
    let sort = Z3.Sort.mk_uninterpreted_s z3_ctx s in
    let typ t =
      let qual_name = Printf.sprintf "%s_%s" (get_name r) (string_of_lltype t) in
      let r = set_name r qual_name in
      let subst_vars = change_sort_of_vars sort (sort_of_lltype t) in
      let subst_llt = subst_lltype_expr var t in
      let subst = subst_llt @@ subst_vars in
      subst_in_rule subst r in
    let ts = collect_types_in_module (get_llmodule ()) in
    List.map typ ts in
  let f u = poly_rule get_name set_name subst_in_rule u r in
  match ss with
  | [] -> [r]
  | s::tl -> 
     let rs = f tl in
     rs >>= poly_rule_one s

let polymorphic_rule ss = function
  | Sequent_rule r ->
     List.map (fun u -> Sequent_rule u)
              (poly_rule seq_get_name seq_set_name
                         CalculusOps.subst_in_sequent_schema ss r)
  | Rewrite_rule r ->
     List.map (fun u -> Rewrite_rule u)
              (poly_rule rw_get_name rw_set_name
                         CalculusOps.subst_in_rewrite_schema ss r)

type rule_apply =
| CalculusOnce of Calculus.t
| CalculusAtType of (lltype -> Calculus.t) * (lltype -> bool)

(** generates the logic and the abduction logic of module [m] *)
let add_rules_of_module base_logic m =
  (** pairs of rule generation functions and a filter that checks they
      are applied only to certain types *)
  let rule_generators =
    CalculusAtType (sizeof_logic_of_type, int_struct_filter)
    ::CalculusAtType (field_functions_logic, struct_filter)
    ::CalculusAtType (exploded_struct_logic, struct_filter)
    ::CalculusAtType (struct_expr_simplifier_logic, struct_filter)
    ::[] in 
  let all_types = collect_types_in_module m in
  let add_calculus log c = { log with Core.calculus = log.Core.calculus@c } in
  let apply_generator log = function
    | CalculusOnce g -> add_calculus log g
    | CalculusAtType (g, f) ->
      let f_types = List.filter f all_types in
      List.fold_left add_calculus log (List.map g f_types) in
  List.fold_left apply_generator base_logic rule_generators
