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
open Rulegen_helpers


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
  { empty_logic with rw_rules = [rewrite_rule]; }
  

let eltptr_logic_of_struct t = match struct_name t with
  | None ->
    (* TODO: handle unnamed structs *)
    empty_logic
  | Some name ->
    let args_struct_t = args_of_type t in
    (** the physical offset inside the struct, as a logical expression *)
    let offset_of_field i =
      let offset = Llvm_target.offset_of_element !lltarget t i in
      Arg_op("numeric_const", [Arg_string(Int64.to_string offset)]) in
    let x_var = Arg_var (Vars.AnyVar (0, "x")) in
    let root_var = Arg_var (Vars.AnyVar (0, "r")) in
    let jump_var = Arg_var (Vars.AnyVar (0, "j")) in
    let subelt_eltptr_rules i subelt_type =
      let jump = Arg_op ("jump", [args_num i; jump_var]) in
      let equiv_left =
	mkPPred ("eltptr", [x_var; args_struct_t;
			    root_var; jump]) in
      let new_root =
	if i = 0 then root_var
	else Arg_op ("builtin_plus", [root_var; offset_of_field i]) in
      let equiv_right =
	mkPPred ("eltptr", [x_var; args_of_type subelt_type;
			    new_root; jump_var]) in
      gen_seq_rules_of_equiv ("eltptr_"^(string_of_struct t))
	(equiv_left, equiv_right) in
    let eltptr_rules =
      List.flatten 
	(Array.to_list
	   (Array.mapi subelt_eltptr_rules (struct_element_types t))) in
    { empty_logic with seq_rules = eltptr_rules; }

let fold_unfold_logic_of_struct t =
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
  { empty_logic with seq_rules = rules; }

(** generates the logic and the abduction logic of module [m] *)
let logic_of_module m =
  let all_typs = collect_types_in_module m in
  let filter_int_and_struct t = match classify_type t with
    | Integer
    | Struct -> true
    | _ -> false in
  let typs = List.filter filter_int_and_struct all_typs in
  let log_typs = List.map sizeof_logic_of_type typs in
  let logic = List.fold_left add_logic empty_logic log_typs in
  let filter_struct t = match classify_type t with
    | Struct -> true
    | _ -> false in
  (* here we filter typs and not all_typs because filter_struct gives
     a subset of typs *)
  let typs = List.filter filter_struct typs in
  let log_typs = List.map eltptr_logic_of_struct typs in
  let logic = List.fold_left add_logic logic log_typs in
  let log_typs = List.map fold_unfold_logic_of_struct typs in
  let logic = List.fold_left add_logic logic log_typs in
  if !Lstar_config.auto_gen_list_logic then (
    if log log_phase then
      print_endline "Generating list logic for the module";
    let l = List.map Sllist_rulegen.list_logic_of_type typs in
    let (list_logs,abduct_list_logs) = List.split l in
    (List.fold_left add_logic logic list_logs,
     List.fold_left add_logic logic abduct_list_logs)
  ) else (logic, logic)
