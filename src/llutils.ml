(*** Llutils: generic utility functions for LStar *)

(* LLVM modules *)
open Llvm
open TypeKind
(* coreStar modules *)
open Psyntax

(*** Generic sugar *)
let warn s = print_endline ("WARNING: "^s)
let implement_this s = failwith ("Not implemented: "^s)

let add_logic l1 l2 =
  { seq_rules = l1.seq_rules @ l2.seq_rules;
    rw_rules = l1.rw_rules @ l2.rw_rules;
    consdecl = l1.consdecl @ l2.consdecl;
    dummy = l1.dummy; (* this doesn't seem to be used by coreStar... *) }

(*** Overcoming the shortcomings of LLVM OCaml bindings *)

(** used to catch metadata information in values *)
exception MetaData of llvalue

let idMap = Hashtbl.create 1000
let llcontext = ref (global_context ())
let lltarget = ref (Llvm_target.TargetData.create "")

let string_of_struct s =
  match struct_name s with
  | None -> string_of_lltype s
  | Some name -> name

(** gets names of named and unnamed variables *)
(* Ideally, we would get them from the SlotTracker thingy in
   lib/VMCore/AsmWriter.cpp, but it's not in the ocaml bindings... *)
let value_id v =
  let id = value_name v in
  if id = "" then
    try
      Hashtbl.find idMap v
    with Not_found ->
      let id = "%"^(string_of_int (Hashtbl.length idMap + 1)) in
      Hashtbl.add idMap v id;
      id
  else id

(** extract the location of instruction [instr] from the debug information *)
let location_of_instr instr =
  let dbg_mdkind_id = mdkind_id !llcontext "dbg" in
  match metadata instr dbg_mdkind_id with
  | None -> None
  | Some md ->
    let line, col =
      match (int64_of_const (operand md 0), int64_of_const (operand md 1)) with
      | Some l, Some c -> Int64.to_int l, Int64.to_int c
      | _ -> assert false (* I wish the bindings were typed somehow... *) in
    let loc = {Printing.begin_line = line; Printing.begin_column = col;
	       Printing.end_line = line;   Printing.end_column = col; } in
    Some loc

(** extract and record the original file name for module [m] *)
let set_source_name m =
  match get_named_metadata m "llvm.dbg.cu" with
  | [||] -> ()
  | amd ->
    let md = Array.get amd 0 in
    match get_mdstring (operand md 4), get_mdstring (operand md 3) with
    | Some dir, Some file ->
      let src_name = Filename.concat dir file in
      Config.source_file := src_name;
      Config.source_base_name := Filename.basename src_name
    | _ -> ()


(*** Collect all the types in a module *)
module LlvalueSet = Set.Make (struct type t = llvalue let compare = compare end)
module LltypeSet = Set.Make (struct type t = lltype let compare = compare end)

let rec collect_type (seen_t, seen_const) t =
  if (LltypeSet.mem t seen_t) then (seen_t, seen_const)
  else
    let seen_t = LltypeSet.add t seen_t in
    match (classify_type t) with
      | Struct ->
	Array.fold_left collect_type (seen_t, seen_const) (struct_element_types t)
      | Pointer
      | Array
      | Vector -> collect_type (seen_t, seen_const) (element_type t)
      | _ -> (seen_t, seen_const)

(** looks for new types in the value v *)
let rec collect_types_in_value o v =
  let (seen_t, seen_const) = collect_type o (type_of v) in
  (*  if v is not a constant, then its structs have already been collected explicitly *)
  if (not (is_constant v)) || (LlvalueSet.mem v seen_const) then (seen_t, seen_const)
  else
    let seen_const = LlvalueSet.add v seen_const in
    let o = (seen_t, seen_const) in
    let num_op = num_operands v in
    let rec collect_from_op o n =
      if n = num_op then o
      else (
	let o = collect_types_in_value o (operand v n) in
	collect_from_op o (n+1)) in
    collect_from_op o 0

(** looks for new types in the instruction i *)
let collect_types_in_instr o i =
  let num_op = num_operands i in
  let o = collect_type o (type_of i) in
  let rec collect_from_op o n =
    if n = num_op then o
    else (
      let o = collect_types_in_value o (operand i n) in
      collect_from_op o (n+1)) in
  collect_from_op o 0

let collect_types_in_block o b =
  fold_left_instrs collect_types_in_instr o b

let collect_types_in_function o f =
  let o = collect_type o (type_of f) in
  fold_left_blocks collect_types_in_block o f

(** collects all the types referred to by the functions of module [m] *)
let collect_types_in_module m =
  (* we only care about the types that are used inside functions,
     so let's collect only those *)
  let o = (LltypeSet.empty,LlvalueSet.empty) in
  let (typs, _) = fold_left_functions collect_types_in_function o m in
  LltypeSet.elements typs


(*** pretty printing *)
let pp_spec fmt (pre,post) =
  Format.fprintf fmt "@[{%a}\n{%a}\n@."
    Sepprover.string_inner_form pre Sepprover.string_inner_form post

(** dump things into files in the output directory *)
let dump_into_file suffix pp_stuff stuff =
  let fname = Filename.concat !Lstar_config.outdir
    (!Lstar_config.bitcode_base_name ^ "." ^ suffix) in
  let file_out = open_out fname in
  let file_fmt = Format.formatter_of_out_channel file_out in
  Format.fprintf file_fmt "@[%a@." pp_stuff stuff;
  close_out file_out
