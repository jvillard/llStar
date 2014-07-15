(*** Llutils: generic utility functions for llStar *)

open Corestar_std
(* LLVM modules *)
open Llvm
open TypeKind

(*** Uncomment this when llStar segfaults: it's most likely that the
     bindings are used incorrectly *)

(*
let debug_value_call s f v =
  Format.print_flush ();
  print_endline ("about to call "^s);
  dump_value v;
  Format.print_flush ();
  let result = f v in
  print_endline "did the call";
  result

let type_of v =
  debug_value_call "type_of" type_of v

let classify_value v =
  debug_value_call "classify_value" classify_value v

let instr_opcode v =
  debug_value_call "instr_opcode" instr_opcode v

let operand v i =
  debug_value_call ("operand"^(string_of_int i)) (fun v -> operand v i) v
*)

(*** Generic sugar *)
let warn s = print_endline ("WARNING: "^s)
let implement_this s = failwith ("Not implemented: "^s)


(*** Overcoming the shortcomings of LLVM OCaml bindings *)

(** used to catch metadata information in values *)
exception MetaData of llvalue

let idMap = Hashtbl.create 1000
let llmodule = ref None
let llcontext = ref (global_context ())
let lltarget = ref (Llvm_target.DataLayout.of_string "")

(* must be called only after initialisation *)
let get_llmodule () = from_some !llmodule

let lltype_of_name s =
  from_some (Llvm.type_by_name (get_llmodule ()) s)

let string_of_struct s = match struct_name s with
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
      (* TODO: restore *)
      (* let src_name = Filename.concat dir file in *)
      (* Config.source_file := src_name; *)
      (* Config.source_base_name := Filename.basename src_name *)
      ()
    | _ -> ()

(** extract a block's label *)
let label_of_bblock b = value_id (value_of_block b)


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


(** dump things into files in the output directory *)
let dump_into_file suffix pp_stuff stuff =
  let fname = !Llstar_config.bitcode_base_name ^ "." ^ suffix in
  let file_out = open_out fname in
  let file_fmt = Format.formatter_of_out_channel file_out in
  Format.fprintf file_fmt "@[%a@." pp_stuff stuff;
  close_out file_out

let mk_0 f = function
  | [] -> f
  | _ -> assert false
let mk_1 f = function
  | [a] -> f a
  | _ -> assert false
let mk_2 f = function
  | [a; b] -> f a b
  | _ -> assert false
let mk_3 f = function
  | [a; b; c] -> f a b c
  | _ -> assert false
