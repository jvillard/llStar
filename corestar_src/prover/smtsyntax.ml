(********************************************************
   This file is part of coreStar
	src/prover/smtsyntax.ml
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   coreStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)

open Format
open List
open Corestar_std
open Debug
open Psyntax

type smt_response =
  | Unsupported
  | Error of string
  | Sat
  | Unsat
  | Unknown

module Regexp = struct type t = Str.regexp let compare = compare end
module RegexpSet = Set.Make (Regexp)

let predeclared = ref RegexpSet.empty

let is_predeclared id =
  RegexpSet.exists (fun r -> Str.string_match r id 0) !predeclared

(* This function should be used below to munge all symbols (usually known as
  identifiers). See Section 3.1 of SMT-LIB standard for allowed symbols. *)
(* TODO: Munge keywords such as par, NUMERAL, _, as, let. *)
let id_munge =
  let ok_char = Array.make 256 false in
  let range_ok a z =
    for c = Char.code a to Char.code z do ok_char.(c) <- true done in
  range_ok 'a' 'z'; range_ok 'A' 'Z'; range_ok '0' '9';
  String.iter (fun c -> ok_char.(Char.code c) <- true) "~!@$^&*_+=<>?/-";
  fun s ->
    let n = String.length s in
    let rec ok i = i = n || (ok_char.(Char.code s.[i]) && ok (succ i)) in
    if ok 0 then s else begin
      let r = Buffer.create (n + 2) in
      Buffer.add_char r '|';
      String.iter
        (function '|' -> Buffer.add_string r "PIPE" | c -> Buffer.add_char r c)
        s;
      Buffer.add_char r '|';
      Buffer.contents r
    end


(** Datatype to hold smt type annotations *)
type smt_type =
| SType_var of int (** type variable *)
| SType_elastic_bv of int (** bitvector of unknown size with a type id *)
| SType_bv of string (** bitvector of known size *)
| SType_bool (** boolean sort *)
| SType_int (** mathematical integer sort *)
| SType_type of string (** user-defined type *)
| SType_fun of (smt_type list * smt_type) list (** function type *)
(* I don't think SMT-LIB does higher-order but hopefully it won't show up... *)

exception Type_mismatch of smt_type * smt_type

let rec pp_smt_type f = function
| SType_var i -> fprintf f "tvar%d" i
| SType_elastic_bv i -> fprintf f "(_ BitVec bvar%d)" i
| SType_bv sz -> fprintf f "(_ BitVec %s)" sz
| SType_bool -> fprintf f "Bool"
| SType_int -> fprintf f "Int"
| SType_type s -> fprintf f "%s" s
| SType_fun l ->
  let pp_single_funt f (stl, rt) =
    fprintf f "(%a) -> %a" (list_format "" pp_smt_type) stl pp_smt_type rt in
  (list_format " U" pp_single_funt) f l


let rec refines ta tb =
  match (ta, tb) with
  | (SType_var i, SType_var j) -> j <= i
  | (_, SType_var _) -> true
  | (SType_elastic_bv i, SType_elastic_bv j) -> j <= i
  | (SType_bv _, SType_elastic_bv _) -> true
  | (SType_elastic_bv _, SType_bv _) -> false
  | (SType_int, SType_int) | (SType_bool, SType_bool) -> true
  | (SType_type s1, SType_type s2) when s1 = s2 -> true
  | (SType_fun la, SType_fun lb) ->
    List.for_all (fun (targsb, trb) ->
      List.exists (fun (targsa, tra) ->
	length targsa = length targsb && refines tra trb &&
	  (List.for_all2 refines targsb targsa))
	la)
      lb
  | (SType_var _, _) -> false
  | _ -> false

let compatible ta tb =
  refines ta tb or refines tb ta

let rec compatible_list tla tlb =
  match (tla, tlb) with
  | ([], []) -> true
  | (ta::tla, tb::tlb) -> compatible ta tb && (compatible_list tla tlb)
  | _ -> raise (Invalid_argument "mismatching list lengths in compatible_list")

(*** naive implementation of a union-find structure *)
let uf = Hashtbl.create 256
let uf_find i =
  let rec lookup i = try lookup (Hashtbl.find uf i) with Not_found -> i in
  lookup i
let uf_union i j =
  if i <> j then
    let ri = uf_find i in
    let rj = uf_find j in
    if ri <> rj then
      if refines ri rj then Hashtbl.add uf rj ri
      else Hashtbl.add uf ri rj

(** unification of types *)
let unify ta tb =
  let ta = uf_find ta in
  let tb = uf_find tb in
  let rec aux (ta, tb) =
    match (ta, tb) with
    | (SType_var _, _) | (_, SType_var _)
    | (SType_elastic_bv _, SType_elastic_bv _)
    | (SType_elastic_bv _, SType_bv _) | (SType_bv _, SType_elastic_bv _) ->
      uf_union ta tb
    | (SType_bv s1, SType_bv s2) when s1 = s2 -> ()
    | (SType_type s1, SType_type s2) when s1 = s2 -> ()
    | (SType_int, SType_int) | (SType_bool, SType_bool) -> ()
    | (SType_fun tla, SType_fun tlb) ->
      let unify_tfun (la, ra) (lb, rb) =
	try
	  if compatible_list (ra::la) (rb::lb) then
	    (iter aux ((ra,rb)::(combine la lb)); true)
	  else false
	with Invalid_argument _ -> false in
      (* if there is a type in tla that's compatible with a type in
	 tlb, fine (and recursive unification on the arguments/result
	 will have then been performer by unify_tfun). Otherwise,
	 overload the function type with a new type. *)
      if List.exists (fun ft -> List.exists (unify_tfun ft) tlb) tla then ()
      else uf_union ta (SType_fun (tla@tlb))
    | _ ->
      raise (Type_mismatch (ta, tb)) in
  aux (ta, tb)

let rec unify_list = function
  | a::b::tl -> unify a b; unify_list (b::tl)
  | _::[] | [] -> ()

(** next fresh index for type variables and elastic bitvectors *)
let __typeindex = ref 0
let fresh_type_index () =
  __typeindex := !__typeindex +1;
  !__typeindex -1

let rec complete_concat_funt = function
  | ([ti; tj], tk)::tl ->
    let (ui, uj, uk) =
      match (uf_find ti, uf_find tj, uf_find tk) with
      | (SType_bv i, SType_bv j, _) ->
	let k = string_of_int ((int_of_string i) + (int_of_string j)) in
	(SType_bv i, SType_bv j, SType_bv k)
      | (SType_bv i, _, SType_bv k) ->
	let j = string_of_int ((int_of_string k) - (int_of_string i)) in
	(SType_bv i, SType_bv j, SType_bv k)
      | (_ , SType_bv j, SType_bv k) ->
	let i = string_of_int ((int_of_string k) - (int_of_string j)) in
	(SType_bv i, SType_bv j, SType_bv k)
      | a -> a in
    ([ui; uj], uk)::(complete_concat_funt tl)
  | a::tl -> a::(complete_concat_funt tl)
  | [] -> []

(** typing context *)
let typing_context : (string, smt_type) Hashtbl.t = Hashtbl.create 256
let lookup_type id =
  (* horrible special cases... *)
  let extract_regexp = Str.regexp "(_ extract \\([0-9]+\\) \\([0-9]+\\))" in
  if Str.string_match extract_regexp id 0 then
    let i = int_of_string (Str.matched_group 1 id) in
    let j = int_of_string (Str.matched_group 2 id) in
    SType_fun [([SType_elastic_bv (fresh_type_index ())],
		SType_bv (string_of_int (i-j+1)))]
  else
    try
      let t = uf_find (Hashtbl.find typing_context id) in
      (* another horrible hack *)
      if id = "concat" then
	match t with
	| SType_fun l ->
	  let new_t = SType_fun (complete_concat_funt l) in
	  unify t new_t; new_t
	| _ -> t
      else t
    with Not_found ->
      let t = SType_var (fresh_type_index ()) in    
      Hashtbl.add typing_context id t;
      t

(* should this be a hashtbl? *)
let default_types = ref []

let reset_typing_context () =
  Hashtbl.clear typing_context;
  List.iter (fun (id,typ) -> Hashtbl.add typing_context id typ) !default_types

let dump_typing_context () =
  Hashtbl.iter (fun id t -> fprintf logf "%s: %a@ " id pp_smt_type (uf_find t)) typing_context

let add_default_type id typ =
  default_types := (id,typ)::!default_types

let native_ops : (string * (Psyntax.args list -> (string * Psyntax.args list))) list ref = ref []

let add_native_op args_op smt_op_regexp mk_smt_op optype =
  predeclared := RegexpSet.add smt_op_regexp !predeclared;
  (*add_default_type smt_op_string optype;
    Hashtbl.add typing_context smt_op_string optype;*)
  native_ops := (args_op, mk_smt_op)::!native_ops

(** bitvector operations *)
let add_native_bitvector_ops () =
  (* bit-vector operations in SMT-LIB are polymorphic in the size of
     the bit-vectors. The type of a bit-vector operation is that both
     arguments and the result are bit-vectors of the same size *)
  let bvop_t () =
    let t = SType_elastic_bv (fresh_type_index ()) in
    SType_fun [([t; t], t)] in
  List.iter (fun s ->
    let mk_bvop args = (s, args) in
    add_native_op ("builtin_"^s) (Str.regexp_string s) mk_bvop (bvop_t ()))
    ["bvadd"; "bvsub"; "bvneg"; "bvmul";
     "bvurem"; "bvsrem"; "bvsmod";
     "bvshl"; "bvlshr"; "bvashr";
     "bvor"; "bvand"; "bvnot"; "bvnand"; "bvnor"; "bvxnor"];
  add_native_op "builtin_bvconcat" (Str.regexp_string "concat") (fun args -> "concat", args) (bvop_t ());
  add_native_op "builtin_bvextract" (Str.regexp "(_ extract [0-9]+ [0-9]+)")
    (function
  Arg_op("numeric_const",[Arg_string i])::Arg_op("numeric_const",[Arg_string j])::args
    | Arg_string i::Arg_string j::args -> Printf.sprintf "(_ extract %s %s)" i j, args
    | a -> "op_builtin_bvextract", a)
    (SType_fun [([SType_elastic_bv (fresh_type_index ())],
		 SType_elastic_bv (fresh_type_index ()))])

(** mathematical integer operations *)
let add_native_int_ops () =
  List.iter (fun (args_str, smt_str) ->
    let mk_op args = (smt_str, args) in
    add_native_op args_str (Str.regexp_string smt_str) mk_op
    (SType_fun [([SType_int; SType_int], SType_int)]))
    [("builtin_plus", "+");
     ("builtin_minus", "-");
     ("builtin_mult", "*");
     ("builtin_div", "/");
    ]

let add_native_int_rels () =
  List.iter (fun (args_str, smt_str) ->
    let mk_op args = (smt_str, args) in
    add_native_op args_str (Str.regexp_string smt_str) mk_op
    (SType_fun [([SType_int; SType_int], SType_bool)]))
    [("GT", ">");
     ("GE", ">=");
     ("LT", "<");
     ("LE", "<=");
    ]

let add_native_false () =
  add_native_op "@False" (Str.regexp_string "false")
    (function [] -> ("false", []) | a -> ("op_@False", a))
    (SType_bool)

let sexp_of_sort s =
  (* lookup "final" type of id, ie the representative of idt *)
  match uf_find s with
  | SType_bool -> "Bool"
  | SType_int -> "Int"
  | SType_type s -> s
  | SType_bv sz ->
    Printf.sprintf "(_ BitVec %s)" sz
  | SType_elastic_bv i ->
    (* if we don't know the size of the bit-vector at this point, we
       have to pick one. 12 is as good a size as any I guess... *)
    unify (SType_bv "12") (SType_elastic_bv i);
    "(_ BitVec 12)"
  | SType_var i ->
    (* let's decide that this identifier is of type Int *)
    unify SType_int (SType_var i);
    "Int"
  | SType_fun _ -> raise (Invalid_argument "Unexpected function type")

let rec sexp_of_sort_list = function
  | [] -> ""
  | t::tl -> " " ^ (sexp_of_sort t) ^ (sexp_of_sort_list tl)
