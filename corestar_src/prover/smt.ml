(********************************************************
   This file is part of coreStar
	src/prover/smt.ml
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   coreStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)


open Clogic
open Congruence
open Corestar_std
open Cterm
open Debug
open Format
open List
open Psyntax
open Smtsyntax
open Unix
open Config

exception SMT_error of string
exception SMT_fatal_error

(*let Config.smt_run = ref true;; *)
let smt_fdepth = ref 0;;
let smtout = ref Pervasives.stdin;;
let smtin = ref Pervasives.stderr;;
let smterr = ref Pervasives.stdin;;
let smtpath = ref ""

let smtout_lex = ref (Lexing.from_string "");;

let smt_memo = Hashtbl.create 31;;

let smt_onstack = ref [[]];;

let send_custom_commands =
  let decl_re = Str.regexp "[ \t]*([ \t]*declare-fun[ \t]+\\([^ \t()]+\\)" in
  fun () ->
    if !Config.smt_custom_commands <> "" then (
      let cc = open_in !Config.smt_custom_commands in
      try while true do begin
	let cmd = input_line cc in
	if log log_smt then printf "@[%s@." cmd;
	if Str.string_match decl_re cmd 0 then
	  predeclared := StringSet.add (Str.matched_group 1 cmd) !predeclared;
	output_string !smtin cmd;
	output_char !smtin '\n'
      end done
      with End_of_file -> close_in cc
    )

let smt_declare_datatype dtid decl =
  if log log_smt then printf "@[Sending out %s declaration: %s@." dtid decl;
  output_string !smtin decl; (* TODO: handle errors *)
  predeclared := StringSet.add dtid !predeclared

let smt_init () : unit =
  smtpath :=
    if (!Config.solver_path <> "")
    then !Config.solver_path
    else System.getenv "JSTAR_SMT_PATH";
  if !smtpath = "" then Config.smt_run := false
  else
    try
      begin
        if log log_smt then printf "@[Initialising SMT@.";
        let args = System.getenv "JSTAR_SMT_ARGUMENTS" in
        let command = Filename.quote !smtpath ^ " " ^ args in
        if log log_phase then
          fprintf logf "@[execute <%s>@]@\n" command;
        let o, i, e = Unix.open_process_full command (environment()) in
        smtout := o;  smtin := i;  smterr := e;
        smtout_lex := Lexing.from_channel !smtout;
        Config.smt_run := true;
        if log log_smt then printf "@[SMT running.@]";
        output_string i "(set-option :print-success false)\n";
        send_custom_commands ();
	add_native_bitvector_ops ();
	add_native_int_ops ();
	flush i
      end
    with
    | Unix_error(err,f,a) ->
      match err with
      | ENOENT -> printf "@[@{<b>ERROR:@} Bad path for SMT solver: %s@." a;
                  Config.smt_run := false
      | _ -> raise (Unix_error(err,f,a))


let smt_fatal_recover () : unit  =
  printf "@[<2>@{<b>SMT ERROR:@}@ ";
  printf "The SMT solver <%s> stopped unexpectedly.@." !smtpath;
  if log log_smt then
    begin
      printf "@[Error report from the solver:@.";
      try while true do printf "@[%s@." (input_line !smterr) done
      with End_of_file -> ()
    end;
  printf "@[Turning off SMT for this example.@.";
  ignore (Unix.close_process_full (!smtout, !smtin, !smterr));
  print_flush();
  Config.smt_run := false


(* Partition a list into sublists of equal elements *)
let rec equiv_partition
    (eq : 'a -> 'a -> bool)
    (xs : 'a list)
    : 'a list list =
  match xs with
  | x::xs ->
     let (e, xs') = partition (eq x) xs in
     let eqs = equiv_partition eq xs' in
     (x::e) :: eqs
  | [] -> []


(* construct all (unordered) pairs of list elements *)
let rec list_to_pairs
    (xs : 'a list)
    : ('a * 'a) list =
  match xs with
  | x::xs -> (map (fun y -> (x,y)) xs) @ list_to_pairs xs
  | [] -> []


(** feeds the typing context and type unification thingy with info
    gathered from the give args. *)
(* The type inference part is more or less the algorithm W. *)
let rec sexp_of_args = function
  | Arg_var v ->
    let vname = id_munge (Vars.string_var v) in
    let tv = lookup_type vname in
    (vname, tv)
  | Arg_string s ->
    let rxp = (Str.regexp "^\\(-?[0-9]+\\)") in
    let expr =
      if Str.string_match rxp s 0 then
	Str.matched_group 1 s
      else (
	(* if it's not an int that SMT-LIB will recognise, we need to
	   declare it as such (let's pretend strings are ints...) *)
	(* maintain uniqueness of bindings *)
	Hashtbl.remove typing_context ("string_const_"^s);
	Hashtbl.add typing_context ("string_const_"^s) SType_int;
	"string_const_"^s
      ) in
    (expr, SType_int)
  | Arg_op (binop, [a1;a2]) when List.mem_assoc binop !bin_ops ->
    (* SMT-LIB knows about these operations, so we don't add their
       types to the typing context *)
    let (e1, t1) = sexp_of_args a1 in
    let (e2, t2) = sexp_of_args a2 in
    let smt_binop = List.assoc binop !bin_ops in
    let expr = Printf.sprintf "(%s %s %s)" smt_binop e1 e2 in
    let smt_binop_t = lookup_type smt_binop in
    let result_t = SType_var (fresh_type_index ()) in
    unify smt_binop_t (SType_fun [([t1; t2], result_t)]);
    (expr, result_t)
  | Arg_op ("numeric_const", [Arg_string(a)]) -> (a, SType_int)
  | Arg_op ("bv_const", [Arg_string(sz); Arg_string(n)]) ->
    (Printf.sprintf "(_ bv%s %s)" n sz, SType_bv sz)
  | Arg_op (name, args) | Arg_cons (name, args) ->
    let op_name = id_munge ("op_"^name) in
    let (args_exp, args_types) = sexp_of_args_list args in
    let expr =
      if args = [] then op_name
      else Printf.sprintf "(%s %s)" op_name args_exp in
    let result_type = SType_var (fresh_type_index ()) in
    let op_type = lookup_type op_name in
    if name <> "tuple" then
      if args = [] then unify result_type op_type
      else unify op_type (SType_fun [(args_types, result_type)]);
    (expr, result_type)
  | Arg_record fldlist ->
    (* TODO: implement records *)
    ("", SType_var (fresh_type_index ()))
and sexp_of_args_list = function
  | [] -> ("", [])
  | a::al ->
    let (e, t) = sexp_of_args a in
    let (el, tl) = sexp_of_args_list al in
    (" " ^ e ^ el, t::tl)

let sexp_of_eq (a1, a2) =
  let (e1, t1) = sexp_of_args a1 in
  let (e2, t2) = sexp_of_args a2 in
  unify t1 t2;
  Printf.sprintf "(= %s %s)" e1 e2

let sexp_of_neq (a1, a2) =
  let (e1, t1) = sexp_of_args a1 in
  let (e2, t2) = sexp_of_args a2 in
  let t1, t2 = uf_find t1, uf_find t2 in
  if not (refines t1 t2) && not (refines t2 t1) then
    (* incompatible types! *)
    "true"
  else Printf.sprintf "(distinct %s %s)" e1 e2

let sexp_of_pred = function
  | (bip, (Arg_op ("tuple",[a1;a2]))) when List.mem_assoc bip intbinrels ->
    let (args_exp, args_types) = sexp_of_args_list [a1;a2] in
    let expr = Printf.sprintf "(%s %s)" (List.assoc bip intbinrels) args_exp in
    unify_list (SType_int::args_types);
    (expr, SType_bool)
  | ("@False", Arg_op ("tuple",[])) ->
    ("false", SType_bool)
  | (s, Arg_op ("tuple",al)) ->
    let name = id_munge("pred_"^s) in
    let (args_exp, args_types) = sexp_of_args_list al in
    let op_type = lookup_type name in
    unify op_type (SType_fun [(args_types, SType_bool)]);
    ((if al = [] then name
      else Printf.sprintf "(%s %s)" name args_exp),
     SType_bool)
  | _ -> failwith "TODO"

let rec sexp_of_form ts form =
  let eqs =
    map
      (fun (a1,a2) ->
	(get_pargs_norecs false ts [] a1, get_pargs_norecs false ts [] a2))
      form.eqs in
  let neqs =
    map
      (fun (a1,a2) ->
	(get_pargs_norecs false ts [] a1, get_pargs_norecs false ts [] a2))
      form.neqs in
  let eq_sexp = String.concat " " (map sexp_of_eq eqs) in
  let neq_sexp = String.concat " " (map sexp_of_neq neqs) in
  let disj_list =
    map
      (fun (f1,f2) ->
	let f1s = sexp_of_form ts f1 in
	let f2s = sexp_of_form ts f2 in
	"(or " ^ f1s ^ " " ^ f2s ^ ")")
      form.disjuncts in
  let disj_sexp = String.concat " " disj_list in
  let plain_list =
    map (fun u -> fst (sexp_of_pred u))
      (RMSet.map_to_list form.plain
	 (fun (s,r) -> (s, get_pargs_norecs false ts [] r))) in
  let plain_sexp = String.concat " " plain_list in
  let form_sexp = "(and true "^eq_sexp^" "^neq_sexp^" "^disj_sexp^" "^plain_sexp^")" in
  form_sexp

let sexp_of_sort s =
  (* lookup "final" type of id, ie the representative of idt *)
  match uf_find s with
  | SType_bool -> "Bool"
  | SType_int -> "Int"
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

let decl_sexp_of_typed_id id idt =
  match (uf_find idt) with
  | SType_fun l ->
    let add_one_tfun_decl (sexp, decls) (argtl, resl) =
      let decl =
	Printf.sprintf "(declare-fun %s (%s) %s)"
	  id (sexp_of_sort_list argtl) (sexp_of_sort resl) in
      if mem decl decls then (sexp, decls)
      else (sexp^decl, decl::decls) in
    fst (List.fold_left add_one_tfun_decl ("", []) l)
  | _ ->
    Printf.sprintf "(declare-fun %s () %s)" id (sexp_of_sort idt)


(*** Main SMT IO functions *)

let smt_listen () =
  match Smtparse.main Smtlex.token !smtout_lex with
    | Error e -> raise (SMT_error e)
    | response -> response

let smt_command
    (cmd : string)
    : unit =
  try
    if log log_smt then printf "@[%s@." cmd;
    print_flush();
    output_string !smtin cmd;
    output_string !smtin "\n";
    flush !smtin;
  with End_of_file | Sys_error _ -> raise SMT_fatal_error

let send_all_types () =
  let f id idt =
    if not (StringSet.mem id !predeclared) then
      smt_command (decl_sexp_of_typed_id id idt) in
  Hashtbl.iter f typing_context

let smt_assert (ass : string) : unit =
  let cmd = "(assert " ^ ass ^ " )" in
  smt_command cmd;
  smt_onstack := (cmd :: List.hd !smt_onstack) :: List.tl !smt_onstack

let smt_check_sat () : bool =
    try
      let x = Hashtbl.find smt_memo !smt_onstack in
      if log log_smt then printf "@[[Found memoised SMT call!]@.";
      x
    with Not_found ->
      smt_command "(check-sat)";
      let x = match smt_listen () with
        | Sat -> true
        | Unsat -> false
        | Unknown -> if log log_smt then printf
          "@[[Warning: smt returned 'unknown' rather than 'sat']@."; true
        | _ -> failwith "TODO" in
      if log log_smt then printf "@[  %b@." x;
      Hashtbl.add smt_memo !smt_onstack x;
      x

let smt_check_unsat () : bool =
  not (smt_check_sat ())

let smt_push () : unit =
  smt_command "(push)";
  incr smt_fdepth;
  smt_onstack := ([]::!smt_onstack)

let smt_pop () : unit =
  smt_command "(pop)";
  decr smt_fdepth;
  smt_onstack := List.tl !smt_onstack


let smt_reset () : unit =
  for i = 1 to !smt_fdepth do smt_pop () done;
  assert (!smt_fdepth = 0);
  assert (!smt_onstack = [[]])


(** Check whether two args are equal under the current assumptions *)
let smt_test_eq (a1 : Psyntax.args) (a2 : Psyntax.args) : bool =
  let s = sexp_of_neq (a1,a2) in
  smt_push();
  smt_assert s;
  let r = smt_check_unsat() in
  smt_pop(); r

let exists_sexp idl sexp =
  (* lookup "final" type of id, ie the representative of idt *)
  let exists_decls =
    fold_left
      (fun s id ->
	Printf.sprintf "%s (%s %s)" s id (sexp_of_sort (lookup_type id)))
      ""
      idl in
  if idl = [] then sexp
  else Printf.sprintf "(exists %s %s)" exists_decls sexp

(** try to establish that the pure parts of a sequent are valid using the SMT solver *)
let finish_him
    (ts : term_structure)
    (asm : formula)
    (obl : formula)
    : bool =
  try
    reset_typing_context ();
    let eqs = filter (fun (a,b) -> a <> b) (get_eqs_norecs ts) in
    let neqs = filter (fun (a,b) -> a <> b) (get_neqs_norecs ts) in
    let asm_eq_sexp = String.concat " " (map sexp_of_eq eqs) in
    let asm_neq_sexp = String.concat " " (map sexp_of_neq neqs) in
    let _ = map sexp_of_args (get_args_all ts) in
    let asm_sexp = sexp_of_form ts asm in
    let obl_sexp = sexp_of_form ts obl in
    (* Construct the query *)
    let asm_sexp = "(and true "^asm_eq_sexp^" "^asm_neq_sexp^" "^asm_sexp^") " in
    let eq_neq_vset =
      ev_args_list (let (a,b) = split (eqs@neqs) in a@b) VarSet.empty in
    let eq_neq_ts_vset = ev_args_list (get_args_all ts) eq_neq_vset in
    let rec add_ev_of_form_to_vset form vset =
      let vset_with_disjs =
	fold_left
	  (fun evset (f1,f2) ->
	    add_ev_of_form_to_vset f2 (add_ev_of_form_to_vset f1 evset))
	  vset
	  form.disjuncts in
      RMSet.fold_to_list form.plain
	(fun (_,r) evset ->
	  let args = get_pargs_norecs false ts [] r in
	  ev_args args evset)
	vset_with_disjs in
    let left_vset = add_ev_of_form_to_vset asm eq_neq_ts_vset in
    let obl_vset = add_ev_of_form_to_vset obl VarSet.empty in
    let evars = VarSet.diff obl_vset left_vset in
    let evars_ids = List.map (fun v -> id_munge (Vars.string_var v))
      (VarSet.elements evars) in
    let obl_sexp = exists_sexp evars_ids obl_sexp in
    let query = "(not (=> " ^ asm_sexp ^ obl_sexp ^ "))" in

    smt_push(); (* Push a frame to allow reuse of prover *)
    send_all_types ();
    smt_assert query;
    (* check whether the forumula is unsatisfiable *)
    let r = smt_check_unsat() in
    smt_pop(); r
  with
  | Type_mismatch (ta, tb) ->
    printf "@[@{<b>SMT ERROR@}: type mismatch: %a # %a@."
      pp_smt_type ta pp_smt_type tb;
    print_flush();
    false
  | SMT_error r ->
    smt_reset();
    printf "@[@{<b>SMT ERROR@}: %s@." r;
    print_flush();
    false
  | SMT_fatal_error ->
    smt_fatal_recover();
    false


let true_sequent_smt (seq : sequent) : bool =
  (Clogic.true_sequent seq)
    ||
  (* Call the SMT if the other check fails *)
  (if (not !Config.smt_run) then false
  else
  (Clogic.plain seq.assumption  &&  Clogic.plain seq.obligation
    &&
   ((if log log_smt then printf "@[Calling SMT to prove@\n %a@." Clogic.pp_sequent seq);
    finish_him seq.ts seq.assumption seq.obligation)))


let frame_sequent_smt (seq : sequent) : bool =
  (Clogic.frame_sequent seq)
    ||
  (if (not !Config.smt_run) then false
  else
  (Clogic.plain seq.obligation
    &&
   ((if log log_smt then printf "@[Calling SMT to get frame from@\n %a@." Clogic.pp_sequent seq);
    finish_him seq.ts seq.assumption seq.obligation)))


(* Update the congruence closure using the SMT solver *)
let ask_the_audience
    (ts : term_structure)
    (form : formula)
    : term_structure =
  if (not !Config.smt_run) then raise Backtrack.No_match;
  try
    if log log_smt then
      begin
        printf "@[Calling SMT to update congruence closure@.";
        printf "@[Current formula:@\n %a@." Clogic.pp_ts_formula (Clogic.mk_ts_form ts form)
      end;
    reset_typing_context ();
    (* Construct equalities and ineqalities from ts *)
    let eqs = filter (fun (a,b) -> a <> b) (get_eqs_norecs ts) in
    let neqs = filter (fun (a,b) -> a <> b) (get_neqs_norecs ts) in
    let ts_eq_sexp = String.concat " " (map sexp_of_eq eqs) in
    let ts_neq_sexp = String.concat " " (map sexp_of_neq neqs) in
    (* get types from ts *)
    let _ = map sexp_of_args (get_args_all ts) in
    let form_sexp = sexp_of_form ts form in
    (* Assert the assumption *)
    let assm_query = "(and true " ^ ts_eq_sexp ^" "^ ts_neq_sexp ^" "^ form_sexp ^ ")" in

    smt_push(); (* Push a frame to allow reuse of prover *)
    (* declare predicates *)
    send_all_types ();
    smt_assert assm_query;
    (* check for a contradiction *)
    if log log_smt then printf "@[[Checking for contradiction in assumption]@.";
    if smt_check_unsat() then (smt_reset(); raise Assm_Contradiction);
    (* check whether there are any new equalities to find; otherwise raise Backtrack.No_match *)
    (*
    if log log_smt then printf "[Checking for new equalities]@\n";
    smt_push();
    let reps = get_args_rep ts in
    let rep_sexps = String.concat " " (map (fun (x,y) -> string_sexp_neq (snd x,snd y))
                                                (list_to_pairs reps) )
    in
    smt_assert ( "(and true " ^ rep_sexps ^ " )" );
    if smt_check_sat() then (smt_reset(); raise Backtrack.No_match);
    smt_pop();
    *)
    (* Update the term structure using the new equalities *)
    let reps = get_args_rep ts in
    let req_equiv = map (map fst)
      (equiv_partition (fun x y -> smt_test_eq (snd x) (snd y)) reps) in
    if for_all (fun ls -> List.length ls = 1) req_equiv then
      (smt_reset(); raise Backtrack.No_match);
    smt_pop();
    fold_left make_list_equal ts req_equiv
    with
    | Type_mismatch (ta, tb) ->
      printf "@[@{<b>SMT ERROR@}: type mismatch: %a # %a@."
	pp_smt_type ta pp_smt_type tb;
      smt_reset();
      print_flush();
      raise Backtrack.No_match
    | SMT_error r ->
      smt_reset();
      printf "@[@{<b>SMT ERROR@}: %s@." r;
      print_flush();
      raise Backtrack.No_match
    | SMT_fatal_error ->
      smt_fatal_recover();
      raise Backtrack.No_match

