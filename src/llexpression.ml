(*** Llexpression: turn llvm types and values into Z3 expressions *)

open Format
(* LLVM modules *)
open Llvm
open TypeKind
open ValueKind
(* coreStar modules *)
open Corestar_std
open Debug
open Syntax
(* llStar modules *)
open Llutils

(*** sorts *)

let bool_sort = Z3.Boolean.mk_sort z3_ctx
let int_sort = int_sort (* from Syntax *)
let bv_sort sz = Z3.BitVector.mk_sort z3_ctx sz
let void_sort = Z3.Sort.mk_uninterpreted_s z3_ctx "void"
let bblabel_sort = Z3.Sort.mk_uninterpreted_s z3_ctx "bblabel"
let lltype_sort = Z3.Sort.mk_uninterpreted_s z3_ctx "lltype"
let jump_sort = Z3.Sort.mk_uninterpreted_s z3_ctx "jump"
let md_sort = Z3.Sort.mk_uninterpreted_s z3_ctx "metadata"
let pointer_sort = bv_sort 64
let size_sort = bv_sort 64
let idx_sort = bv_sort 64
let function_sort args_t ret_t = int_sort (* TODO *)
let array_sort elt_t = Z3.Z3Array.mk_sort z3_ctx idx_sort elt_t
let vector_sort sz elt_t = int_sort (* TODO *)

let struct_sort_map = Hashtbl.create 0

let rec named_sort n =
  (sort_of_lltype @@ from_some @@ type_by_name (get_llmodule ())) n

and sort_of_lltype t = match (classify_type t) with
  | Void -> void_sort
  | Float
  | Half
  | Double
  | X86fp80
  | X86_mmx
  | Fp128
  | Ppc_fp128 -> failwith "floats_not_supported"
  | Label -> bblabel_sort
  | Integer ->
    let sz = integer_bitwidth t in
    bv_sort sz
  | TypeKind.Function -> (* silly name clash *)
    let ret_type = sort_of_lltype (return_type t) in
    let par_types = sort_of_lltype_array (param_types t) in
    function_sort par_types ret_type
  | Struct ->
    if is_opaque t then named_sort (string_of_struct t)
    else struct_sort t
  | Array ->
    let elt_t = sort_of_lltype (element_type t) in
    array_sort elt_t
  | Pointer -> pointer_sort
  | Vector ->
    let elt_t = sort_of_lltype (element_type t) in
    vector_sort (vector_size t) elt_t
  | Metadata -> md_sort
and sort_of_lltype_array ta =
  Array.to_list (Array.map sort_of_lltype ta)

and struct_sort t = Hashtbl.find struct_sort_map t

let struct_as_fields_sort l =
  let is_it_this_struct t =
    let elts_t = struct_element_types t in
    let elts_s = List.map sort_of_lltype (Array.to_list elts_t) in
    elts_s = l in
  let f t s r =
    if r = None && is_it_this_struct t  then Some s
    else r in
  let r = Hashtbl.fold f struct_sort_map None in
  match r with
  | None -> raise Not_found (* TODO: error message *)
  | Some s -> s

(** creates constructors for struct types [stl]
    [stl] should contain all the struct types of the module *)
(* FIXME: probably doesn't work if a struct contains vectors or arrays
   of structs *)
let declare_struct_types_in_llmodule m =
  let all_types = collect_types_in_module m in
  let struct_filter t = match classify_type t with
    | Struct -> true
    | _ -> false in
  let stl = List.filter struct_filter all_types in
  prerr_endline ("found "^(string_of_int (List.length stl))^" structs");
  let struct_index t =
    let rec f i = function
      | tt::_ when tt = t -> i
      | _::tl -> f (i+1) tl
      | [] -> assert false in
    f 0 stl in
  let dummy_sort = Z3.Sort.mk_uninterpreted_s z3_ctx "dummy" in
  let one_struct t =
    prerr_endline "one_struct: begin";
    let struct_name = string_of_struct t in
    let struct_sym = Z3.Symbol.mk_string z3_ctx struct_name in
    let constr_sym =
      let s = "mk_"^struct_name in
      Z3.Symbol.mk_string z3_ctx s in
    let struct_recog =
      let s = "is_"^struct_name in
      Z3.Symbol.mk_string z3_ctx s in
    let elts = struct_element_types t in
    let elt_sym i =
      let s = Printf.sprintf "%s_fld%d" struct_name i in
      Z3.Symbol.mk_string z3_ctx s in
    let elts_syms = Array.to_list (Array.mapi (fun i _ -> elt_sym i) elts) in
    prerr_endline "one_struct: here";
    let one_elt et =
      try (sort_of_lltype et, 0)
      with Not_found ->
	(* element type was a struct and we haven't yet constructed
	   the table of structs, so return the index of the struct
	   instead *)
	(dummy_sort, struct_index t) in
    let elts_sorts_and_indices = List.map one_elt (Array.to_list elts) in
    let (elts_sorts, indices) = List.split elts_sorts_and_indices in
    prerr_endline "one_struct: almost";
    (struct_sym, [Z3.Datatype.mk_constructor z3_ctx
		     constr_sym struct_recog elts_syms elts_sorts indices]) in
  let (s, c) = List.split (List.map one_struct stl) in
  prerr_endline "all structs: done";
  (* FIXME: understand how to do mutually recursive sorts in Z3 *)
  (* let sorts = *)
  (*   if s = [] then [] *)
  (*   else if List.length s = 1 then [Z3.Datatype.mk_sort z3_ctx (List.hd s) (List.hd c)] *)
  (*   else Z3.Datatype.mk_sorts z3_ctx s c in *)
  let sorts = List.map2
  prerr_endline "sorts done";
  List.iter2 (fun st so ->
    fprintf logf "Adding struct %s with sort %s@?@\n" (Llvm.string_of_lltype st) (Z3.Sort.to_string so);
    Hashtbl.add struct_sort_map st so) stl sorts

(*** /sorts *)


(*** helpers for mk_*_type *)
let bv_fun = Z3.FuncDecl.mk_func_decl_s z3_ctx "bv" [int_sort] lltype_sort
let rec repeat n e = if n <= 0 then [] else e::(repeat (n-1) e)
let named_cons =
  Z3.FuncDecl.mk_func_decl_s z3_ctx
    "named" [lltype_sort] lltype_sort
(*** /helpers *)

let mk_int_type = Z3.Expr.mk_const_s z3_ctx "int" lltype_sort
let mk_bv_type sz =
  let sz = Z3.Arithmetic.Integer.mk_numeral_i z3_ctx sz in
  Z3.FuncDecl.apply bv_fun [sz]
let mk_fp_type t = failwith "floats not supported"
let mk_void_type = Z3.Expr.mk_const_s z3_ctx "void" lltype_sort
let mk_label_type = Z3.Expr.mk_const_s z3_ctx "label" lltype_sort
let mk_named_type s =
  (* TODO: should probably keep these in a map instead of creating them on the fly *)
  let v = Syntax.mk_plvar lltype_sort s in
  Z3.FuncDecl.apply named_cons [v]
let mk_opaque_type = Z3.Expr.mk_const_s z3_ctx "opaque" lltype_sort
let mk_struct_type elts_t =
  let n = List.length elts_t in
  let s = Z3.FuncDecl.mk_func_decl_s z3_ctx "struct" (repeat n lltype_sort) lltype_sort in
  Z3.FuncDecl.apply s elts_t
let mk_function_type args_t ret_t =
  let n = List.length args_t + 1 in
  let s = Z3.FuncDecl.mk_func_decl_s z3_ctx "function" (repeat n lltype_sort) lltype_sort in
  Z3.FuncDecl.apply s (ret_t::args_t)
let mk_pointer_type elt_t =
  let s = Z3.FuncDecl.mk_func_decl_s z3_ctx "pointer_type" [lltype_sort] lltype_sort in
  Z3.FuncDecl.apply s [elt_t]
let mk_vector_type size elt_t =
  let s = Z3.FuncDecl.mk_func_decl_s z3_ctx "vector" [size_sort; lltype_sort] lltype_sort in
  Z3.FuncDecl.apply s [size; elt_t]
let mk_array_type size elt_t =
  let s = Z3.FuncDecl.mk_func_decl_s z3_ctx "array" [size_sort; lltype_sort] lltype_sort in
  Z3.FuncDecl.apply s [size; elt_t]
let mk_metadata_type = Z3.Expr.mk_const_s z3_ctx "MD" lltype_sort

let mk_i8_type = mk_bv_type 8
let mk_i32_type = mk_bv_type 32
let mk_i64_type = mk_bv_type 64
let mk_ptrBV_type = mk_i64_type
let mk_void_pointer_type elt_t = mk_pointer_type mk_i8_type

(* shorthands for value expressions *)
let mk_int i = Z3.Arithmetic.Integer.mk_numeral_i z3_ctx i
let mk_int64 i = Z3.Arithmetic.Integer.mk_numeral_s z3_ctx (Int64.to_string i)
let mk_bv sz i = Z3.BitVector.mk_numeral z3_ctx i sz
let mk_bv64 sz i = mk_bv sz (Int64.to_string i)
let mk_fp t exp s = failwith "floats not supported"
let mk_void = Z3.Expr.mk_const_s z3_ctx "void" void_sort
let mk_struct s elts_v =
  Format.fprintf Debug.logf "making struct %s { %a }" (Z3.Sort.to_string s) (pp_list_sep ", " Syntax.pp_expr) elts_v;
  let cons = List.hd (Z3.Datatype.get_constructors s) in
  Z3.FuncDecl.apply cons elts_v
let mk_struct_llt t =
  let s = Hashtbl.find struct_sort_map t in
  mk_struct s
let mk_struct_field s i st =
  let accs = List.hd (Z3.Datatype.get_accessors s) in
  let acc = List.nth accs i in
  Z3.FuncDecl.apply acc [st]
let mk_struct_field_llt t =
  let s = Hashtbl.find struct_sort_map t in
  mk_struct_field s
let mk_array_val elts_v = failwith "TODO: mk_array_val"
let mk_vector_val elts_v = failwith "TODO: mk_vector_val"
let mk_null_ptr = mk_bv 64 "0"
let mk_undef t = Z3.Expr.mk_const_s z3_ctx "undef" t

(* predicates *)
let malloced_pred =
  Z3.FuncDecl.mk_func_decl_s z3_ctx
    "malloced" [pointer_sort; size_sort] bool_sort
let padding_pred =
  Z3.FuncDecl.mk_func_decl_s z3_ctx
    "pad" [pointer_sort; size_sort] bool_sort

let jump_end = Z3.Expr.mk_const_s z3_ctx "jump_end" jump_sort
let jump =
  Z3.FuncDecl.mk_func_decl_s z3_ctx "jump" [size_sort; jump_sort] jump_sort
let eltptr =
  Z3.FuncDecl.mk_func_decl_s z3_ctx
    "eltptr" [pointer_sort; lltype_sort; jump_sort] pointer_sort
let sizeof_fun =
  Z3.FuncDecl.mk_func_decl_s z3_ctx "sizeof" [lltype_sort] size_sort

let is_func_name name e =
  let s = Z3.FuncDecl.get_name (Z3.Expr.get_func_decl e) in
  Z3.Symbol.is_string_symbol s && Z3.Symbol.get_string s = name


let mk_pointer ptr v =
  let f = Z3.FuncDecl.mk_func_decl_s z3_ctx
    "pointer" [pointer_sort; Z3.Expr.get_sort v] bool_sort in
  Z3.FuncDecl.apply f [ptr; v]
let mk_array ptr size v =
  let f = Z3.FuncDecl.mk_func_decl_s z3_ctx
    "array" [pointer_sort; size_sort; Z3.Expr.get_sort v] bool_sort in
  Z3.FuncDecl.apply f [ptr; size; v]
let mk_malloced ptr sz = Z3.FuncDecl.apply malloced_pred [ptr; sz]
(** padding with [size] bytes at address [x] *)
let mk_padding x size = Z3.FuncDecl.apply padding_pred [x; size]
let mk_jump_end = jump_end
let mk_jump jhead jtail = Z3.FuncDecl.apply jump [jhead; jtail]
let mk_eltptr ptr t jchain = Z3.FuncDecl.apply eltptr [ptr; t; jchain]
let mk_sizeof t = Z3.FuncDecl.apply sizeof_fun [t]

let mk_pointer_size =
  let ptr_bitsize = (Llvm_target.DataLayout.pointer_size !lltarget) * 8 in
  mk_bv ptr_bitsize
    (string_of_int (Llvm_target.DataLayout.pointer_size !lltarget))
let field_type =
  Z3.FuncDecl.mk_func_decl_s z3_ctx "field_type" [lltype_sort; size_sort] lltype_sort
let mk_field_type st i = Z3.FuncDecl.apply field_type [st; i]
let offset =
  Z3.FuncDecl.mk_func_decl_s z3_ctx "offset" [lltype_sort; size_sort] size_sort
let mk_offset st i = Z3.FuncDecl.apply offset [st; i]
let mk_exploded_struct x v =
  let f = Z3.FuncDecl.mk_func_decl_s z3_ctx
    "exploded_struct" [pointer_sort; Z3.Expr.get_sort v] bool_sort in
  Z3.FuncDecl.apply f [x; v]

let mk_bitcast x t =
  let bt = Z3.FuncDecl.mk_func_decl_s z3_ctx
    "!bitcast" [pointer_sort; lltype_sort] bool_sort in
  Z3.FuncDecl.apply bt [x; t]

let is_pointer = is_func_name "pointer"
let is_array = is_func_name "array"
let is_exploded_struct = is_func_name "exploded_struct"

let expr_of_sizeof t =
  let size64 = Llvm_target.DataLayout.abi_size t !lltarget in
  mk_bv64 64 size64

let string_of_fptype t = match (classify_type t) with
  | Float -> "float"
  | Half -> "half"
  | Double -> "double"
  | X86fp80 -> "x86_fp80"
  | Fp128 -> "fp128"
  | Ppc_fp128 -> "ppc_fp128"
  | _ -> failwith "Passed a non-float type to string_of_fptype"

(** returns the number of bits in the exposant and the significand of a float type [t] *)
(* these are defined as in the "FloatingPoints" SMT theory, eg the
   size of significand includes the hidden bit. These values could be slightly off...*)
let eb_sb_of_fpt t = match (classify_type t) with
  | Half -> (5, 11)
  | Float -> (8, 24)
  | Double -> (11, 53)
  | X86fp80 -> (15, 64)
  | Fp128 -> (113, 15)
  | Ppc_fp128 -> (64, 64)
  | _ -> failwith "Passed a non-float type to eb_sb_of_fpt"


let rec expr_of_lltype t = match (classify_type t) with
  | Void -> mk_void_type
  | Float
  | Half
  | Double
  | X86fp80
  | X86_mmx
  | Fp128
  | Ppc_fp128 -> mk_fp_type (string_of_fptype t)
  | Label -> mk_label_type
  | Integer ->
    let sz = integer_bitwidth t in
    mk_bv_type sz
  | TypeKind.Function -> (* silly name clash *)
    let ret_type = expr_of_lltype (return_type t) in
    let par_types = expr_of_lltype_array (param_types t) in
    mk_function_type par_types ret_type
  | Struct -> (
    if is_opaque t then mk_opaque_type
    else match struct_name t with
	| None ->
	  let elts_types = struct_element_types t in
	  let elt_ta = expr_of_lltype_array elts_types in
	  mk_struct_type elt_ta
	| Some n -> mk_named_type n
  )
  | Array ->
    let elt_t = expr_of_lltype (element_type t) in
    let size = mk_bv 64 (string_of_int (array_length t)) in
    mk_array_type size elt_t
  | Pointer ->
    let elt_t = expr_of_lltype (element_type t) in
    mk_pointer_type elt_t
  | Vector ->
    let elt_t = expr_of_lltype (element_type t) in
    let size = mk_bv 64 (string_of_int (vector_size t)) in
    mk_vector_type size elt_t
  | Metadata -> mk_metadata_type
and expr_of_lltype_array ta =
  Array.to_list (Array.map expr_of_lltype ta)

let rec expr_zero_of_type t = match (classify_type t) with
  | Float
  | Half
  | Double
  | X86fp80
  | Fp128
  | Ppc_fp128 -> mk_fp (string_of_fptype t) "0" "0"
  | Integer ->
    let sz = integer_bitwidth t in
    mk_bv sz "0"
  | Struct when not (is_opaque t) ->
    let elts_types = struct_element_types t in
    let elt_za = expr_zero_of_type_array elts_types in
    mk_struct_llt t elt_za
  | Array ->
    let elt_z = expr_zero_of_type (element_type t) in
    mk_array_val (repeat (Llvm.array_length t) elt_z) (* TODO: can we trust this length? *)
  | Pointer -> mk_null_ptr
  | Vector ->
    let elt_z = expr_zero_of_type (element_type t) in
    mk_vector_val (repeat (Llvm.vector_size t) elt_z)
  | _ -> failwith ("PANIC: asked to generate a ConstantAggregateZero of a wrong type ")
and expr_zero_of_type_array ta =
  Array.to_list (Array.map expr_zero_of_type ta)

let expr_of_int_const v =
  let sz = integer_bitwidth (type_of v) in
  match int64_of_const v with
  | Some i -> mk_bv64 sz i
  | None -> mk_fresh_lvar bool_sort "i"

let expr_of_fp_const v =
  let fpt = sort_of_lltype (type_of v) in
  (* TODO: implement.
     even getting the fp constant from the bindings is non-trivial. *)
  mk_fresh_lvar fpt "f"

(** expression that returns (_ bv1 1) if [e] = [f] and (_ bv1 0) otherwise *)
let lleq e f =
  Z3.Boolean.mk_ite z3_ctx
    (Z3.Boolean.mk_eq z3_ctx e f)
    (mk_bv 1 "1")
    (mk_bv 1 "0")

(** expression that returns (_ bv1 1) if [e] != [f] and (_ bv1 0) otherwise *)
let llne e f =
  Z3.Boolean.mk_ite z3_ctx
    (Z3.Boolean.mk_distinct z3_ctx [e; f])
    (mk_bv 1 "1")
    (mk_bv 1 "0")

(** builds binary operation from constant expression or instruction *)
let rec expr_of_op opcode opval =
  let expr_of_binop bop =
    let x = expr_of_llvalue (operand opval 0) in
    let y = expr_of_llvalue (operand opval 1) in
    bop x y in
  match opcode with
  (* Standard Binary Operators *)
  | Opcode.Add -> expr_of_binop (Z3.BitVector.mk_add z3_ctx)
  | Opcode.Sub -> expr_of_binop (Z3.BitVector.mk_sub z3_ctx)
  | Opcode.Mul -> expr_of_binop (Z3.BitVector.mk_mul z3_ctx)
  | Opcode.UDiv -> expr_of_binop (Z3.BitVector.mk_udiv z3_ctx)
  | Opcode.SDiv -> expr_of_binop (Z3.BitVector.mk_sdiv z3_ctx)
  | Opcode.URem -> expr_of_binop (Z3.BitVector.mk_urem z3_ctx)
  | Opcode.SRem -> expr_of_binop (Z3.BitVector.mk_srem z3_ctx)
  (* Logical Operators *)
  | Opcode.Shl -> expr_of_binop (Z3.BitVector.mk_shl z3_ctx)
  | Opcode.LShr -> expr_of_binop (Z3.BitVector.mk_lshr z3_ctx)
  | Opcode.AShr -> expr_of_binop (Z3.BitVector.mk_ashr z3_ctx)
  | Opcode.And -> expr_of_binop (Z3.BitVector.mk_and z3_ctx)
  | Opcode.Or -> expr_of_binop (Z3.BitVector.mk_or z3_ctx)
  | Opcode.Xor -> expr_of_binop (Z3.BitVector.mk_xor z3_ctx)
  (* Conversions Operators *)
  | Opcode.BitCast ->
    let value = operand opval 0 in
    expr_of_llvalue value (* TODO: needs conversion sometimes? eg struct to int *)
  | Opcode.Trunc ->
    let value = operand opval 0 in
    let v = expr_of_llvalue value in
    let to_sz = Llvm_target.DataLayout.size_in_bits (type_of opval) !lltarget in
    let to_sz = Int64.to_int to_sz in
    Z3.BitVector.mk_extract z3_ctx (to_sz - 1) 0 v
  | Opcode.ZExt ->
    let value = operand opval 0 in
    let v = expr_of_llvalue value in
    let from_sz = Llvm_target.DataLayout.size_in_bits (type_of value) !lltarget in
    let from_sz = Int64.to_int from_sz in
    let to_sz = Llvm_target.DataLayout.size_in_bits (type_of opval) !lltarget in
    let to_sz = Int64.to_int to_sz in
    let i = to_sz - from_sz in
    Z3.BitVector.mk_zero_ext z3_ctx i v
  | Opcode.SExt ->
    let value = operand opval 0 in
    let v = expr_of_llvalue value in
    let from_sz = Llvm_target.DataLayout.size_in_bits (type_of value) !lltarget in
    let from_sz = Int64.to_int from_sz in
    let to_sz = Llvm_target.DataLayout.size_in_bits (type_of opval) !lltarget in
    let to_sz = Int64.to_int to_sz in
    let i = to_sz - from_sz in
    Z3.BitVector.mk_sign_ext z3_ctx i v
  | Opcode.PtrToInt
  | Opcode.IntToPtr ->
    let value = operand opval 0 in
    let v = expr_of_llvalue value in
    let from_sz = Llvm_target.DataLayout.size_in_bits (type_of value) !lltarget in
    let from_sz = Int64.to_int from_sz in
    let to_sz = Llvm_target.DataLayout.size_in_bits (type_of opval) !lltarget in
    let to_sz = Int64.to_int to_sz in
    if from_sz < to_sz then
      Z3.BitVector.mk_zero_ext z3_ctx (to_sz - from_sz) v
    else if from_sz = to_sz then
      v
    else (* from_sz > to_sz *)
      Z3.BitVector.mk_extract z3_ctx (to_sz - 1) 0 v
  (* Other Operators *)
  | Opcode.GetElementPtr ->
    let value = operand opval 0 in
    (* jump indices are in operands 1 to (num_operands opval) of opval *)
    let max_op = num_operands opval in
    let rec jlist_from_op i =
      if i = max_op then []
      else operand opval i::(jlist_from_op (i+1)) in
    let rec jump_chain_of_lidx = function
      | [] -> mk_jump_end
      | idx::tl ->
	(* convert constant integers to 64 bits *)
	let idx = match int64_of_const idx with
	  | None -> idx
	  | Some i -> const_of_int64 (integer_type !llcontext 64) i false in
	let expr_idx = expr_of_llvalue idx in
	mk_jump expr_idx (jump_chain_of_lidx tl) in
    let jump_chain = jump_chain_of_lidx (jlist_from_op 1) in
    mk_eltptr (expr_of_llvalue value) (expr_of_lltype (type_of value)) jump_chain
  | Opcode.ICmp ->
    let op = match icmp_predicate opval with
      | None -> assert false
      | Some p -> match p with
	  | Icmp.Eq -> lleq
	  | Icmp.Ne -> llne
	  | Icmp.Ugt -> Z3.BitVector.mk_ugt z3_ctx
	  | Icmp.Sgt -> Z3.BitVector.mk_sgt z3_ctx
	  | Icmp.Uge -> Z3.BitVector.mk_uge z3_ctx
	  | Icmp.Sge -> Z3.BitVector.mk_sge z3_ctx
	  | Icmp.Ult -> Z3.BitVector.mk_ult z3_ctx
	  | Icmp.Slt -> Z3.BitVector.mk_slt z3_ctx
	  | Icmp.Ule -> Z3.BitVector.mk_ult z3_ctx
	  | Icmp.Sle -> Z3.BitVector.mk_sle z3_ctx in
    let v1 = expr_of_llvalue (operand opval 0) in
    let v2 = expr_of_llvalue (operand opval 1) in
    op v1 v2
  | Opcode.FAdd
  | Opcode.FSub
  | Opcode.FMul
  | Opcode.FDiv
  | Opcode.FRem
  | Opcode.FPToUI
  | Opcode.FPToSI
  | Opcode.UIToFP
  | Opcode.SIToFP
  | Opcode.FPTrunc
  | Opcode.FPExt
  | Opcode.FCmp (* huh? cannot get the comparison operation from the bindings! *)
    -> failwith "floats not supported"
  | Opcode.Select
  | Opcode.ExtractElement
  | Opcode.InsertElement
  | Opcode.ShuffleVector
  | Opcode.ExtractValue
  | Opcode.InsertValue -> implement_this "vector operations"
  | Opcode.LandingPad | Opcode.Resume | Opcode.AtomicRMW
  | Opcode.AtomicCmpXchg | Opcode.Fence | Opcode.VAArg | Opcode.UserOp2
  | Opcode.UserOp1 | Opcode.Call | Opcode.PHI | Opcode.Store | Opcode.Load
  | Opcode.Alloca | Opcode.Unreachable | Opcode.Invalid2 | Opcode.Invoke
  | Opcode.IndirectBr | Opcode.Switch | Opcode.Br | Opcode.Ret
  | Opcode.Invalid ->
    (* these opcodes are not allowed in constant expressions according
       to LLVM language reference *)
    dump_value opval;
    failwith "Unexpected operation in expression."

and expr_of_const_expr v = expr_of_op (constexpr_opcode v) v

and expr_of_llvalue v = match classify_value v with
  | NullValue -> (* LLVM unallocated value *) failwith "Got invalid value from bindings (NullValue)"
  | BasicBlock -> failwith "Invalid bitcode? Unexpected BasickBlock value"
  | MDNode -> raise (MetaData v)
  | MDString -> raise (MetaData v)
  | BlockAddress -> implement_this "blockaddress values"
  | ConstantAggregateZero -> expr_zero_of_type (type_of v)
  | ConstantArray
  | ConstantDataArray -> implement_this "ConstantDataArray values"
  | ConstantExpr -> expr_of_const_expr v
  | ConstantFP -> expr_of_fp_const v
  | ConstantInt -> expr_of_int_const v
  | ConstantPointerNull -> mk_null_ptr
  | ConstantStruct -> mk_struct_llt (type_of v) (expr_of_composite_llvalue v)
  | ConstantVector
  | ConstantDataVector -> mk_vector_val (expr_of_composite_llvalue v)
  | Function -> implement_this "function values"
  | GlobalAlias -> implement_this "global alias values" (* undocumented? *)
  | UndefValue -> mk_undef (sort_of_lltype (type_of v))
  | Argument
  | InlineAsm
  | Instruction _ -> mk_plvar (sort_of_lltype (type_of v)) (value_id v)
  | GlobalVariable -> mk_pgvar (sort_of_lltype (type_of v)) (value_id v)


and expr_of_composite_llvalue v =
  let size = num_operands v in
  let rec expr_of_ops n =
    if n < size then expr_of_llvalue (operand v n)::(expr_of_ops (n+1))
    else [] in
  expr_of_ops 0

let rec pp_llexpr f e = ()

let string_of_llexpr e =
  pp_llexpr Format.str_formatter e;
  Format.flush_str_formatter ()

let polymorphic_update e =
  if Z3.Boolean.is_eq e then mk_2 (Z3.Boolean.mk_eq z3_ctx)
  else if Z3.Boolean.is_distinct e then Z3.Boolean.mk_distinct z3_ctx
  else if is_pointer e then mk_2 mk_pointer
  else if is_array e then mk_3 mk_array
  else if is_exploded_struct e then mk_2 mk_exploded_struct
  else Z3.Expr.update e

let pp_sort f = pp_string f @@ Z3.Sort.to_string

let change_sort_of_vars abs_s conc_s =
  let h = Syntax.ExprHashMap.create 0 in
  let rec f e =
    let app op args =
      let e = Z3.FuncDecl.apply op args in
      let args = List.map f args in
      polymorphic_update e args in
    let var v =
      let name = Z3.FuncDecl.get_name (Z3.Expr.get_func_decl v) in
      if Z3.Sort.equal (Z3.Expr.get_sort v) abs_s
	&& Z3.Symbol.is_string_symbol name then
	(* we create fresh names to avoid clashes, but we want the
	   same fresh name for all occurrences of the same variable,
	   hence the hashmap. *)
	let name = Z3.Symbol.get_string name in
	Z3.Expr.mk_fresh_const z3_ctx name conc_s
      else v in
    try Syntax.ExprHashMap.find h e
    with Not_found ->
      let r = (Syntax.on_var var & Syntax.on_app app) e in
      ExprHashMap.add h e r;
      r
  in f
