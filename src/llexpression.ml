(*** Llexpression: turn llvm types and values into CoreStar expressions (args) *)

(* LLVM modules *)
open Llvm
open Llvm_target
open TypeKind
open ValueKind
(* coreStar modules *)
open Psyntax
(* LStar modules *)
open Llutils

(** return value expression constant *)
let ret_arg = Arg_var(Spec.ret_v1)

(* shorthands for integer expressions *)
let numargs i = Arg_op("numeric_const",[i])
let numargs_of_str i = numargs (Arg_string i)
let numargs_of_int i = numargs_of_str (string_of_int i)
let numargs_of_int64 i = numargs_of_str (Int64.to_string i)
let bvargs64_of_int64 sz i =
  let args_sz = Arg_string (Int64.to_string sz) in
  let args_abv = Arg_string (Int64.to_string (Int64.abs i)) in
  let ubvargs = Arg_op("bv_const", [args_sz; args_abv]) in
  if Int64.compare i Int64.zero < 0 then
    Arg_op("bvneg."^(Int64.to_string sz), [ubvargs])
  else ubvargs
let bvargs_of_int sz i = bvargs64_of_int64 (Int64.of_int sz) (Int64.of_int i)
let bvargs_of_int64 sz i = bvargs64_of_int64 (Int64.of_int sz) i
let bvargs64_of_int sz i = bvargs64_of_int64 sz (Int64.of_int i)

(* a few functions for creating predicates. Adds a layer of
   type-safety and avoids catastrophic typos *)
let mkUndef sz = Arg_var (Vars.freshe ())
let mkUndef64 sz = mkUndef (Arg_string (Int64.to_string sz))
let mkPointer ptr ptr_t v = mkSPred ("pointer", [ptr; ptr_t; v])
(** padding with [size] bytes at address [x] *)
let mkPadding x size = mkSPred ("pad", [x; size])
let mkEltptr ptr t jchain = Arg_op ("eltptr", [ptr; t; jchain])

let mkIntegerType sz = Arg_op("integer_type", [Arg_string (string_of_int sz)])
let mkFloatType = Arg_op("float_type", [])
let mkVoidType = Arg_op("void_type", [])
let mkLabelType = Arg_op("label_type", [])
let mkNamedType name = Arg_op("named_type", [name])
let mkStructType elts_t = Arg_op("struct_type", elts_t)
let mkFunctionType args_t ret_t = Arg_op("function_type", ret_t::args_t)
let mkPointerType elt_t = Arg_op("pointer_type", [elt_t])
let mkVectorType elt_t = Arg_op("pointer_type", [elt_t])
let mkArrayType size elts_t = Arg_op("array_type", [size; elts_t])
let mkMDType = Arg_op("MD_type", [])

let mkI8Type = Arg_op("integer_type", [Arg_string "8"])
let mkI32Type = Arg_op("integer_type", [Arg_string "32"])
let mkI64Type = Arg_op("integer_type", [Arg_string "64"])
let mkVoidPointerType elt_t = mkPointerType mkI8Type

(* [v] of type [t1] is symbolically converted to a value of type [t2] *)
let mkValConversion t1 t2 v = Arg_op ("cast_value", [t1; t2; v])

let args_sizeof t =
  let size64 = Llvm_target.abi_size !lltarget t in
  bvargs_of_int64 64 size64

let rec args_of_type t = match (classify_type t) with
  | Void -> mkVoidType
  | Float
  | Half
  | Double
  | X86fp80
  | Fp128
  | Ppc_fp128 -> mkFloatType
  | Label -> mkLabelType
  | Integer -> mkIntegerType (integer_bitwidth t)
  | TypeKind.Function -> (* silly name clash *)
    let ret_type = return_type t in
    let par_types = param_types t in
    mkFunctionType (args_of_type_array par_types) (args_of_type ret_type)
  | Struct -> (
    if is_opaque t then Arg_op("opaque_type", [])
    else match struct_name t with
      | None ->
	let elts_types = struct_element_types t in
	mkStructType (args_of_type_array elts_types)
      | Some n -> mkNamedType (Arg_string n)
  )
  | Array ->
    let elt_t = element_type t in
    mkArrayType (bvargs_of_int 64 (array_length t)) (args_of_type elt_t)
  | Pointer ->
    let elt_t = element_type t in
    mkPointerType (args_of_type elt_t)
  | Vector ->
    let elt_t = element_type t in
    mkVectorType (args_of_type elt_t)
  | Metadata -> mkMDType
and args_of_type_array ta =
  Array.to_list (Array.map args_of_type ta)

let args_of_int_const v = match int64_of_const v with
  | Some i ->
    let sz = integer_bitwidth (type_of v) in
    bvargs_of_int64 sz i
  | None -> Arg_var (Vars.freshe ())

(** builds binary operation from constant expression or instruction *)
let rec args_of_op opcode opval =
  let args_of_binop bopname =
    let x = args_of_value (operand opval 0) in
    let y = args_of_value (operand opval 1) in
    Arg_op(bopname, [x; y]) in
  let typed_name_of_bvop op =
    let t = type_of opval in
    let sz = Llvm_target.size_in_bits !lltarget t in
    Printf.sprintf "%s.%Ld" op sz in
  match opcode with
  (* Standard Binary Operators *)
  | Opcode.Add -> args_of_binop (typed_name_of_bvop "bvadd")
  | Opcode.Sub -> args_of_binop (typed_name_of_bvop "bvsub")
  | Opcode.Mul -> args_of_binop (typed_name_of_bvop "bvmul")
  | Opcode.UDiv -> args_of_binop (typed_name_of_bvop "bvudiv")
  | Opcode.SDiv -> args_of_binop (typed_name_of_bvop "bvsdiv")
  | Opcode.URem -> args_of_binop (typed_name_of_bvop "bvurem")
  | Opcode.SRem -> args_of_binop (typed_name_of_bvop "bvsrem")
  (* Logical Operators *)
  | Opcode.Shl -> args_of_binop (typed_name_of_bvop "bvshl")
  | Opcode.LShr -> args_of_binop (typed_name_of_bvop "bvlshr")
  | Opcode.AShr -> args_of_binop (typed_name_of_bvop "bvashr")
  | Opcode.And -> args_of_binop (typed_name_of_bvop "bvand")
  | Opcode.Or -> args_of_binop (typed_name_of_bvop "bvor")
  | Opcode.Xor -> args_of_binop (typed_name_of_bvop "bvxor")
  (* Conversions Operators *)
  | Opcode.BitCast
  | Opcode.Trunc
  | Opcode.ZExt
  | Opcode.SExt
  | Opcode.PtrToInt
  | Opcode.IntToPtr ->
    let value = operand opval 0 in
    let v = args_of_value value in
    let from_sz = Llvm_target.size_in_bits !lltarget (type_of value) in
    let to_sz = Llvm_target.size_in_bits !lltarget (type_of opval) in
    if opcode= Opcode.ZExt ||
      ((opcode= Opcode.PtrToInt || opcode= Opcode.IntToPtr)
       && Int64.compare from_sz to_sz < 0) then
      let zeroes = Int64.sub to_sz from_sz in
      Arg_op(Printf.sprintf "concat.%Ld.%Ld" zeroes from_sz,
	     [bvargs64_of_int zeroes 0;v])
    else if opcode= Opcode.BitCast ||
	   ((opcode= Opcode.PtrToInt || opcode= Opcode.IntToPtr)
	    && Int64.compare from_sz to_sz = 0) then
      v
    else if opcode= Opcode.SExt then
      let signs = Int64.sub to_sz from_sz in
      Arg_op(Printf.sprintf "sign_extend.%Ld" from_sz,
	     [numargs_of_int64 signs;v])
    else if opcode= Opcode.Trunc ||
	   ((opcode= Opcode.PtrToInt || opcode= Opcode.IntToPtr)
	    && Int64.compare from_sz to_sz > 0) then
      Arg_op(Printf.sprintf "extract.%Ld" from_sz,
	     [numargs_of_int64 (Int64.sub to_sz Int64.one);
	      numargs_of_int 0;v])
    else (* all cases accounted for, unreachable *) assert(false)
  (* Other Operators *)
  | Opcode.GetElementPtr ->
    let value = operand opval 0 in
    (* jump indices are in operands 1 to (num_operands opval) of opval *)
    let max_op = num_operands opval in
    let rec jlist_from_op i =
      if i = max_op then []
      else operand opval i::(jlist_from_op (i+1)) in
    let rec jump_chain_of_lidx = function
      | [] -> Arg_op ("jump_end", [])
      | idx::tl ->
	(* convert constant integers to 64 bits *)
	let idx = match int64_of_const idx with
	  | None -> idx
	  | Some i -> const_of_int64 (integer_type !llcontext 64) i false in
	let args_idx = args_of_value idx in
	Arg_op ("jump", [args_idx; jump_chain_of_lidx tl]) in
    let jump_chain = jump_chain_of_lidx (jlist_from_op 1) in
    mkEltptr (args_of_value value) (args_of_type (type_of value)) jump_chain
  | Opcode.ICmp ->
    let op = match icmp_predicate opval with
      | None -> assert false
      | Some p -> match p with
	  | Icmp.Eq -> "builtin_eq"
	  | Icmp.Ne -> "builtin_neq"
	  | Icmp.Ugt -> "bvugt"
	  | Icmp.Sgt -> "bvsgt"
	  | Icmp.Uge -> "bvuge"
	  | Icmp.Sge -> "bvsge"
	  | Icmp.Ult -> "bvult"
	  | Icmp.Slt -> "bvslt"
	  | Icmp.Ule -> "bvule"
	  | Icmp.Sle -> "bvsle" in
    let v1 = args_of_value (operand opval 0) in
    let v2 = args_of_value (operand opval 1) in
    Arg_op(op, [v1; v2])
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
  | Opcode.FCmp
  | Opcode.Select
  | Opcode.ExtractElement
  | Opcode.InsertElement
  | Opcode.ShuffleVector
  | Opcode.ExtractValue
  | Opcode.InsertValue ->
    (* TODO: implement *)
    Arg_var (Vars.freshe ())
  | Opcode.Unwind | Opcode.LandingPad | Opcode.Resume | Opcode.AtomicRMW
  | Opcode.AtomicCmpXchg | Opcode.Fence | Opcode.VAArg | Opcode.UserOp2
  | Opcode.UserOp1 | Opcode.Call | Opcode.PHI | Opcode.Store | Opcode.Load
  | Opcode.Alloca | Opcode.Unreachable | Opcode.Invalid2 | Opcode.Invoke
  | Opcode.IndirectBr | Opcode.Switch | Opcode.Br | Opcode.Ret
  | Opcode.Invalid ->
    (* these opcodes are not allowed in constant expressions according
       to LLVM language reference *)
    dump_value opval;
    failwith "Unexpected operation in expression."

and args_of_const_expr v = args_of_op (constexpr_opcode v) v

and args_of_value v = match classify_value v with
  | NullValue -> implement_this "NullValue"
  | Argument -> Arg_var (Vars.concretep_str (value_id v))
  | BasicBlock -> failwith "Invalid bitcode? Unexpected BasickBlock value"
  | InlineAsm -> Arg_var (Vars.freshe ())
  | MDNode -> raise (MetaData v)
  | MDString -> raise (MetaData v)
  | BlockAddress -> Arg_op("block_addr", [args_of_value (operand v 0)])
  | ConstantAggregateZero -> implement_this "ConstantAggregateZero"
  | ConstantArray -> args_of_composite_value "array" v
  | ConstantExpr -> args_of_const_expr v
  | ConstantFP -> Arg_var (Vars.freshe ())
  | ConstantInt -> args_of_int_const v
  | ConstantPointerNull -> bvargs64_of_int (size_in_bits !lltarget (type_of v)) 0
  | ConstantStruct -> args_of_composite_value "struct" v
  | ConstantVector -> args_of_composite_value "vector" v
  | Function -> Arg_op("function", [args_of_value (operand v 0)])
  | GlobalAlias -> implement_this "value is a global alias" (* undocumented? *)
  | GlobalVariable -> Arg_var (Vars.concretep_str (value_id v))
  | UndefValue -> Arg_op("undef", [])
  | Instruction op -> Arg_var (Vars.concretep_str (value_id v))

and args_of_composite_value aggr v =
  let size = num_operands v in
  let rec args_of_ops n =
    if n < size then args_of_value (operand v n)::(args_of_ops (n+1))
    else [] in
  Arg_op(aggr, args_of_ops 0)
