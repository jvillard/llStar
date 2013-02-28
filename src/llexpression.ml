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
let numargs i = Arg_op("numeric_const",[Arg_string i])
let numargs_of_int i = numargs (string_of_int i)
let numargs_of_int64 i = numargs (Int64.to_string i)
let bvargs sz i = Arg_op("bv_const",[Arg_string sz; Arg_string i])
let bvargs_of_int sz i = bvargs (string_of_int sz) (string_of_int i)
let bvargs_of_int64 sz i = bvargs (string_of_int sz) (Int64.to_string i)
let bvargs64_of_int sz i = bvargs (Int64.to_string sz) (string_of_int i)
let bvargs64_of_int64 sz i = bvargs (Int64.to_string sz) (Int64.to_string i)

(* a few functions for creating predicates. Adds a layer of
   type-safety and avoids catastrophic typos *)
let mkPointer ptr ptr_t v = mkSPred ("pointer", [ptr; ptr_t; v])
let mkArray ptr start_idx end_idx size array_t v =
  mkSPred ("array", [ptr; start_idx; end_idx; size; array_t; v])

let args_sizeof target t =
  let size64 = Llvm_target.store_size target t in
  bvargs_of_int64 64 size64

let rec args_of_type t = match (classify_type t) with
  | Void -> Arg_op("void_type",[])
  | Float
  | Half
  | Double
  | X86fp80
  | Fp128
  | Ppc_fp128 -> Arg_op("float_type", [])
  | Label -> Arg_op("label_type", [])
  | Integer -> Arg_op("integer_type", [numargs_of_int (integer_bitwidth t)])
  | TypeKind.Function -> (* silly name clash *)
    let ret_type = return_type t in
    let par_types = param_types t in
    Arg_op("function_type", args_of_type ret_type::(args_of_type_array par_types))
  | Struct -> (
    if is_opaque t then Arg_op("opaque_type", [])
    else match struct_name t with
      | None ->
	let elts_types = struct_element_types t in
	Arg_op("struct_type", args_of_type_array elts_types)
      | Some n ->
	Arg_op("named_type", [Arg_string n])
  )
  | Array ->
    let elt_t = element_type t in
    Arg_op("array_type", [args_of_type elt_t])
  | Pointer ->
    let elt_t = element_type t in
    Arg_op("pointer_type", [args_of_type elt_t])
  | Vector ->
    let elt_t = element_type t in
    Arg_op("vector_type", [args_of_type elt_t])
  | Metadata -> Arg_op("MD_type", [])
and args_of_type_array ta =
  Array.to_list (Array.map args_of_type ta)

let mk_type_ppred argv argt =
  mkPPred ("type", [argv; argt])

let mk_lltype_ppred argv llt =
  mk_type_ppred argv (args_of_type llt)

let args_of_int_const v = match int64_of_const v with
  | Some i ->
    let sz = integer_bitwidth (type_of v) in
    bvargs_of_int64 sz i
  | None -> Arg_var (Vars.freshe ())

let rec args_of_const_expr v = match constexpr_opcode v with
  | Opcode.Add ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_bvadd", [x; y])
  | Opcode.Sub ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_bvsub", [x; y])
  | Opcode.Mul ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_bvmul", [x; y])
  | Opcode.UDiv
  | Opcode.SDiv ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_bvdiv", [x; y])
  | Opcode.URem
  | Opcode.SRem ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_bvrem", [x; y])
  | Opcode.FAdd
  | Opcode.FSub
  | Opcode.FMul
  | Opcode.FDiv
  | Opcode.FRem ->
    (* TODO: This is the only safe thing until floats are supported *)
    Arg_var (Vars.freshe ())    
  (* TODO@CoreStar: translate bitwise operation to z3 *)  
  | Opcode.Shl ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_bvshl", [x; y])
  | Opcode.LShr
  | Opcode.AShr ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_bvshr", [x; y])
  | Opcode.And ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_bvand", [x; y])
  | Opcode.Or ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_bvor", [x; y])
  | Opcode.Xor ->
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op("builtin_bvxor", [x; y])
  (* /TODO CoreStar/z3 *)
  | Opcode.Unwind | Opcode.LandingPad | Opcode.Resume | Opcode.AtomicRMW
  | Opcode.AtomicCmpXchg | Opcode.Fence | Opcode.InsertValue
  | Opcode.ExtractValue | Opcode.ShuffleVector | Opcode.InsertElement
  | Opcode.ExtractElement | Opcode.VAArg | Opcode.UserOp2 | Opcode.UserOp1
  | Opcode.Select | Opcode.Call | Opcode.PHI | Opcode.FCmp | Opcode.ICmp
  | Opcode.BitCast | Opcode.IntToPtr | Opcode.PtrToInt | Opcode.FPExt
  | Opcode.FPTrunc | Opcode.SIToFP | Opcode.UIToFP | Opcode.FPToSI
  | Opcode.FPToUI | Opcode.SExt | Opcode.ZExt | Opcode.Trunc
  | Opcode.GetElementPtr | Opcode.Store | Opcode.Load | Opcode.Alloca
  | Opcode.Unreachable | Opcode.Invalid2 | Opcode.Invoke | Opcode.IndirectBr
  | Opcode.Switch | Opcode.Br | Opcode.Ret | Opcode.Invalid ->
    (* TODO: implement *)
    Arg_var (Vars.freshe ())

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

(** compute the pure predicate corresponding to a getelementpointer instruction *)
let ppred_of_gep x t ptr lidx =
  let rec jump_chain_of_lidx = function
    | [] -> Arg_op ("jump_end", [])
    | idx::tl ->
      let args_idx = args_of_value idx in
      Arg_op ("jump", [args_idx; jump_chain_of_lidx tl]) in
  let jump_chain = jump_chain_of_lidx lidx in
  mkPPred ("eltptr", [x; args_of_type t; ptr; jump_chain])
