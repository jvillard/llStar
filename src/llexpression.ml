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
let mkArray ptr start_idx end_idx size elt_sz v =
  mkSPred ("array", [ptr; start_idx; end_idx; size; elt_sz; v])

let args_sizeof t =
  let size64 = Llvm_target.store_size !lltarget t in
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

let args_of_int_const v = match int64_of_const v with
  | Some i ->
    let sz = integer_bitwidth (type_of v) in
    bvargs_of_int64 sz i
  | None -> Arg_var (Vars.freshe ())

let rec args_of_const_expr v =
  let mk_binop bopname =
    let x = args_of_value (operand v 0) in
    let y = args_of_value (operand v 1) in
    Arg_op(bopname, [x; y]) in
  match constexpr_opcode v with
  | Opcode.Add -> mk_binop "bvadd"
  | Opcode.Sub -> mk_binop "bvsub"
  | Opcode.Mul -> mk_binop "bvmul"
  | Opcode.UDiv -> mk_binop "bvudiv"
  | Opcode.SDiv -> mk_binop "bvsdiv"
  | Opcode.URem -> mk_binop "bvurem"
  | Opcode.SRem -> mk_binop "bvsrem"
  | Opcode.FAdd
  | Opcode.FSub
  | Opcode.FMul
  | Opcode.FDiv
  | Opcode.FRem ->
    (* TODO: This is the only safe thing until floats are supported *)
    Arg_var (Vars.freshe ())    
  | Opcode.Shl -> mk_binop "bvshl"
  | Opcode.LShr -> mk_binop "bvlshr"
  | Opcode.AShr -> mk_binop "bvashr"
  | Opcode.And -> mk_binop "bvand"
  | Opcode.Or -> mk_binop "bvor"
  | Opcode.Xor -> mk_binop "bvxor"
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
      (* convert constant integers to 64 bits *)
      let idx = match int64_of_const idx with
	| None -> idx
	| Some i -> const_of_int64 (integer_type !llcontext 64) i false in
      let args_idx = args_of_value idx in
      Arg_op ("jump", [args_idx; jump_chain_of_lidx tl]) in
  let jump_chain = jump_chain_of_lidx lidx in
  mkPPred ("eltptr", [x; args_of_type t; ptr; jump_chain])
