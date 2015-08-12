(* llvm-builder.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * LLVM Basic Block builder
 *)

functor LLVMBuilder (structure Spec : TARGET_SPEC) : sig

    type t

    type instr
    type var
    type ty
    type bb
    type constant

    eqtype bin_op
    eqtype cast_op
    eqtype unary_op

    (* TODO: attributes? *)


    (* start a fresh basic block *)
    val new : var -> t

    (* generate textual representation of the BB *)
    val toString : bb -> string

    

    (* Terminators *)

    (* all tail calls are marked 'musttail' and followed by a 'ret void' automatically *)
    val tailCall : t -> (instr * instr vector) -> bb

    val unreachable : t -> bb

    val retVoid : t -> bb

    val ret : t -> instr -> bb

    val br : t -> var -> bb

    val condBr : t -> (instr * var * var) -> bb

    val indirectBr : t -> var -> bb

    

    (* Instruction Builders *)

    (* wrappers for vars and constants *)
    val fromV : var -> instr

    val fromC : constant -> instr


    val uop : t -> unary_op -> instr -> instr

    val bop : t -> bin_op -> (instr * instr) -> instr

    (* getelementptr *)
    val gep : t -> (instr * constant vector) -> instr 

    (* getelementptr inbounds *)
    val gep_ib : t -> (instr * constant vector) -> instr 

    val cast : t -> cast_op -> (instr * ty) -> instr

    (* C calls which return *)
    val call : t -> (instr * instr vector) -> instr

    (* join instruction *)
    val phi : t -> (instr * var) vector -> instr

    
    
  end = struct

  structure LT = LLVMType (structure Spec = Spec)
  structure LV = LLVMVar (structure Spec = Spec)

  type ty = LT.ty
  type var = LV.var

  datatype constant 
   = C_Int of ty * int
   | C_Str of var (* string constants are global vars *)

  datatype bin_op
    = Add 
    | NSWAdd 
    | NUWAdd 
    | FAdd 
    | Sub 
    | NSWSub 
    | NUWSub 
    | FSub 
    | Mul 
    | NSWMul 
    | NUWMul 
    | FMul 
    | UDiv 
    | SDiv 
    | ExactSDiv 
    | FDiv 
    | URem 
    | SRem 
    | FRem 
    | Shl 
    | LShr 
    | AShr 
    | And 
    | Or 
    | Xor

  and unary_op
    = Neg
    | NSWNeg
    | NUWNeg
    | FNeg
    | Not

  and cast_op
    = Trunc 
    | ZExt 
    | SExt 
    | FPToUI 
    | FPToSI 
    | UIToFP 
    | SIToFP 
    | FPTrunc 
    | FPExt 
    | PtrToInt 
    | IntToPtr 
    | BitCast 
    | AddrSpaceCast 
    | ZExtOrBitCast 
    | SExtOrBitCast 
    | TruncOrBitCast 
    | PointerCast 

  datatype res 
    = R_Var of var 
    | R_Const of constant 
    | R_None  (* for instructions which have no result / ignored result *)

  datatype opcode
    = OP_Unary of unary_op
    | OP_Binary of bin_op
    | OP_Cast of cast_op
    | OP_GEP
    | OP_GEP_IB
    | OP_Return
    | OP_Br
    | OP_IndrBr
    | OP_CondBr
    | OP_TailCall
    | OP_Call
    | OP_Unreachable
    | OP_None  (* for wrapped constants and vars *)

  datatype instr = INSTR of {
    result : res,
    kind : opcode,
    args : instr vector
  }

  | PHI of {
    (* not a res because const and none are never allowed *)
    join : var,

          (* val, basic block *)
    preds : (instr * var) vector
  }

  (* type t represents a partially built basic block.
     in particular, we push new instructions onto the block
     as it is used. a block is considered finished if a
     terminator is placed on top, and we then
     reverse the instructions to complete it. *)
  and t = T of { 
    name : var,
    body : instr list ref
  }

  and bb = BB of { 
    name : var,
    body : instr list
  }
  
  

  (******************************************)




  (* new : var -> t *)
  fun new _ = raise Fail "not implemented"


  (* generate texual representation of the BB *)
  (*val toString : bb -> string*)
  fun toString _ = raise Fail "not implemented"

  

  (* Terminators *)

  (* all tail calls are marked 'musttail' and followed by a 'ret void' automatically *)
  (*val tailCall : t -> (instr * instr vector) -> bb*)
  fun tailCall _ = raise Fail "not implemented"

  (*val unreachable : t -> bb*)
  fun unreachable _ = raise Fail "not implemented"

  (*val retVoid : t -> bb*)
  fun retVoid _ = raise Fail "not implemented"

  (*val ret : t -> instr -> bb*)
  fun ret _ = raise Fail "not implemented"

  (*val br : t -> var -> bb*)
  fun br _ = raise Fail "not implemented"

  (*val condBr : t -> (instr * var * var) -> bb*)
  fun condBr _ = raise Fail "not implemented"

  (*val indirectBr : t -> var -> bb*)
  fun indirectBr _ = raise Fail "not implemented"

  

  (* Instruction Builders *)

  (* wrappers for vars and constants *)
  (*val fromV : var -> instr*)
  fun fromV _ = raise Fail "not implemented"

  (*val fromC : constant -> instr*)
  fun fromC _ = raise Fail "not implemented"


  (*val uop : t -> unary_op -> instr -> instr*)
  fun uop _ = raise Fail "not implemented"

  (*val bop : t -> bin_op -> (instr * instr) -> instr*)
  fun bop _ = raise Fail "not implemented"

  (* getelementptr *)
  (*val gep : t -> (instr * constant vector) -> instr *)
  fun gep _ = raise Fail "not implemented"

  (* getelementptr inbounds *)
  (*val gep_ib : t -> (instr * constant vector) -> instr *)
  fun gep_ib _ = raise Fail "not implemented"

  (*val cast : t -> cast_op -> (instr * ty) -> instr*)
  fun cast _ = raise Fail "not implemented"

  (* C calls which return *)
  (*val call : t -> (instr * instr vector) -> instr*)
  fun call _ = raise Fail "not implemented"

  (* join instruction *)
  (*val phi : t -> (instr * var) vector -> instr*)
  fun phi _ = raise Fail "not implemented"

  

     

end
