(* llvm-op.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * LLVM op list
 *)

structure LLVMOp = struct

  datatype bin_op

    (* NUW and NSW stand for “No Unsigned Wrap” and “No Signed Wrap”, and if
       the wrap occurs with those keywords, the result is a poison value.
       There also exists NUW & NSW versions of these operations but we
       haven't included them here here. *)

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

  (* There aren't any unary ops in LLVM, but we include
     these to make it easier. *)
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

(* TODO
  and atomic_op
    | XChg
*)    

end
