(* llvm-op.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * LLVM op list
 *)

structure LLVMOp = struct

  datatype op_code

    (* NUW and NSW stand for “No Unsigned Wrap” and “No Signed Wrap”, and if
       the wrap occurs with those keywords, the result is a poison value.
       There also exists NUW & NSW versions of these operations but we
       haven't included them here here. *)

    = Add              
    | FAdd        
    | Sub 
    | FSub 
    | Mul 
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

    (* casts *)
    | Trunc 
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


  (* utilities follow *)

  (* (# inputs, hasOutput) *)
  fun arity (x : op_code) : (int * bool) = (case x
    (* binary ops *)
    of Add      => (2, true)
    | FAdd      => (2, true)
    | Sub       => (2, true)
    | FSub      => (2, true)
    | Mul       => (2, true)
    | FMul      => (2, true)
    | UDiv      => (2, true)
    | SDiv      => (2, true)
    | ExactSDiv => (2, true)
    | FDiv      => (2, true)
    | URem      => (2, true)
    | SRem      => (2, true)
    | FRem      => (2, true)
    | Shl       => (2, true)
    | LShr      => (2, true)
    | AShr      => (2, true)
    | And       => (2, true)
    | Or        => (2, true)
    | Xor       => (2, true)

    (* casts *)
    | Trunc     => (1, true)
    | ZExt      => (1, true)
    | SExt      => (1, true)
    | FPToUI    => (1, true)
    | FPToSI    => (1, true)
    | UIToFP    => (1, true)
    | SIToFP    => (1, true)
    | FPTrunc   => (1, true)
    | FPExt     => (1, true)
    | PtrToInt  => (1, true)
    | IntToPtr  => (1, true)
    | BitCast   => (1, true)
    (* end arity *))


  structure Ty = LLVMTy
  structure AS = LLVMAttribute.Set
  structure V = Vector

  fun typeCheck (x : op_code, inputs : Ty.t vector) : Ty.t option = let
      val (numInput, hasRes) = arity(x)
      val _ = if numInput = V.length inputs 
                then ()
              else raise Fail "bogus number of args"

      fun sameTys () = V.foldl 
    in 
      (* WIP(kavon): this is probably going to be ugly. *)
      raise Fail "need to do this."
      (* SOME(V.sub(inputs, 0)) *)
    end

  fun checkCast (x : op_code, (from : Ty.t, to : Ty.t)) : Ty.t =
    (* FIXME(kavon): we need to actually check something. *)
    to

  fun checkAttrs (x : op_code, attrSet : AS.set) : AS.set =
    (* perform an intersection operation or something? *)
    attrSet

(*
      (* gets the LLVM op name. note that there are no unary ops in LLVM
     so it is incorrect to ask for one of their names! *)
  and bopName (x : Op.bin_op) : string = (case x
    (* binary ops *)
     of Op.Add         => "add" 
      | Op.NSWAdd      => "add nsw"
      | Op.NUWAdd      => "add nuw"
      | Op.FAdd        => "fadd"
      | Op.Sub         => "sub"
      | Op.NSWSub      => "sub nsw"
      | Op.NUWSub      => "sub nuw"
      | Op.FSub        => "fsub"
      | Op.Mul         => "mul"
      | Op.NSWMul      => "mul nsw"
      | Op.NUWMul      => "mul nuw"
      | Op.FMul        => "fmul"
      | Op.UDiv        => "udiv"
      | Op.SDiv        => "sdiv"
      | Op.ExactSDiv   => "sdiv exact"
      | Op.FDiv        => "fdiv"
      | Op.URem        => "urem"
      | Op.SRem        => "srem" 
      | Op.FRem        => "frem" 
      | Op.Shl         => "shl" 
      | Op.LShr        => "lshr" 
      | Op.AShr        => "ashr" 
      | Op.And         => "and"
      | Op.Or          => "or"
      | Op.Xor         => "xor"
      (* esac *))

  and castName (x : Op.cast_op) : string = (case x
    (* binary ops *)
     of Op.Trunc            => "trunc"
      | Op.ZExt             => "zext"
      | Op.SExt             => "sext"
      | Op.FPToUI           => "fptoui"
      | Op.FPToSI           => "fotpsi"
      | Op.UIToFP           => "uitofp"
      | Op.SIToFP           => "sitofp"
      | Op.FPTrunc          => "fptrunc"
      | Op.FPExt            => "fpext"
      | Op.PtrToInt         => "ptrtoint"
      | Op.IntToPtr         => "inttoptr"
      | Op.BitCast          => "bitcast"
      | Op.AddrSpaceCast    => "addrspacecast"
      (* esac *))

*)

end
