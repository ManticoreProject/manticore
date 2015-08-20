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
  structure LT = LLVMType
  structure AS = LLVMAttribute.Set
  structure V = Vector


  (* the design of this was meant to allow for easy extension
     for new opcodes... it should be as simple as adding new, simple cases here
     and there and that's it. *)
  fun typeCheck (x : op_code, inputs : Ty.t vector) : Ty.t option = let

      (* TODO(kavon): the bool from arity is ignored because
                      it's implicity known when writing the case here. 
                      maybe remove it? *)

      val (numInput, _) = arity(x)
      val _ = if numInput = V.length inputs 
                then ()
              else raise Fail "bogus number of args"

      (* allows one to specify a chk which changes based upon the argument number. *)
      fun checkTys chk inputs = 
        V.foldli (fn (i, t, b) => b andalso chk(i, t)) true inputs

      (* all arguments must be of the same type, and the arguments can only
         be a certian kind. We ensure one argument matches the kind requirement,
         then we ensure the rest are the same as that one. *)
      fun sameKinds chk inputs = let
          val first = V.sub(inputs, 0)
        in
          if chk first
            then 
              if checkTys (fn (_, t) => LT.same(first, t)) inputs
                then first
                else 
                  raise Fail "not all types are the same"
          else 
            raise Fail "invalid type kind"
        end

    in 
      case x
        (* binary ops *)
        of Add      => SOME(sameKinds intOrVecOfInt inputs)
        | Sub       => SOME(sameKinds intOrVecOfInt inputs)
        | Mul       => SOME(sameKinds intOrVecOfInt inputs)
        | UDiv      => SOME(sameKinds intOrVecOfInt inputs)
        | SDiv      => SOME(sameKinds intOrVecOfInt inputs)
        | ExactSDiv => SOME(sameKinds intOrVecOfInt inputs)
        | URem      => SOME(sameKinds intOrVecOfInt inputs)
        | SRem      => SOME(sameKinds intOrVecOfInt inputs)
        | Shl       => SOME(sameKinds intOrVecOfInt inputs)
        | LShr      => SOME(sameKinds intOrVecOfInt inputs)
        | AShr      => SOME(sameKinds intOrVecOfInt inputs)
        | And       => SOME(sameKinds intOrVecOfInt inputs)
        | Or        => SOME(sameKinds intOrVecOfInt inputs)
        | Xor       => SOME(sameKinds intOrVecOfInt inputs)

        | FAdd      => SOME(sameKinds realOrVecOfReal inputs)
        | FSub      => SOME(sameKinds realOrVecOfReal inputs)
        | FMul      => SOME(sameKinds realOrVecOfReal inputs)
        | FDiv      => SOME(sameKinds realOrVecOfReal inputs)
        | FRem      => SOME(sameKinds realOrVecOfReal inputs)
    end

  and intOrVecOfInt (t : Ty.t) = (case LT.node t
    of Ty.T_Int _ => true
     | Ty.T_Vector (_, t) => (case LT.node t
        of Ty.T_Int _ => true
         | _ => false
        (* esac *))
     | _ => false 
    (* esac *))

  (* real here means double or float *)
  and realOrVecOfReal (t : Ty.t) = (case LT.node t
    of Ty.T_Float => true
     | Ty.T_Double => true
     | Ty.T_Vector (_, t) => (case LT.node t
        of Ty.T_Float => true
         | Ty.T_Double => true
         | _ => false
        (* esac *))
     | _ => false 
    (* esac *))



  fun checkCast (x : op_code, (from : Ty.t, to : Ty.t)) : Ty.t =
    (* FIXME(kavon): we need to actually check something. *)
    to



  fun checkAttrs (x : op_code, attrSet : AS.set) : AS.set =
    (* perform an intersection operation or something? *)
    attrSet

  

  (* gets the LLVM op name *)
  fun nameOf (x : op_code) : string = (case x
    (* binary ops *)
     of Add         => "add" 
      | FAdd        => "fadd"
      | Sub         => "sub"
      | FSub        => "fsub"
      | Mul         => "mul"
      | FMul        => "fmul"
      | UDiv        => "udiv"
      | SDiv        => "sdiv"
      | ExactSDiv   => "sdiv exact" (* TODO: consider making Exact an attribute. *)
      | FDiv        => "fdiv"
      | URem        => "urem"
      | SRem        => "srem" 
      | FRem        => "frem" 
      | Shl         => "shl" 
      | LShr        => "lshr" 
      | AShr        => "ashr" 
      | And         => "and"
      | Or          => "or"
      | Xor         => "xor"
      
      | Trunc       => "trunc"
      | ZExt        => "zext"
      | SExt        => "sext"
      | FPToUI      => "fptoui"
      | FPToSI      => "fotpsi"
      | UIToFP      => "uitofp"
      | SIToFP      => "sitofp"
      | FPTrunc     => "fptrunc"
      | FPExt       => "fpext"
      | PtrToInt    => "ptrtoint"
      | IntToPtr    => "inttoptr"
      | BitCast     => "bitcast"
     (* esac *))

end
