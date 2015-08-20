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
    of ( Add      
       | FAdd      
       | Sub       
       | FSub      
       | Mul       
       | FMul      
       | UDiv      
       | SDiv      
       | FDiv      
       | URem      
       | SRem      
       | FRem      
       | Shl       
       | LShr      
       | AShr      
       | And       
       | Or        
       | Xor ) => (2, true)

    (* casts *)
    | ( Trunc   
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
      | BitCast )   => (1, true)
    (* end arity *))


  structure Ty = LLVMTy
  structure LT = LLVMType
  structure A = LLVMAttribute
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
      of ( Add      
          | Sub       
          | Mul       
          | UDiv      
          | SDiv      
          | URem      
          | SRem      
          | Shl       
          | LShr      
          | AShr      
          | And       
          | Or        
          | Xor ) => SOME(sameKinds intOrVecOfInt inputs)

      |  ( FAdd
         | FSub
         | FMul
         | FDiv
         | FRem ) => SOME(sameKinds realOrVecOfReal inputs)
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
    if AS.isEmpty(AS.difference(attrSet, validAttrs(x)))
      then attrSet
    else raise Fail "invalid attributes specified for an op code"
    

  and validAttrs (x : op_code) : AS.set = (case x
    of SDiv => AS.addList(AS.empty, [A.ExactDiv])
     | ( FAdd
        | FSub
        | FMul
        | FDiv
        | FRem
         ) => AS.addList(AS.empty, 
            [A.NoNaN, A.NoInf, A.NoSZero, A.AllowRecip, A.FastMath])

     (* fast math also applies to Fcmp *)

     | ( Add
        | Sub
        | Mul
        | Shl ) => AS.addList(AS.empty, [A.NSW, A.NUW])

     | _ => AS.empty

    )


  

  (* gets the LLVM op name *)
  fun toString (x : op_code) : string = (case x
    (* binary ops *)
     of Add         => "add" 
      | FAdd        => "fadd"
      | Sub         => "sub"
      | FSub        => "fsub"
      | Mul         => "mul"
      | FMul        => "fmul"
      | UDiv        => "udiv"
      | SDiv        => "sdiv"
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
