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
    
    (* not a real LLVM instruction, but used for
       assigning rhs forms to lhs when its allowed (primarily: constants) *)
    | Nop

    | Icmp of icmp_kind
    | Fcmp of fcmp_kind

    and icmp_kind
      = S of cmp (* signed *)
      | US of cmp (* unsigned *)

    and fcmp_kind
      = O of cmp (* ordered *)
      | UO of cmp (* unordered *)
      | TRUE
      | FALSE
      | ORD
      | UNO

    and cmp
      = EQ
      | NE
      | GT
      | GE
      | LT
      | LE 
      

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
       | Xor
       | Icmp _
       | Fcmp _ ) => (2, true)

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
      | BitCast
      | Nop )   => (1, true)
    (* end arity *))


  structure Ty = LLVMTy
  structure LT = LLVMType
  structure A = LLVMAttribute
  structure AS = LLVMAttribute.Set
  structure V = Vector
  structure L = List
  structure S = String


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

      | Icmp _ => let
          (* gives us the type of the two operands *)
          val ty = sameKinds (fn x => (intOrVecOfInt x) orelse (ptrOrVecOfPtr x)) inputs
          val i1Ty = LT.mkInt(LT.cnt 1)
        in
          SOME( case vecSize ty
                  of NONE => i1Ty
                   | SOME i => LT.mkVector(i, i1Ty) )
        end

      | Fcmp _ => let
          (* gives us the type of the two operands *)
          val ty = sameKinds realOrVecOfReal inputs
          val i1Ty = LT.mkInt(LT.cnt 1)
        in
          SOME( case vecSize ty
                  of NONE => i1Ty
                   | SOME i => LT.mkVector(i, i1Ty) )
        end

      |  ( FAdd
         | FSub
         | FMul
         | FDiv
         | FRem ) => SOME(sameKinds realOrVecOfReal inputs)
      
      | Nop => SOME(sameKinds (fn _ => true) inputs)
    end

  and vecSize (t : Ty.t) = (case LT.node t
    of Ty.T_Vector (i, _) => SOME i
     | _ => NONE
    (* esac *))

  and ptrOrVecOfPtr (t : Ty.t) = (case LT.node t
    of Ty.T_Ptr _ => true
     | Ty.T_Vector (_, t) => (case LT.node t
        of Ty.T_Ptr _ => true
         | _ => false
        (* esac *))
     | _ => false 
    (* esac *))

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
    
    
    (* NOTE autoCast will _not_ produce the following opcodes because it assumes all integers
        are signed, since "rawTyToCTy" in heap-transfer-fn.sml assumes that too.
      | ZExt    
      | FPToUI  
      | UIToFP  
      *)

    (* automatically determine which cast would be appropriate given the two types.
       this is mostly a bandaid for the fact that llvm doesn't allow bitcasts to/from
       pointers.  FIXME this doesn't handle vectors *)
  fun autoCast (from : Ty.t, to : Ty.t) : op_code = (case (LT.node from, LT.node to)
    of (Ty.T_Ptr _, Ty.T_Int _) => PtrToInt
     | (Ty.T_Int _, Ty.T_Ptr _) => IntToPtr
     | (Ty.T_Int fromW, Ty.T_Int toW) => (case Int.compare(LT.tnc toW, LT.tnc fromW)
        (* I want to make the int's width... *)
        of GREATER => SExt        
         | LESS => Trunc
         | EQUAL => BitCast (* a silly cast *)
         (* esac *))
         
     | (Ty.T_Float, Ty.T_Double) => FPExt
     | (Ty.T_Double, Ty.T_Float) => FPTrunc
     
     | ( (Ty.T_Float, Ty.T_Int _)
       | (Ty.T_Double, Ty.T_Int _)) => FPToSI
       
     | ( (Ty.T_Int _, Ty.T_Float)
       | (Ty.T_Int _, Ty.T_Double)) => SIToFP
     
     | _ => BitCast
    (* esac *))
    
  

    (* FIXME doesn't check many things right now. should add FPtoUI/SI etc *)
  and checkCast (x : op_code, (from : Ty.t, to : Ty.t)) : Ty.t = (case x
      of PtrToInt => if (isPtr from) andalso (isInt to) then to else err "ptrtoint"
       | IntToPtr => if (isInt from) andalso (isPtr to) then to else err "inttoptr"
       
       | BitCast => if ((isPtr from) andalso (isInt to)) 
                        orelse
                        ((isInt from) andalso (isPtr to)) 
                    then
                        err "can't bitcast between pointer & int. use inttoptr or ptrtoint"
                    else to
            
       | _ => to (* FIXME(kavon): it would be wise to add size checks for bitcasts
                   and other checking goodies maybe? *)
      (* esac *))
      
  and isPtr (t : Ty.t) = (case LT.node t
      of Ty.T_Ptr _ => true
       | _ => false
      (* esac *))
      
  and isInt (t : Ty.t) = (case LT.node t
      of Ty.T_Int _ => true
       | _ => false
      (* esac *))
      
  and err s = raise Fail ("(llvm-backend) casting type error: " ^ s)



  fun checkAttrs (x : op_code, attrSet : AS.set) : AS.set = let
      val invalidAttrs = AS.difference(attrSet, validAttrs(x))
    in
      if AS.isEmpty(invalidAttrs)
        then attrSet
      else raise Fail 
        (S.concat 
          ["\nthe following attributes are invalid for opcode '", toString(x), "'\n\t",
           S.concatWith " " (L.map A.toString (AS.listItems invalidAttrs))])
    end
    

  and validAttrs (x : op_code) : AS.set = (case x
    of SDiv => AS.addList(AS.empty, [A.ExactDiv])
     | ( FAdd
        | FSub
        | FMul
        | FDiv
        | FRem
        | Fcmp _
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
  and toString (x : op_code) : string = (case x
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
      
      (* NOTE not a real op though *)
      | Nop         => "nop"

      | Icmp _ => "icmp"
      | Fcmp _ => "fcmp"
    (* esac *))


  and icmpKindToStr (kind : icmp_kind) = (case kind
        of ( S(EQ)
            | US(EQ) ) => "eq"
         | ( S(NE)
            | US(NE) ) => "ne"
         | S c => "s" ^ cmpToStr(c)
         | US c => "u" ^ cmpToStr(c)
        (* esac *))

  and fcmpKindToStr (kind : fcmp_kind) = (case kind
        of O c => "o" ^ cmpToStr(c)
         | UO c => "u" ^ cmpToStr(c)
         | ORD => "ord"
         | UNO => "uno"
         | TRUE => "true"
         | FALSE => "false"
        (* esac *))

  and cmpToStr (c : cmp) = (case c
     of EQ => "eq"
      | NE => "ne"
      | GT => "gt"
      | GE => "ge"
      | LT => "lt"
      | LE => "le"
    (* esac *))

end
