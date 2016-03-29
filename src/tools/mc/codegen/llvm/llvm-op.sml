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

    (* general arithmetic *)
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
    
    (* memory operations *)
    | Load
    | Store (* NOTE LLVM does <val> <ptr>, but in this interface we do <ptr> <val> *)
    
    (* these operations use the SequentiallyConsistent memory ordering semantics
      defined by the LLVM IR here: 
      http://llvm.org/docs/Atomics.html#sequentiallyconsistent
      
      If this turns out to be the wrong approach then we'll add another consistency
      arg to these dcons.
     *)
    | Armw of phi (* atomic read-modify-write, implements fetch-and-phi primitive *)
    | CmpXchg  (* takes (addr, cmp, new) and returns a { cmp, i1 } *)

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

    | Icmp of icmp_kind
    | Fcmp of fcmp_kind
    
    and phi
    = P_Add
    (* while there are a lot of other atomic update options,
       CFG currently only needs support for Add. *)
    

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

  (* (# inputs *)
  fun arity (x : op_code) = (case x
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
       | Fcmp _
       | Store
       | Armw _ ) => 2

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
      | Load )   => 1
      
    | CmpXchg => 3
    (* end arity *))

  structure Ty = LLVMTy
  structure LT = LLVMType
  structure A = LLVMAttribute
  structure AS = LLVMAttribute.Set
  
  structure V = Vector
  structure L = List
  structure S = String

  fun err f expected observed = 
    raise Fail ("(llvm-backend) " ^ f ^ ": incompatible types,\n " 
        ^ "\texpected: " ^ expected ^ "\n"
        ^ "\tobserved: " ^ observed ^ "\n")


  (* the design of this was meant to allow for easy extension
     for new opcodes... it should be as simple as adding new, simple cases here
     and there and that's it. 
     
     NOTE NOTE NOTE typeCheck _needs_ to be filled in because
     it tells the builder, specifically LB.mk, what the result type of an instruction is!! 
     *)
  fun typeCheck (x : op_code, inputs : Ty.t vector) : Ty.t option = let

      (* TODO(kavon): the bool from arity is ignored because
                      it's implicity known when writing the case here. 
                      maybe remove it? *)

      val numInput = arity x
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
         
      
      
      | Store => let 
        (* store has no result but we check the types *)
        val left = V.sub(inputs, 0) (* addr *)
        val right = V.sub(inputs, 1) (* val to store *)
        
        val _ = if not (isPtr left)
                then err "store" "left arg to be a pointer" (LT.nameOf left)
                else ()
                
        val _ = if not (LT.same(LT.deref left, right))
                then err "store" (LT.nameOf(LT.mkPtr right)) (LT.nameOf left)
                else ()
        in
            NONE
        end
        
      | Load => let
        val arg = V.sub(inputs, 0)
        val _ = if not (isPtr arg)
                then err "load" "pointer" (LT.nameOf arg)
                else ()
        in
            SOME (LT.deref arg)
        end
        
      | Armw _ => let
          val left = V.sub(inputs, 0) (* addr *)
          val right = V.sub(inputs, 1) (* val to store *)
          
          val _ = if isPtr left andalso isInt(LT.deref left)
                  then ()
                  else err "atomicrmw" "left arg to be an int pointer" (LT.nameOf left)
                  
          val _ = if not (isInt right)
                  then err "atomicrmw" "right arg to be an int" (LT.nameOf left)
                  else ()
                  
          val _ = if not (LT.same(LT.deref left, right))
                  then err "atomicrmw" (LT.nameOf(LT.mkPtr right)) (LT.nameOf left)
                  else ()
          in
              SOME(LT.deref left)
          end
      
      | CmpXchg => let
            val ptr = V.sub(inputs, 0) (* addr *)
            val cmp = V.sub(inputs, 1) (* val to compare *)
            val new = V.sub(inputs, 2) (* val to replace with *)
            
            val derefPtr = LT.deref ptr
            
            fun intOrPtr t =                    (* NOTE this assumes vprocTy is a pointer *)
                if isPtr t orelse isInt t orelse (LT.same(LT.vprocTy, t))
                    then ()
                    else err "cmpxchg" "int or ptr to compare & exchange" (LT.nameOf t)
            
            val _ = (intOrPtr cmp ; intOrPtr new)
                    
            val _ = if LT.same(cmp, new)
                    then ()
                    else err "cmpxchg" 
                        "comparison and replacement to be the same" 
                        ((LT.nameOf new) ^ " vs " ^ (LT.nameOf cmp))
                        
            val _ = if isPtr ptr then () else err "cmpxchg" "first arg to be ptr" (LT.nameOf ptr)
            
            val _ = if LT.same(cmp, LT.deref ptr)
                    then ()
                    else err "cmpxchg" 
                    "ptr to ty to be the same as replaced ty" 
                    ((LT.nameOf ptr) ^ " and " ^ (LT.nameOf cmp))
                    
            val tyOfXchg = LT.mkUStruct [cmp, LT.boolTy]
            
            (* NOTE cmpxchg actually returns a { cmp, i1 } type, not just cmp
               
               here's an example
               
               %val_success = cmpxchg i32* %ptr, i32 %cmp, i32 %squared acq_rel monotonic ; yields  { i32, i1 }
              %value_loaded = extractvalue { i32, i1 } %val_success, 0
              %success = extractvalue { i32, i1 } %val_success, 1
              br i1 %success, label %done, label %loop
               
               so CAS and BCAS need to add the extractvalue instruction afterwards 
               
               *)
            
          in
            SOME tyOfXchg
          end
        
    (* do not be tempted to put a wildcard here, make sure
       the type returned, if any, is the one this operation outputs. *)
        
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
    
    and isPtr (t : Ty.t) = (case LT.node t
        of Ty.T_Ptr _ => true
         | _ => false
        (* esac *))
        
    and isInt (t : Ty.t) = (case LT.node t
        of Ty.T_Int _ => true
         | _ => false
        (* esac *))
    
    
    (* automatically determine which cast would be appropriate given the two types.
       this is mostly a bandaid for the fact that llvm doesn't allow bitcasts to/from
       pointers.  FIXME this doesn't handle vectors *)
  and autoCaster signed = fn (from : Ty.t, to : Ty.t) => (case (LT.node from, LT.node to)
    of (Ty.T_Ptr _, Ty.T_Int _) => PtrToInt
     | (Ty.T_Int _, Ty.T_Ptr _) => IntToPtr
     | (Ty.T_Int fromW, Ty.T_Int toW) => (case Int.compare(LT.tnc toW, LT.tnc fromW)
        (* I want to make the int's width... *)
        of GREATER => if signed then SExt else ZExt
         | LESS => Trunc
         | EQUAL => BitCast (* a silly cast *)
         (* esac *))
         
     | (Ty.T_Float, Ty.T_Double) => FPExt
     | (Ty.T_Double, Ty.T_Float) => FPTrunc
     
     | ( (Ty.T_Float, Ty.T_Int _)
       | (Ty.T_Double, Ty.T_Int _)) => if signed then FPToSI else FPToUI
       
     | ( (Ty.T_Int _, Ty.T_Float)
       | (Ty.T_Int _, Ty.T_Double)) => if signed then SIToFP else UIToFP
     
     | _ => BitCast
    (* esac *))
    
    
  and autoCast x = autoCaster true x
  
  (* a cast for objects which are logically unsigned integers, such as addresses. *)
  and simpleCast (from : Ty.t, to : Ty.t) : op_code = 
      (case (LT.node from, LT.node to)
        of (Ty.T_Ptr _, Ty.T_Int _) => PtrToInt
         | (Ty.T_Int _, Ty.T_Ptr _) => IntToPtr
         | (Ty.T_Int fromW, Ty.T_Int toW) => (case Int.compare(LT.tnc toW, LT.tnc fromW)
            (* I want to make the int's width... *)
            of GREATER => ZExt
             | LESS => Trunc
             | EQUAL => BitCast (* a silly cast *)
             (* esac *))
         | _ => BitCast
        (* esac *))
        
  (* a cast that is non destructive or changes information. logically it's like ORing
     the information contained in the value upon the "empty value" of the destination
     type. *)
  and safeCast (from : Ty.t, to : Ty.t) : op_code = 
      (case (LT.node from, LT.node to)
        of (Ty.T_Ptr _, Ty.T_Int _) => PtrToInt
         | (Ty.T_Int _, Ty.T_Ptr _) => IntToPtr
         | (Ty.T_Int fromW, Ty.T_Int toW) => (case Int.compare(LT.tnc toW, LT.tnc fromW)
            (* I want to make the int's width... *)
            of GREATER => ZExt
             | LESS => raise Fail "not a safe cast"
             | EQUAL => BitCast (* a silly cast *)
             (* esac *))
         | _ => BitCast (* better hope the width is the same *)
        (* esac *))
  

    (* FIXME doesn't check many things right now. should add FPtoUI/SI etc 
       TODO should check that sext, zext trunc etc have a rhs whose
       width is smaller than the lhs. you can't zext i64 to i64, it's 
       rejected as a type error. *)
  and checkCast (x : op_code, (from : Ty.t, to : Ty.t)) : Ty.t = (case x
      of 
      (*PtrToInt => if (isPtr from) andalso (isInt to) then to else castErr "ptrtoint"
       | IntToPtr => if (isInt from) andalso (isPtr to) then to else castErr "inttoptr"
       
       | BitCast => if ((isPtr from) andalso (isInt to)) 
                        orelse
                        ((isInt from) andalso (isPtr to)) 
                    then
                        castErr "can't bitcast between pointer & int. use inttoptr or ptrtoint"
                    else to*)
            
        _ => to (* FIXME(kavon): it would be wise to add size checks for bitcasts
                   and other checking goodies maybe? also we need to add that vproc is a pointer *)
      (* esac *))
      
  and castErr s = raise Fail ("(llvm-backend) casting type error: " ^ s)



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
    of (SDiv | UDiv) => AS.addList(AS.empty, [A.ExactDiv])
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
        
     | (Load | Store) => AS.addList(AS.empty, 
         [A.Atomic, A.Volatile, A.Aligned 0])
         
     | (Armw _ | CmpXchg) => AS.addList(AS.empty, [A.Volatile])

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
      
      | Load        => "load"
      | Store       => "store"
      
      | Armw _      => "atomicrmw"
      | CmpXchg     => "cmpxchg"
      
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

      | Icmp _ => "icmp"
      | Fcmp _ => "fcmp"
    (* esac *))


  and phiKindToStr (p : phi) = (case p
      of P_Add => "add"
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
