(* llvm-builder.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * LLVM Basic Block builder
 *)

structure LLVMBuilder : sig

    type t

    type instr
    type var
    type ty
    type bb
    type constant
    type attrs
    type convention

    eqtype op_code


    (*  TODO
        it would be nice to have this fn to add a comment to an instruction
        so that when printing it is placed just before the instruction.
        val comment : str -> instr -> instr
      *)


    (* start a fresh basic block. first one is the name
       of the block and the list is the list of arguments
       to the block *)
    val new : (var * var list) -> t
    
    val labelOf : t -> var
    
    val paramsOf : t -> var list
    
    (* produces a copy of the given BB with a fresh name
       and fresh parameter names (but keeps parameter types the same). *)
    val copy : t -> t
    
    (* same as copy, but you can provide your own name (var should have label ty) *)
    val copy' : var -> t -> t

    (* generate textual representation of the BB *)
    val toString : bb -> string

    

    (* Terminators *)

    (* all tail calls are marked 'musttail' and followed by a 'ret void' automatically,
       so this is not a general tail call, it's for CPS style tail calls, and the jwaCC
       is implicit *)
    val tailCall : t -> (instr * instr vector) -> bb

    val unreachable : t -> bb

    val retVoid : t -> bb

    val ret : t -> instr -> bb

    val br : t -> var -> bb

    val condBr : t -> (instr * var * var) -> bb
    
            (* compared val -> (defaultLabel * (intTag * label) list)  *)
    val switch : t -> instr -> (var * (constant * var) list) -> bb

    (* NOTE(kavon): I don't see myself using indirectbr because we should
                    not need to take block addresses, so I'm not implementing it. *)
    (*val indirectBr : t -> (instr * var vector) -> bb*)

    

    (* Instruction Builders *)

    (* wrappers for vars and constants *)
    val fromV : var -> instr

    val fromC : constant -> instr
    
    val toV : instr -> var
    
    val toTy : instr -> ty

    val intC : (ty * IntegerLit.integer) -> constant
    
    (* equivalent to  fromC(intC(ty, Int.toLarge i))  because this happens frequently. *)
    val iconst : ty -> int -> instr
       
    val floatC : (ty * FloatLit.t) -> constant
    
    val undef : ty -> constant

    (* The do-it-all LLVM basic block builder that you will become friends with.
    
       The way it works conceptually is similar to appending to the basic block, but
       this appending is done using references under the hood so that you can partially apply mk to
       a basic block in progress, and then reuse that same partially applied function over and over
       to keep appending to that one block. It is remnicent of an object-oriented style.
    *)
    val mk : t -> attrs -> op_code -> instr vector -> instr


    (* a note about the different flavors of GEP supported by this interface below:
        "The type of each index argument depends on the type it is indexing into. 
        
        (1) When indexing into a (optionally packed) structure, only i32 integer constants are allowed (when using a vector of indices they must all be the same i32 integer constant). 
        
        (2) When indexing into an array, pointer or vector, integers of any width are allowed, and they are not required to be constant. These integers are treated as signed values where relevant."
        
        So, gep and gep_ib correspond to case (1). calcAddr and calcAddr_ib correspond to case (2).
        
        The reason for this distinction in LLVM is presumably because it is not obvious what the type
        of a GEP is if it involves indexing into a structure without constants, whereas the other types
        can only have one possible type. NOTE We might be missing the following case:
        
        gep [ 32 x structTy ]*  using offsets i32 %dynamicVal, i32 <const val>
        
        GEP is honestly so overloaded and has many different caveats that I don't think it's even
        worth trying to enforce any further correctness within this builder interface. unfortunately we
        need to determine result type info so :/ 
     *)

    val gep : t -> (instr * constant vector) -> instr 

    val gep_ib : t -> (instr * constant vector) -> instr 
    
    val calcAddr : t -> (instr * instr vector) -> instr
    
    val calcAddr_ib : t -> (instr * instr vector) -> instr

    (* NOTE(kavon): something probably not supported right now is GEPs that calculate a
       vector of addresses, extend the interface if desired. *)
       
    (* (struct, #[offset1, offset2, ...]) *)
    val extractV : t -> (instr * constant vector) -> instr
    
    (* (struct, value, #[offset1, offset2, ...]) *)
    val insertV : t -> (instr * instr * constant vector) -> instr 

    val cast : t -> op_code -> (instr * ty) -> instr


    (* NOTE having three different 'call' functions in the sig
        is kind of messy. would be nice to merge them together 
        or something but I don't have time to refactor ~kavon *)

    (* calls which return, without a specific calling convention *)
    val call : t -> (instr * instr vector) -> instr
    
    (* call with a specific convention *)
    val callAs : t -> convention -> (instr * instr vector) -> instr
    
    (* call with a specific convention and attributes *)
    val callAs' : t -> (attrs * convention) -> (instr * instr vector) -> instr

    (* calling convention goodies *)
    val jwaCC : convention  (* the Manticore calling convention *)
    val stdCC : convention  (* standard C calling convention *)
    val fastCC : convention  (* "fast" C calling convention *)
    
    val cctoStr : convention -> string

    (*
      PHI's should be mutable in this interface, so we do not
      expose the ability to construct PHI{...}'s directly.
      Instead, one can update an in-progress block with predecessors, and
      upon finishing a basic block, phi's will be added to the correct position
      in a block (in particular, they must occur at the start of a basic block).

      Use the interface below for joins in order to produce phis.
      
      If all phis are not able to be determined before terminating a basic block,
      its reccomended to produce unterminated blocks plus a thunk that will terminate
      it at a later point.
    *) 
  val addIncoming : t -> (var * instr list) -> t

    
  end = struct

  structure LV = LLVMVar
  structure LT = LV.LT
  structure V = Vector

  structure Ty = LLVMTy
  structure Op = LLVMOp
  structure A = LLVMAttribute
  structure AS = LLVMAttribute.Set

  structure S = String
  structure V = Vector
  structure L = List
    
  structure F64ToBits = FloatToBitsFn (IEEEFloat64Params);
  structure F32ToBits = FloatToBitsFn (IEEEFloat32Params);
  structure W8 = Word8
  structure W8V = Word8Vector

  type ty = Ty.t
  type var = LV.var
  type attrs = AS.set
  type convention = string

  type op_code = Op.op_code


  val noCC  = ""
  val jwaCC  = "cc93"
  val stdCC  = "ccc"
  val fastCC  = "fastcc"
  fun cctoStr x = x
  

  datatype res 
    = R_Var of var 
    | R_Const of constant 
    | R_None  (* for instructions which have no LHS, like terminators *)

    (* Simple Constants in LLVM *)
  and constant 
    = C_Int of ty * IntInf.int
    
    | C_Float of ty * FloatLit.t  
    
    | C_Str of var (* TODO string constants are global vars, why do we need this? *)
    
    | C_Undef of ty  (* for undefined literals of any type except label or void *)
    
                  (* defaultTarg * (tag * targ) *)
  type switch_arms = (var * (constant * var) list)

  datatype opkind
    = OP of op_code
    | OP_GEP
    | OP_GEP_IB
    | OP_ExtractVal
    | OP_InsertVal
    | OP_Return
    | OP_Br
    | OP_CondBr
    | OP_TailCall
    | OP_Call of convention option
    | OP_Unreachable
    | OP_Switch of switch_arms
    | OP_None  (* for wrapped constants and vars, as no operation occurs *)


  datatype instr 
    = INSTR of {
        result : res,
        kind : opkind,
        atr : attrs,
        args : instr vector
      }

    | PHI of {
        join : var,   (* not a res because const and none are never allowed *)      
        preds : (instr * var) vector    (* val, basic block *)
      }

  (* type t represents a partially built basic block.
     in particular, we push new instructions onto the block
     as it is used. a block is considered finished if a
     terminator is placed on top, and we then
     reverse the instructions to complete it. *)
  and t = T of { 
    name : var,
    args : var list,
    body : instr list ref,
    
    (* a label and a list of arguments to this basic block consitutes an incoming
       edge, and this mantains the list of such edges *)
    incoming : (var * instr list) list ref
  }

  and bb = BB of { 
    name : var,
    body : instr list
  }
  
  
  

(**************************************************
 **************************************************)

  fun typeCheck (f : string) (expected : LT.ty, observed : LT.ty) =
    if LT.same(expected, observed) 
      then expected
      else raise Fail ("(llvm-backend) " ^ f ^ ": incompatible types,\n " 
        ^ "\texpected: " ^ LT.nameOf expected ^ "\n"
        ^ "\tobserved: " ^ LT.nameOf observed ^ "\n")

  (* new : (var, var list) -> t *)
  fun new (v, args) = (
    typeCheck "new" (LT.labelTy, LV.typeOf v) ;
    T { name = v, body = ref nil, args = args, incoming = ref nil }
   )
   
   (* labelOf : t -> var *)
   fun labelOf (T { name,... }) = name
   
   (* paramsOf : t -> var list *)
   fun paramsOf (T { args,... }) = args


  (* generate texual representation of the BB
     this will eventually be the meaty part of this builder.  *)
  (* toString : bb -> string *)
  fun toString (bb as BB {name, body}) : string = let
      val header = LV.identOf(name) ^ ":\n\t"
    in
      header ^ (S.concatWith "\n\t" (cvt body)) ^ "\n\n"
    end
    
    
  and break (INSTR{result,...}) = (case result
     of R_Var v => (LV.toString v, LV.typeOf v)
      
      | R_Const(C_Int(ty, i)) => (IntegerLit.toString i, ty)
      
      | R_Const(C_Undef ty) => ("undef", ty)

      | R_Const(C_Float(ty, f)) => let
        (* NOTE To quote the LLVM Language reference Manual:
        
        "The one non-intuitive notation for constants is the hexadecimal form of 
        floating point constants. For example, the form 'double    0x432ff973cafa8000' 
        is equivalent to (but harder to read than) 'double 4.5e+15'. The only time 
        hexadecimal floating point constants are required (and the only time that they 
        are generated by the disassembler) is when a floating point constant must be 
        emitted but it cannot be represented as a decimal floating point number in a 
        reasonable number of digits. For example, NaN’s, infinities, and other special 
        values are represented in their IEEE hexadecimal format so that assembly and 
        disassembly do not cause any bits to change in the constants."
        
        We decided to always print them in hexidecimal format. *)
        
           fun build vec = S.concat ("0x" :: (L.map cvt (Word8Vector.toList vec)))
           
           (* word8 is 8 bit value, as hex it's 1-2 digits, so we pad it *)
           and cvt w = StringCvt.padLeft #"0" 2 (Word8.fmt StringCvt.HEX w)
        
           val floatStr = if LT.same(ty, LT.floatTy)
                            then let
            (********** 32-bit IEEE representation *********
                NOTE LLVM uses a really terrible representation for
                float32 literals. Here it is:
                
                "The exact bit representation of the float is laid out with the
                corresponding bitwise representation of a double:  the sign
                bit is copied over, the exponent is encoded in the larger width,
                and the 23 bits of significand fills in the top 23 bits of significand
                in the double.  A double has 52 bits of significand, so this means
                that the last 29 bits of significand will always be ignored.  As an
                error-detection measure, the IR parser requires them to be zero." 
                            - John McCall's message on the LLVM developer mailing list in 2011
                
                https://groups.google.com/d/msg/llvm-dev/IlqV3TbSk6M/27dAggZOMb0J
                
             *)             
                            
                            
                            val (f32Vec, _) = F32ToBits.toBits f
                            val bits = L.foldr (* list of list of bits to list of bits *)
                                    (fn (xs, ys) => xs @ ys)
                                    nil
                                    (L.map (fn w => String.explode(StringCvt.padLeft #"0" 8 (Word8.fmt StringCvt.BIN w)))  (* vec to list of bits *)
                                        (L.tabulate(4, fn i => W8V.sub(f32Vec, i))) (* vec to list *)
                                    )
                                    
                            val indexedBits = ListPair.zipEq(L.tabulate(32, fn i => 31-i), bits)
                            fun filter f = L.filter f indexedBits
                            fun strip xs = L.map (fn (_, x) => x) xs
                                    
                            val signBit = filter (fn (i,_) => i = 31)
                            
                            val exponent = filter (fn (i,_) => 23 <= i andalso i <= 30)
                            
                            val mantissa = filter (fn (i,_) => 0 <= i andalso i <= 22)
                            
                            (* get the SP exponent _with_ bias *)
                            val exponentInt = let
                                    val binScan = StringCvt.scanString (Int.scan StringCvt.BIN)
                                in
                                    case binScan(String.implode (strip exponent))
                                      of SOME i => i
                                       | NONE => raise Fail "couldn't scan right."
                                end
                            
                            (* now we convert SP exponent to DP exponent *)
                            val newExponent = let
                                    val newInt = (case exponentInt
                                          of 0 => 0
                                           | 255 => 2047
                                           (* not a special exponent, convert the bias, so (sp-127)+1023 = sp+896 *)
                                           | i => i + 896)
                                           
                                    val asBits = Int.fmt StringCvt.BIN newInt
                                    (* the exponent must be 11 bitsin DP *)
                                    val padded = StringCvt.padLeft #"0" 11 asBits
                                in 
                                    String.explode padded
                                end
                                
                            (* the exponent grew by 3 bits, and we were originally 32 bits short, so we need 29 bits *)
                            val padding = L.tabulate(29, fn _ => #"0")
                            
                            (* put together all of the bits into a single string *)
                            val DPbits = String.implode((strip signBit) @ newExponent @ (strip mantissa) @ padding)
                            
                            (* now we do a change of base to hex and pad *)
                            val hexString = let
                                    val toIntInf = StringCvt.scanString (IntInf.scan StringCvt.BIN)
                                    val SOME i = toIntInf DPbits
                                in
                                    "0x" ^ (StringCvt.padLeft #"0" 16 (IntInf.fmt StringCvt.HEX i))
                                end
                            
                            in
                                hexString
                            end
                                
            (********** 64-bit IEEE representation **********)
                          else if LT.same(ty, LT.doubleTy)
                            then let 
                                    val (byteVec, _) = F64ToBits.toBits f
                                 in
                                    build byteVec
                                 end
                                
                          else raise Fail "unknown float lit type"
      in
            (floatStr, ty)
      end 

      | R_Const(C_Str v) => (LV.toString v, LV.typeOf v)

      | _ => raise Fail "invalid argument for an instruction"
  (* esac *))

  (* we don't use map because the body is actually a reversed
     list of instructions, so in the processing we put the
     list of strings back together in the right order. *)
  and cvt body = let
    fun process(nil, strs) = strs
      | process(inst::body, strs) = process(body, (getStr inst)::strs)
    in
      process(body, nil)
    end

  and getStr (phi as PHI{join : var, preds : (instr * var) vector }) = let
      
      fun cvt (ins, l) = let
        val (resName, resTy) = break ins
      in
        "[ " ^ resName ^ ", " ^ LV.toString l ^ " ]"
      end 
          

      val tings : string vector = V.map cvt preds
      val tings = V.foldr (fn (v, a) => v::a) [] tings
      val rhs = S.concatWith ", " tings

      val (lhs, lhsTy) = (LV.toString join, (LT.nameOf o LV.typeOf) join)

    in
      S.concat [lhs, " = phi ", lhsTy, " ", rhs ]
    end

    | getStr (inst as INSTR{result, kind, args, atr}) = let

      fun getArgStr withTy = fn instr => let
          val (resName, resTy) = break instr
        in
          if withTy
            then LT.nameOf(resTy) ^ " " ^ resName
          else resName
        end

      fun breakRes result = (case result
                         of R_Var v => SOME (LV.toString v, LV.typeOf v)
                          | R_None => NONE
                          | _ => raise Fail "invalid LHS for an instruction."
                      (* esac *))
                      
      fun getAlignment (attrs) = let
        fun f x = (case x
                    of A.Aligned _ => true
                     | _ => false)
      in
        L.find f (AS.listItems attrs)
      end
      
      fun optAttr(atr, a) = if AS.member(atr, a)
                            then (A.toString a) ^ " "
                            else ""
      
      (* pluck the single atomic ordering that must be in this set out,
         as a string. If there is no ordering specified, and there is a default,
         the default is returned. *)
      fun atomicOrdering(atr, dflt) = let
        val possibleOrderings = AS.addList(AS.empty, A.atomicOrderings)
        val chosenOrdering = AS.intersection(atr, possibleOrderings)
      in
        case (AS.listItems chosenOrdering, dflt)
        of ([ordering], _)          => A.toString ordering
         | (_, SOME dfltOrdering)   => A.toString dfltOrdering
         | _ => raise Fail ("either too many atomic orderings specified, or none, with no default for an op")
      end

      fun mkGEP(inbounds, (resName, resTy)) = (let
              val (ptrName, ptrTy) = break (V.sub(args, 0))
              val offsets = L.tabulate ((V.length args) - 1,
                              fn i => getArgStr true (V.sub(args, i+1)))
              val offsets = S.concatWith ", " offsets
            in
              S.concat
                [ resName, " = getelementptr " ^ (if inbounds then "inbounds " else ""),
                  (LT.nameOf o LT.deref) ptrTy, ", ",
                  LT.nameOf ptrTy, " ", ptrName, ", ",
                  offsets
                ]
            end)
    in
          (case (kind, breakRes result)    
            of (OP opc, SOME(resName, resTy)) => (case opc
                (*
                
                    Opcodes with results
                
                *)
              of (  Op.Add
                  | Op.Sub
                  | Op.Mul
                  | Op.Shl
                  | Op.SDiv
                  | Op.UDiv
                  | Op.SRem
                  | Op.URem
                  | Op.LShr 
                  | Op.AShr 
                  | Op.And 
                  | Op.Or 
                  | Op.Xor ) => let
                      (* we already ran checkAttr even though some of
                         these attrs are mutually exclusive relative to op *)
                         
                      val nsw = optAttr(atr, A.NSW)            
                      val nuw = optAttr(atr, A.NUW)
                      val exact = optAttr(atr, A.ExactDiv)
                      
                      val opcStr = Op.toString opc
                      val (arg1, ty) = break(V.sub(args, 0)) 
                      val (arg2, _) = break(V.sub(args, 1)) 
                    in
                      S.concat[resName, " = ", opcStr, " ", nuw, nsw, exact, LT.nameOf ty, " ", arg1, ", ", arg2]
                    end

               | (  Op.FAdd
                  | Op.FSub
                  | Op.FMul
                  | Op.FDiv
                  | Op.FRem ) => let
                      val fmathFlags = 
                            if AS.member(atr, A.FastMath)
                              then (A.toString A.FastMath)
                            else S.concatWith " " (L.map A.toString (AS.listItems atr))
                      val opcStr = Op.toString opc
                      val (arg1, ty) = break(V.sub(args, 0)) 
                      val (arg2, _) = break(V.sub(args, 1)) 
                    in
                      S.concat[resName, " = ", opcStr, " ",
                       fmathFlags, if not(AS.isEmpty(atr)) then " " else "",
                       LT.nameOf ty, " ", arg1, ", ", arg2]
                    end

               | Op.Icmp kind => let
                  val icmpStr = Op.toString(opc) ^ " " ^ Op.icmpKindToStr kind
                  val (arg1, ty) = break(V.sub(args, 0)) 
                  val (arg2, _) = break(V.sub(args, 1)) 
                in
                  S.concat [resName, " = ", icmpStr, " ", LT.nameOf ty, " ", arg1, ", ", arg2]
                end

               | Op.Fcmp kind => let
                      val fmathFlags = 
                            if AS.member(atr, A.FastMath)
                              then (A.toString A.FastMath)
                            else S.concatWith " " (L.map A.toString (AS.listItems atr))
                      val (arg1, ty) = break(V.sub(args, 0)) 
                      val (arg2, _) = break(V.sub(args, 1)) 
                    in
                      S.concat[resName, " = ", Op.toString opc, " ",
                       fmathFlags, if not(AS.isEmpty(atr)) then " " else "",
                       Op.fcmpKindToStr kind, " ",
                       LT.nameOf ty, " ", arg1, ", ", arg2]
                    end
                    
                (* casts *)
               | ( Op.Trunc   
                 | Op.ZExt    
                 | Op.SExt    
                 | Op.FPToUI  
                 | Op.FPToSI  
                 | Op.UIToFP  
                 | Op.SIToFP  
                 | Op.FPTrunc 
                 | Op.FPExt   
                 | Op.PtrToInt
                 | Op.IntToPtr
                 | Op.BitCast
                 | Op.AddrSpace ) => let
                    val (arg1, ty) = break(V.sub(args, 0))
                    in
                    S.concat[
                        resName, " = ", (Op.toString opc), " ",
                        LT.nameOf ty, " ", arg1, " to ", LT.nameOf resTy
                    ]
                    end
                                    
                | Op.Load => let
                     val (ptr, ptrTy) = break(V.sub(args, 0))
                     
                     val alignment = (case getAlignment atr
                                         of SOME a => (", " ^ (A.toString a) ^ " ")
                                          | NONE => "")
                                          
                     val atomic = optAttr(atr, A.Atomic)
                     val volatile = optAttr(atr, A.Volatile)
                 in
                     S.concat[
                         resName, " = ", Op.toString opc, " ", atomic, volatile,
                         LT.nameOf(LT.deref ptrTy), ", ",
                         LT.nameOf ptrTy, " ", ptr, alignment
                     ]
                 end
                 
               | Op.CmpXchg => let
                    val (addr, addrTy) = break(V.sub(args, 0))
                    val (cmp, cmpTy) = break(V.sub(args, 1))
                    val (new, newTy) = break(V.sub(args, 2))
                    
                    val volatile = optAttr(atr, A.Volatile)
                    val ordering = atomicOrdering(atr, SOME A.SeqCst)
                    
                   in
                   
                    S.concat[
                        resName, " = ", Op.toString opc, " ", volatile,
                        LT.nameOf addrTy, " ", addr, ", ",
                        LT.nameOf cmpTy, " ", cmp, ", ",
                        LT.nameOf newTy, " ", new, " ", ordering, " ", ordering
                    ]
                   
                   end
                   
                | Op.Armw phi => let
                     val (addr, addrTy) = break(V.sub(args, 0))
                     val (var, varTy) = break(V.sub(args, 1))
                     
                     val volatile = optAttr(atr, A.Volatile)
                     val ordering = atomicOrdering(atr, SOME A.SeqCst)
                     
                    in
                    
                     S.concat [
                         resName, " = ", Op.toString opc, " ", volatile,
                         Op.phiKindToStr phi, " ",
                         LT.nameOf addrTy, " ", addr, ", ",
                         LT.nameOf varTy, " ", var, " ", ordering
                     ]
                    
                    end
                                
               | _ => raise Fail ("opcode " ^ (Op.toString opc) ^ " (with result) not implemented.")

              (* esac *))
              
              (**** 
              
                    op codes which have no result
                    
               *****)
             | (OP opc, NONE) => (case opc
                 of Op.Store => let
                      val (ptr, ptrTy) = break(V.sub(args, 0))
                      val (var, varTy) = break(V.sub(args, 1))
                      
                      val alignment = (case getAlignment atr
                                          of SOME a => (", " ^ (A.toString a) ^ " ")
                                           | NONE => "")
                                           
                      val atomic = optAttr(atr, A.Atomic)
                      val volatile = optAttr(atr, A.Volatile)
                  in
                      S.concat[
                          Op.toString opc, " ", atomic, volatile,
                          LT.nameOf varTy, " ", var, ", ",
                          LT.nameOf ptrTy, " ", ptr, alignment
                      ]
                  end
                  
                  | Op.Pause => 
                        (* NOTE this op can only be implemented with inline assembly in LLVM,
                                and currently we assume x86 as the backend target.
                                not sure if the 2nd part should say "~{dirflag},~{fpsr},~{flags}"
                                instead of the empty string. *)
                        "call void asm sideeffect \"pause\", \"\"()"
                        
                  | Op.Fence => let
                    val ordering = atomicOrdering(atr, SOME A.SeqCst)
                  in
                    S.concat ["fence ", ordering]
                  end
                
                | _ => raise Fail ("opcode " ^ (Op.toString opc) ^ " (with no result) not implemented.")
                 
                 (* esac *))
             
             | (OP_GEP, SOME info) => mkGEP(false, info)
             
             | (OP_GEP_IB, SOME info) => mkGEP(true, info)
             
             | (OP_ExtractVal, SOME (resName, resTy)) => (let
                     val (aggName, aggTy) = break (V.sub(args, 0))
                     val offsets = L.tabulate ((V.length args) - 1,
                                     fn i => getArgStr false (V.sub(args, i+1)))
                     val offsets = S.concatWith ", " offsets
                   in
                     S.concat
                       [ resName, " = extractvalue ",
                         LT.nameOf aggTy, " ", aggName, ", ",
                         offsets
                       ]
                   end)
                   
             | (OP_InsertVal, SOME (resName, _)) => (let
                     (* #[agg, valu, offset1, offset2, ...] *)
                     val prefix = 2
                     val (aggName, aggTy) = break (V.sub(args, 0))
                     val (valName, valTy) = break (V.sub(args, 1))
                     val offsets = L.tabulate ((V.length args) - prefix,
                                     fn i => getArgStr false (V.sub(args, i+prefix)))
                     val offsets = S.concatWith ", " offsets
                   in
                     S.concat
                       [ resName, " = insertvalue ",
                         LT.nameOf aggTy, " ", aggName, ", ",
                         LT.nameOf valTy, " ", valName, ", ",
                         offsets
                       ]
                   end)
             
             | (OP_Return, NONE) => 
                if V.length args = 0
                  then "ret void"
                else S.concat ["ret ", getArgStr true (V.sub(args, 0))]
             
             (* unconditional branch *)
             | (OP_Br, NONE) => S.concat ["br ", getArgStr true (V.sub(args, 0))]
             
             | (OP_CondBr, NONE) => 
                 S.concat [
                    "br ", (getArgStr true (V.sub(args, 0))), ", ",
                    (getArgStr true (V.sub(args, 1))), ", ",
                    (getArgStr true (V.sub(args, 2)))
                 ]
                 
                 
             | (OP_Switch (default, arms), NONE) => let
                    (* type switch_arms = (var * (constant * var) list) *)
                    fun var2s default = 
                        ((LT.nameOf o LV.typeOf) default) ^ " " ^ (LV.toString default)
                    
                    fun mkArm (C_Int(ty, i), var) = 
                        S.concat [ "\t\t\t" , LT.nameOf ty, " ", IntegerLit.toString i, ", ", var2s var, "\n"]
                     
                    val armString = S.concat (L.map mkArm arms)
                 in
                    S.concat [
                       "switch ", (getArgStr true (V.sub(args, 0))), ", ",
                       var2s default, 
                       " [\n", armString, "\t\t]"
                    ]
                 end
                 
             
             | (OP_TailCall, NONE) => let
                 val (funcName, funcTy) = break (V.sub(args, 0))
                 val paramStr = S.concatWith ", " (L.tabulate((V.length args) - 1, 
                                 fn i => getArgStr true (V.sub(args, i+1))))
               in   

                (* FIXME TODO(kavon): currently doesn't include any attributes, also its only safe
                   to omit the function ty if it is not var arg and doesn't return a pointer or something. *)

                 S.concat ["musttail call ", jwaCC , " void ", funcName, "(", paramStr, ")"]
               end
               
        (* a NOTE about Call syntax as of LLVM 4.0+
                
            %x1 = call fastcc i8* @M_StringConcat2(i8* undef, i8* undef)  ; <- good
        	%x2 = call fastcc i8* (i8*, ...) @M_printf(i8* undef, i8* undef)  ; <- good
        	; oh my lord, only HERE is @M_printf considered a pointer! "functions are not values, refer to them as pointers"
        	%casted_printf = bitcast i8* (i8*, ...)* @M_printf to i8* (i8*, ...)*   
        	%x4 = call fastcc i8* (i8*, ...) %casted_printf(i8* %x1, i8* %x2) ; <- good
        	; %x2 = call fastcc i8* @M_printf(i8* undef, i8* undef) ; <- incorrect, is vararg.
        	; %x2 = call fastcc i8* (i8*, ...)* @M_printf(i8* undef, i8* undef) ; <- incorrect b/c of the pointer on the end
        	; %x5 = call fastcc i8* (i8*, ...)* %casted_printf(i8* %x4, i8* %x2)  ; <- incorrect due to vararg
        	
        	; it seems the safest thing to do is strip the * off the var and then print the whole type for the function.
                
                *)
             
             | (OP_Call cc, NONE) => let
                 val (funcName, funcTy) = break (V.sub(args, 0))
                 val paramStr = S.concatWith ", " (L.tabulate((V.length args) - 1, 
                                 fn i => getArgStr true (V.sub(args, i+1))))
                                 
                 val cc = case cc of SOME ccStr => ccStr ^ " " | NONE => ""
                 
                 val tail = optAttr(atr, A.Tail) 
               in   
                
                 S.concat [tail, "call ", cc, (LT.nameOf o LT.deref) funcTy, " ", funcName, "(", paramStr, ")"]
               end

             | (OP_Call cc, SOME(resName, resTy)) => let 
                 val (funcName, funcTy) = break (V.sub(args, 0))
                 val paramStr = S.concatWith ", " (L.tabulate((V.length args) - 1, 
                                 fn i => getArgStr true (V.sub(args, i+1))))
                 
                 val cc = case cc of SOME ccStr => ccStr ^ " " | NONE => ""
                 
                 val tail = optAttr(atr, A.Tail) 
               in   
                
                 S.concat [resName, " = ", tail, "call ", cc, (LT.nameOf o LT.deref) funcTy, " ", funcName, "(", paramStr, ")"]
               end

             | (OP_Unreachable, NONE) => "unreachable"

             | (OP_None, _) => raise Fail "basic block should not have wrapped var/const as an instruction."

             (*| _ => raise Fail "bogus LLVM instruction" *)
             
            (* end outer case *))
    
    end (* end of getStr *)


(**************************************************
 **************************************************)

    (* TODO: these need typechecks too *)

    fun intC (ty, i) = C_Int(ty, i)

    fun floatC (ty, f) = C_Float(ty, f)
    
    fun undef ty = C_Undef(ty)
    
    (* this is probably reusable elsewhere *)
    fun grabTy result = (case result
        of R_Var v => LV.typeOf v
         | R_Const(C_Int(theTy, _)) => theTy
         | R_Const(C_Undef theTy) => theTy
         | R_Const(C_Float(theTy, _)) => theTy
         | R_Const(C_Str v) => LV.typeOf v
      (* esac *))

    fun tyOfInstr (INSTR{result,...}) = grabTy result
    
    val toTy = tyOfInstr
    

  (* BASIC BLOCK MANIPULATION FUNCTIONS *)

  (* push an instruction onto the bottom of a given basic block. *)
  fun push (T{body=blk,...}, inst) = (blk := inst :: (!blk) ; inst)
  
  (* place a list of instructions onto the top of a given basic block. *)
  fun prepend (T{body=blk,...}, insts) = (blk := (!blk) @ insts ; insts)

  
  fun incoming(t as T{incoming, args, name, ...}, edge as (_, vals)) = let
    (* light type checking is done for phis here *)
    
    val nameStr = LV.nameOf name
    
    val zippd = ListPair.zipEq(args, vals) 
        handle UnequalLengths => (* better error message *)
            raise Fail ("addIncoming: incorrect number of arguments given to block " ^ nameStr)
            
    val _ = L.app (fn (lv, inst) => let
                    val paramTy = LV.typeOf lv
                    val varTy = toTy inst
                    in
                        (typeCheck ("addIncoming to " ^ nameStr ^ "\n") (paramTy, varTy) ; ())
                    end) 
                  zippd
  in
    (incoming := edge :: (!incoming); t)
  end
  

  


  (* Simple Instruction Builders *)

    (* fromV : var -> instr *)
  fun fromV v = INSTR { result = (R_Var v), kind = OP_None, args = #[], atr = AS.empty }

  (* fromC : constant -> instr *)
  fun fromC c = INSTR { result = (R_Const c), kind = OP_None, args = #[], atr = AS.empty }

  fun toV (INSTR { result = (R_Var v), ... }) = v

  (* Terminators *)

  fun terminate (blk as T {name, body, ...}, terminator) =
    ( push(blk, terminator) ; addPhis(blk) ; BB {name=name, body=(!body)} ) 

  (* we process incoming edges to produce phi instructions, and then place them
     at the beginning of the block. *)
     (*
     PHI of {
         join : var,   (* not a res because const and none are never allowed *)      
         preds : (instr * var) vector    (* val, basic block *)
       }
       
       t = T of { 
         name : var,
         args : var list,
         body : instr list ref,
         
         (* a label and a list of arguments to this basic block consitutes an incoming
            edge, and this mantains the list of such edges *)
         incoming : (var * instr list) list ref
       }
       *)
  and addPhis (blk as T {incoming, args, ...}) = let
        (* a phi with a single pred is allowed. *)
           
        val edges = !incoming
        val numEdges = L.length edges
        
        fun nonEmpty () = let
            val args = ListPair.zip(L.tabulate(L.length args, fn i => i), args)
            
            
            fun mkPhi (pos, argVar) = let
                val argPreds = L.map 
                                (fn (labV, vals) => (L.nth(vals, pos), labV))
                                edges
            in
                PHI { join = argVar, preds = (V.fromList argPreds) }
            end
        in
            prepend(blk, L.map mkPhi args)
        end
        
        fun empty () = [] 
        
        (* NOTE we do nothing if the block has no predecessors because we can't differentiate
        between a start block (which has no preds and cannot have phi nodes) and a dangling
        block somewhere else in the code. If this becomes a problem we could do some adhoc
        fix such as marking blocks as being a starter or not so we emit the dummy casts
        below for dangling blocks. in the current LLVM backend for manticore we rely on
        a contract pass that eliminates unused basic blocks *)
        
        (*fun empty () = let
            fun mkUndef argVar = let
                val tyy = LV.typeOf argVar
            in
                INSTR {
                  result = R_Var argVar,
                  kind = OP(Op.BitCast),
                  args = #[fromC(undef tyy)],
                  atr = AS.empty
                }
            end
        in
            prepend(blk, L.map mkUndef args)
        end*)
        
           
        
    in
        if numEdges > 0 then nonEmpty() else empty()
    end
    
    


  (* unreachable : t -> bb *)
  fun unreachable blk = 
    terminate(blk, INSTR {
        result = R_None,
        kind = OP_Unreachable,
        args = #[],
        atr = AS.empty
      })

  (* retVoid : t -> bb *)
  fun retVoid blk = 
    terminate(blk, INSTR {
        result = R_None,
        kind = OP_Return,
        args = #[],
        atr = AS.empty
      })

  (* ret : t -> instr -> bb *)
  fun ret blk inst = 
    terminate(blk, INSTR {
        result = R_None,
        kind = OP_Return,
        args = #[inst],
        atr = AS.empty
      })

  (* NOTE(kavon): All tail calls are marked `musttail` and followed by a `ret void` automatically
     in order to have tail call optimization performed on it. One must be careful
     to not allow `unreachable` to be placed after a musttail call because it will
     not be correctly lowered to a `jmp` as of LLVM 3.6 (see notes for more info)  *)

  (* tailCall : t -> (instr * instr vector) -> bb *)
  fun tailCall blk = fn (func, args) => 
    (* TODO(kavon): ensure the arg and func types match! *)
    (push(blk, INSTR {
        result = R_None,
        kind = OP_TailCall,
        args = (V.tabulate((V.length args) + 1,
                fn 0 => func 
                 | i => V.sub(args, i-1)
               )),
        atr = AS.empty
     });
     retVoid blk
    )

  (*val br : t -> var -> bb*)
  fun br blk targ =
    ( typeCheck "br" (LT.labelTy, LV.typeOf targ) ;
      terminate(blk, INSTR {
        result = R_None,
        kind = OP_Br,
        args = #[fromV targ],
        atr = AS.empty
      })
    )

  (*val condBr : t -> (instr * var * var) -> bb*)
  fun condBr blk = fn (cond as INSTR{result,...}, trueTarg, falseTarg) => let
      val chk = typeCheck "condBr"
    in
      ( chk (LT.mkInt(LT.cnt 1), grabTy result) ;
        chk (LT.labelTy, LV.typeOf trueTarg) ;
        chk (LT.labelTy, LV.typeOf falseTarg) ;

        terminate(blk, INSTR {
            result = R_None,
            kind = OP_CondBr,
            args = #[cond, fromV trueTarg, fromV falseTarg],
            atr = AS.empty
          })
      )
    end
    
   fun switch blk = fn (cond as INSTR{result,...}) => fn switchArms => let
            (* TODO add a type check like in condBr *)
            val _ = ()
        in
            terminate(blk, INSTR {
                result = R_None,
                kind = (OP_Switch switchArms),
                args = #[cond],
                atr = AS.empty   
            })
        end
        
   
    
 (* try to get a useful name from the argument to use as the prefix of a new symbol *)
  fun genPfx (arg as INSTR{result,...}) = (case result
      of R_Var oldVar => LV.nameOf oldVar
       | R_Const _ => "lit"
       | _ => "r"
      (* esac *))


  (* Instruction Builders *)
  
  fun mk blk attrs opKind inputs = 
    push(blk, 
      INSTR {
        result = 
          (case Op.typeCheck(opKind, V.map tyOfInstr inputs)
            of SOME tyy => R_Var (LV.new("r", tyy))
             | NONE => R_None),
        kind = OP opKind,
        args = inputs,
        atr = Op.checkAttrs(opKind, attrs)
      }
    )


  local

    val i32Ty = LT.mkInt(LT.cnt 32)
    
    (* for gep and gep_ib *)
    fun stripI32 offset = (case offset
      of C_Int(maybei32Ty, i) => 
        (* constant int offsets for struct positions must be i32 type and
           this should not overflow *)
        (typeCheck "gep" (i32Ty, maybei32Ty) ; LargeInt.toInt i)

      | _ => raise Fail "gep: offsets in GEP must be constant i32's"
      (* esac *))
      
    fun stripDummy instr = if (Op.isInt o toTy) instr 
        then 0 
        else raise Fail "offsets for address calc must be integers!"

    fun genGep mode stripper cvtr = 
      fn blk => 
      fn (arg1 as INSTR{result,...}, offsetSeq) => let

        val numOffsets = V.length offsetSeq
        
        val _ = if numOffsets > 0 then ()
                else raise Fail "gep: cannot have empty offset sequence"

        val argTy = (case result
                 of R_Var v => LV.typeOf v
                  | R_Const(C_Undef ty) => ty
                 (* GEP is only valid for pointer types,
                    so it could only be from a var. *)
                  | _ => raise Fail "gep: arg must be a var"
              (* esac *))

        

        val seq : int vector = V.map stripper offsetSeq

        val tyy = (case mode
                    of (OP_GEP | OP_GEP_IB) => LT.gepType(argTy, seq)
                     | OP_ExtractVal => LT.gevType(argTy, seq)
                     | _ => raise Fail "invalid"
                  (* esac *))

        (* first arg of a GEP is what var to offset from, followed by 
           the sequence of offsets. *)
        val args = V.tabulate(numOffsets + 1,
                      fn 0 => arg1
                       | i => cvtr(V.sub(offsetSeq, i-1)))

        val reg = LV.new("r", tyy)
      in
        push(blk,
          INSTR {
            result = R_Var reg,
            kind = mode,
            args = args,
            atr = AS.empty
          }
        )
      end


  in

    (* gep : t -> (instr * constant vector) -> instr *)
    val gep = genGep OP_GEP stripI32 fromC

    (* gep_ib : t -> (instr * constant vector) -> instr *)
    val gep_ib = genGep OP_GEP_IB stripI32 fromC
    
    (* calcAddr : t -> (instr * instr vector) -> instr *)
    val calcAddr = genGep OP_GEP stripDummy (fn x => x)
    
    val calcAddr_ib = genGep OP_GEP_IB stripDummy (fn x => x)
    
    
    
    val extractV = genGep OP_ExtractVal stripI32 fromC

  end
  
  (* t -> (instr * instr * constant vector) -> instr  *)
  fun insertV blk (agg, valu, offsets) = let
        val numOffsets = V.length offsets
        val prefix = 2
        (* #[agg, valu, offset1, offset2, ...] *)
        val args = V.tabulate(numOffsets + prefix,
                      fn 0 => agg
                       | 1 => valu
                       | i => fromC(V.sub(offsets, i-prefix)))
  in
        push(blk,
            INSTR {
                result = R_Var(LV.new(genPfx agg, toTy agg)),
                kind = OP_InsertVal,
                args = args,
                atr = AS.empty
            })
  end
        


  (* t -> cast_op -> (instr * ty) -> instr *)
  fun cast blk castKind = fn (arg : instr, targTy) =>
      push(blk,
        INSTR {
          result = R_Var (LV.new(genPfx arg, Op.checkCast(castKind, (tyOfInstr arg, targTy)))),
          kind = OP castKind,
          args = #[arg],
          atr = AS.empty
        })


  (* call : t -> convention -> (instr * instr vector) -> instr *)
  fun buildCall blk cc atr = 
    fn (func as INSTR{result=R_Var(funcVar),...}, args) => let

      (* TODO(kavon): ensure the call's types match up and that
          funTy is actually the right type: a function or a function ptr *)

      val funTy = LV.typeOf funcVar

      val result = (case LT.returnTy funTy
                     of SOME t => (case LT.node t
                        of Ty.T_Void => R_None
                         | _ => R_Var (LV.new("ret", t))
                        (* esac *))
                      | NONE => raise Fail "expected a function type."
                   (* esac *))

    in
      push(blk,
        INSTR {
          result = result,
          kind = OP_Call cc,
          args = (V.tabulate((V.length args) + 1,
                fn 0 => func 
                 | i => V.sub(args, i-1)
               )),
          atr = atr
        }
      )
    end
    
  and callAs blk cc = buildCall blk (SOME cc) AS.empty
  
  and call blk = buildCall blk NONE AS.empty
  
  and callAs' blk (atr, cc) = buildCall blk (SOME cc) atr

  
  (* addIncoming : t -> (var * instr list) -> t *)
  fun addIncoming blk edge = incoming(blk, edge)

  (* ty -> int -> instr *)
  fun iconst ty i = fromC(intC(ty, Int.toLarge i))
  
  (* produces a copy of the given BB with a fresh name
     and fresh parameter names (but keeps parameter types the same). *)
  fun copy oldBlk = copy' (LV.copy(labelOf oldBlk)) oldBlk
        
  and copy' newLab oldBlk = let
        val newParams = L.map LV.copy (paramsOf oldBlk)
      in
        new(newLab, newParams)
      end
  

end
