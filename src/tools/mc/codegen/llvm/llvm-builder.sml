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

    eqtype op_code


    (* THE TODO LIST
      - make opcodes for icmp and other essential instructions
      - figure out how you will associate attributes with vars for
        stuff like calls? maybe just add a "fromVWithAttr" or something??
      - otherwise, start using the builder in the printer.
      - also, it would be nice to have this fn:
            val comment : str -> instr -> instr
      
      *)


    (* start a fresh basic block. first one is the name
       of the block and the list is the list of arguments
       to the block *)
    val new : (var * var list) -> t
    
    val labelOf : t -> var
    
    val paramsOf : t -> var list

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

    (* NOTE(kavon): I don't see myself using indirectbr because we should
                    not need to take block addresses, so I'm not implementing it. *)
    (*val indirectBr : t -> (instr * var vector) -> bb*)

    

    (* Instruction Builders *)

    (* wrappers for vars and constants *)
    val fromV : var -> instr

    val fromC : constant -> instr
    
    val toV : instr -> var
    
    val toTy : instr -> ty

    val intC : (ty * IntInf.int) -> constant
    
    (* NOTE FIXME TODO XXX in LLVM, float constants are invalid if the representation
       is not an exact value when represented in binary for that type. 
       Example: "1.25" and "1.0" are accepted as written but "1.3"
       is rejected because it is a repeating decimal when written in binary if the
       type is float.
       
       Clang Case Study: the value 1.3
       
       Single Precision -- generates: float 0x3FF4CCCCC0000000  (float 1.3 is rejected)
       Double Precision -- generates: double 1.300000e+00   (double 1.3 is accepted)
       
       I'm pretty sure that 1.3 is a repeating value in IEEE 754, no matter
       the precision, so the fact that it sometimes reject and sometimes accepts
       it kind of a mystery.
       
       Thus, you must provide a FloatLit.float representation of the float,
       because that module knows how to generate a correct hexidecimal representation
       of a floating point number in the IEEE 754 encoding *)
       
    val floatC : (ty * FloatLit.float) -> constant
    
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
       
       
    val extractV : t -> (instr * constant vector) -> instr 

    val cast : t -> op_code -> (instr * ty) -> instr

    (* C calls which return *)
    val call : t -> (instr * instr vector) -> instr



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

  type ty = Ty.t
  type var = LV.var
  type attrs = AS.set

  type op_code = Op.op_code


  datatype res 
    = R_Var of var 
    | R_Const of constant 
    | R_None  (* for instructions which have no LHS, like terminators *)

    (* Simple Constants in LLVM *)
  and constant 
    = C_Int of ty * IntInf.int
    
    | C_Float of ty * FloatLit.float  
    
    | C_Str of var (* TODO string constants are global vars, why do we need this? *)
    
    | C_Undef of ty  (* for undefined literals of any type except label or void *)

  datatype opkind
    = OP of op_code
    | OP_GEP
    | OP_GEP_IB
    | OP_ExtractVal
    | OP_Return
    | OP_Br
    | OP_CondBr
    | OP_TailCall
    | OP_Call
    | OP_Unreachable
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
      
      | R_Const(C_Int(ty, i)) => (IntInf.toString i, ty)
      
      | R_Const(C_Undef ty) => ("undef", ty)

      | R_Const(C_Float(ty, f)) => (FloatLit.toString f, ty)

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
                  | Op.UDiv ) => let
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
                 | Op.BitCast ) => let
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
                    
                   in
                   
                    S.concat[
                        resName, " = ", Op.toString opc, " ", volatile,
                        LT.nameOf addrTy, " ", addr, ", ",
                        LT.nameOf cmpTy, " ", cmp, ", ",
                        LT.nameOf newTy, " ", new, " seq_cst seq_cst"
                    ]
                   
                   end
                   
                | Op.Armw phi => let
                     val (addr, addrTy) = break(V.sub(args, 0))
                     val (var, varTy) = break(V.sub(args, 1))
                     
                     val volatile = optAttr(atr, A.Volatile)
                     
                    in
                    
                     S.concat [
                         resName, " = ", Op.toString opc, " ", volatile,
                         Op.phiKindToStr phi, " ",
                         LT.nameOf addrTy, " ", addr, ", ",
                         LT.nameOf varTy, " ", var, " seq_cst"
                     ]
                    
                    end
                                
               | _ => "; opcode " ^ (Op.toString opc) ^ " (with result) not implemented."

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
                
                | _ => "; opcode " ^ (Op.toString opc) ^ " (with no result) not implemented."
                 
                 (* esac *))
             
             | (OP_GEP, SOME info) => mkGEP(false, info)
             
             | (OP_GEP_IB, SOME info) => mkGEP(true, info)
             
             | (OP_ExtractVal, SOME (resName, resTy)) => (let
                     val (aggName, aggTy) = break (V.sub(args, 0))
                     val offsets = L.tabulate ((V.length args) - 1,
                                     fn i => getArgStr true (V.sub(args, i+1)))
                     val offsets = S.concatWith ", " offsets
                   in
                     S.concat
                       [ resName, " = extractvalue ",
                         LT.nameOf aggTy, " ", aggName, ", ",
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
             
             | (OP_TailCall, NONE) => let
                 val (funcName, funcTy) = break (V.sub(args, 0))
                 val paramStr = S.concatWith ", " (L.tabulate((V.length args) - 1, 
                                 fn i => getArgStr true (V.sub(args, i+1))))
               in   

                (* TODO(kavon): currently doesn't include CC or any attributes *)

                 S.concat ["musttail call ", LT.nameOf funcTy, " ", funcName, "(", paramStr, ")"]
               end
             
             | (OP_Call, NONE) => let
                 val (funcName, funcTy) = break (V.sub(args, 0))
                 val paramStr = S.concatWith ", " (L.tabulate((V.length args) - 1, 
                                 fn i => getArgStr true (V.sub(args, i+1))))
               in   

                (* TODO(kavon): currently doesn't include CC or any attributes *)
                
                 S.concat ["call ", LT.nameOf funcTy, " ", funcName, "(", paramStr, ")"]
               end

             | (OP_Call, SOME(resName, resTy)) => let 
                 val (funcName, funcTy) = break (V.sub(args, 0))
                 val paramStr = S.concatWith ", " (L.tabulate((V.length args) - 1, 
                                 fn i => getArgStr true (V.sub(args, i+1))))
               in   

                (* TODO(kavon): currently doesn't include CC or any attributes *)
                
                 S.concat [resName, " = call ", LT.nameOf funcTy, " ", funcName, "(", paramStr, ")"]
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

  
  fun incoming(t as T{incoming, args, ...}, edge as (_, vals)) = let
    (* light type checking is done for phis here *)
    val zippd = ListPair.zipEq(args, vals) 
        handle UnequalLengths => (* better error message *)
            raise Fail "addIncoming: incorrect number of arguments given to a basic block!"
            
    val _ = L.app (fn (lv, inst) => 
                    if LT.same(LV.typeOf lv, toTy inst) then ()
                    else raise Fail "addIncoming: type mismatch between args and params of a basic block!") zippd
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
        (* a phi with a single pred is allowed. if the block has no predecessors
           it should get deleted by an optimization pass later, and in that case 
           we will emit a warning TODO and bind the args to undef values
           using a dummy bitcast. *)
           
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
        below for dangling blocks. *)
        
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
      
      
      (*fun dummyOffsets nonConstOffs = V.map (fn x => fromC(intC(i32Ty, 0))) nonConstOffs
      
      fun calcAddrMkr mode = 
        fn blk => 
        fn (arg1 as INSTR{result,...}, offsetSeq) => let
        
            val argTy = (case result
                     of R_Var v => LV.typeOf v
                      | R_Const(C_Undef ty) => ty
                     (* GEP is only valid for pointer types,
                        so it could only be from a var. *)
                      | _ => raise Fail "calcAddrMkr: arg must be a var"
                  (* esac *))
        
            val resultTy = LV.gepType()*)


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


  (* t -> cast_op -> (instr * ty) -> instr *)
  fun cast blk castKind = fn (arg : instr, targTy) =>
      push(blk,
        INSTR {
          result = R_Var (LV.new(genPfx arg, Op.checkCast(castKind, (tyOfInstr arg, targTy)))),
          kind = OP castKind,
          args = #[arg],
          atr = AS.empty
        })


  (* call : t -> (instr * instr vector) -> instr *)
  fun call blk = 
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
          kind = OP_Call,
          args = (V.tabulate((V.length args) + 1,
                fn 0 => func 
                 | i => V.sub(args, i-1)
               )),
          atr = AS.empty
        }
      )
    end

  
  (* addIncoming : t -> (var * instr list) -> t *)
  fun addIncoming blk edge = incoming(blk, edge)


end
