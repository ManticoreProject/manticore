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
      - convert old-style instructions such as GEP into the new-style
      - figure out how you will associate attributes with vars for
        stuff like calls? maybe just add a "fromVWithAttr" or something??
      - otherwise, start using the builder in the printer.
      *)


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

    (* NOTE(kavon): I don't see myself using indirectbr because we should
                    not need to take block addresses, so I'm not implementing it. *)
    (*val indirectBr : t -> (instr * var vector) -> bb*)

    

    (* Instruction Builders *)

    (* wrappers for vars and constants *)
    val fromV : var -> instr

    val fromC : constant -> instr

    val intC : (ty * IntInf.int) -> constant
    
    val floatC : (ty * real) -> constant


    val mk : t -> attrs -> op_code -> instr vector -> instr


    (* getelementptr *)
    val gep : t -> (instr * constant vector) -> instr 

    (* getelementptr inbounds *)
    val gep_ib : t -> (instr * constant vector) -> instr 

    (* NOTE(kavon): something not supported right now is GEPs that calculate a
       vector of addresses, extend the interface if desired. *)

    val cast : t -> op_code -> (instr * ty) -> instr

    (* C calls which return *)
    val call : t -> (instr * instr vector) -> instr

    (* join instruction *)
    val phi : t -> (instr * var) vector -> instr


    
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

  and constant 
    = C_Int of ty * IntInf.int
    | C_Float of ty * real  (* QUESTION(kavon): is this precise enough? *)
    | C_Str of var (* string constants are global vars *)

  datatype opkind
    = OP of op_code
    | OP_GEP
    | OP_GEP_IB
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
    body : instr list ref
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

  (* new : var -> t *)
  fun new v = (
    typeCheck "new" (LT.labelTy, LV.typeOf v) ;
    T { name = v, body = ref nil }
   )


  (* generate texual representation of the BB
     this will eventually be the meaty part of this builder.  *)
  (* toString : bb -> string *)
  fun toString (bb as BB {name, body}) : string = let
      val header = LV.identOf(name) ^ ":\n\t"
    in
      header ^ (S.concatWith "\n\t" (cvt body))
    end

  (* we don't use map because the body is actually a reversed
     list of instructions, so in the processing we put the
     list of strings back together in the right order. *)
  and cvt body = let
    fun process(nil, strs) = strs
      | process(inst::body, strs) = process(body, (getStr inst)::strs)
    in
      process(body, nil)
    end

  and getStr (inst as INSTR{result, kind, args, atr}) = let

    fun break (INSTR{result,...}) = (case result
                       of R_Var v => (LV.toString v, LV.typeOf v)
                        
                        | R_Const(C_Int(ty, i)) => (IntInf.toString i, ty)

                        | R_Const(C_Float(ty, f)) => (Real.toString f, ty)

                        | R_Const(C_Str v) => (LV.toString v, LV.typeOf v)

                        | _ => raise Fail "invalid argument for an instruction"
                    (* esac *))

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
      case (kind, breakRes result)    
        of (OP opc, SOME(resName, resTy)) => (case opc
          of (  Op.Add
              | Op.Sub
              | Op.Mul
              | Op.Shl ) => let
                  val nsw = if AS.member(atr, A.NSW)
                            then (A.toString A.NSW ^ " ")
                            else ""
                  val nuw = if AS.member(atr, A.NUW)
                            then (A.toString A.NUW ^ " ")
                            else ""
                  val opcStr = Op.toString opc
                  val (arg1, ty) = break(V.sub(args, 0)) 
                  val (arg2, _) = break(V.sub(args, 1)) 
                in
                  S.concat[resName, " = ", opcStr, " ", nuw, nsw, LT.nameOf ty, " ", arg1, ", ", arg2]
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

           | _ => "; opcode " ^ (Op.toString opc) ^ " not implemented."

          (* esac *))
         
         | (OP_GEP, SOME info) => mkGEP(false, info)
         
         | (OP_GEP_IB, SOME info) => mkGEP(true, info)
         
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

         | _ => raise Fail "bogus LLVM instruction"
    end


(**************************************************
 **************************************************)

    (* TODO: these need typechecks too *)

    fun intC (ty, i) = C_Int(ty, i)

    fun floatC (ty, f) = C_Float(ty, f)
    

  (* push an instruction onto the given basic block *)
  fun push (T{body=blk,...}, inst) = (blk := inst :: (!blk) ; inst)

  (* this is probably reusable elsewhere *)
  fun grabTy result = (case result
      of R_Var v => LV.typeOf v
       | R_Const(C_Int(theTy, _)) => theTy
       | R_Const(C_Float(theTy, _)) => theTy
       | R_Const(C_Str v) => LV.typeOf v
    (* esac *))

  fun tyOfInstr (INSTR{result,...}) = grabTy result


  (* Simple Instruction Builders *)

    (* fromV : var -> instr *)
  fun fromV v = INSTR { result = (R_Var v), kind = OP_None, args = #[], atr = AS.empty }

  (* fromC : constant -> instr *)
  fun fromC c = INSTR { result = (R_Const c), kind = OP_None, args = #[], atr = AS.empty }



  (* Terminators *)

  fun terminate (blk as T {name, body}, inst) = 
    (push(blk, inst) ; BB {name=name, body=(!body)}) 


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

    fun genGep mode = 
      fn blk => 
      fn (arg1 as INSTR{result,...}, offsetSeq) => let

        val numOffsets = V.length offsetSeq
        
        val _ = if numOffsets > 0 then ()
                else raise Fail "gep: cannot have empty offset sequence"

        val argTy = (case result
                 of R_Var v => LV.typeOf v
                 (* GEP is only valid for pointer types,
                    so it could only be from a var. *)
                  | _ => raise Fail "gep: arg must be a var"
              (* esac *))

        fun stripI32 offset = (case offset
          of C_Int(maybei32Ty, i) => 
            (* constant int offsets for struct positions must be i32 type and
               this should not overflow *)
            (typeCheck "gep" (i32Ty, maybei32Ty) ; LargeInt.toInt i)

          | _ => raise Fail "gep: offsets in GEP must be constant i32's"
          (* esac *))

        val seq : int vector = V.map stripI32 offsetSeq

        val tyy = LT.gepType(argTy, seq)

        (* first arg of a GEP is what var to offset from, followed by 
           the sequence of offsets. *)
        val args = V.tabulate(numOffsets + 1,
                      fn 0 => arg1
                       | i => fromC(V.sub(offsetSeq, i-1)))

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
    val gep = genGep OP_GEP

    (* gep_ib : t -> (instr * constant vector) -> instr *)
    val gep_ib = genGep OP_GEP_IB

  end


  (* t -> cast_op -> (instr * ty) -> instr *)
  fun cast blk castKind = fn (arg : instr, targTy) =>
      push(blk,
        INSTR {
          result = R_Var (LV.new("r", Op.checkCast(castKind, (tyOfInstr arg, targTy)))),
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
                         | _ => R_Var (LV.new("r", t))
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


  (* phi : t -> (instr * var) vector -> instr *)
  fun phi blk predPairs = let

    val check = typeCheck "phi"

    fun getRes (INSTR{result,...}, _) = result

    fun typeCheck ((INSTR{result,...}, origin), otherTy) = (
      check(LT.labelTy, LV.typeOf origin) ;
      check(otherTy, grabTy result)
    )

    val tyy : LT.ty = V.foldl typeCheck ((grabTy o getRes) (V.sub(predPairs, 0))) predPairs

    val reg = LV.new("r", tyy)
  
  in
    push(blk,
      PHI {
        join = reg,
        preds = predPairs
      }
    )
  end 

     

end
