(* llvm-builder.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * LLVM Basic Block builder
 *)

functor LLVMBuilder (structure Spec : TARGET_SPEC) :> sig

    type t

    type instr
    type var
    type ty
    type bb
    type constant

    eqtype bin_op
    eqtype unary_op
    eqtype cast_op


    (* TODO(kavon): attributes? *)


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


    val uop : t -> unary_op -> instr -> instr

    val bop : t -> bin_op -> (instr * instr) -> instr

    (* getelementptr *)
    val gep : t -> (instr * constant vector) -> instr 

    (* getelementptr inbounds *)
    val gep_ib : t -> (instr * constant vector) -> instr 

    (* NOTE(kavon): something not supported right now is GEPs that calculate a
       vector of addresses, extend the interface if desired. *)

    val cast : t -> cast_op -> (instr * ty) -> instr

    (* C calls which return *)
    val call : t -> (instr * instr vector) -> instr

    (* join instruction *)
    val phi : t -> (instr * var) vector -> instr

    
    
  end = struct

  structure LV = LLVMVar (structure Spec = Spec)
  structure LT = LV.LT
  structure V = Vector

  structure Ty = LLVMTy
  structure Op = LLVMOp

  type ty = Ty.t
  type var = LV.var


  type bin_op = Op.bin_op
  type unary_op = Op.unary_op
  type cast_op = Op.cast_op


  datatype res 
    = R_Var of var 
    | R_Const of constant 
    | R_None  (* for instructions which have no LHS, like terminators *)

  and constant 
    = C_Int of ty * int (* TODO(kavon) this should be an IntInf.int *)
    | C_Float of ty * real 
    | C_Str of var (* string constants are global vars *)

  datatype opcode
    = OP_Binary of bin_op
    | OP_Cast of cast_op
    | OP_GEP
    | OP_GEP_IB
    | OP_Return
    | OP_Br
    (*| OP_IndrBr NOTE(kavon): see comment in sig *)
    | OP_CondBr
    | OP_TailCall
    | OP_Call
    | OP_Unreachable
    | OP_None  (* for wrapped constants and vars, as no operation occurs *)



  datatype instr 
    = INSTR of {
        result : res,
        kind : opcode,
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
  fun toString _ = raise Fail "todo"


(**************************************************
 **************************************************)

  (* push an instruction onto the given basic block *)
  fun push (T{body=blk,...}, inst) = (blk := inst :: (!blk) ; inst)

  (* this is probably reusable elsewhere *)
  fun grabTy result = (case result
      of R_Var v => LV.typeOf v
       | R_Const(C_Int(theTy, _)) => theTy
       | R_Const(C_Float(theTy, _)) => theTy
       | R_Const(C_Str v) => LV.typeOf v
    (* esac *))


  (* Simple Instruction Builders *)

    (* fromV : var -> instr *)
  fun fromV v = INSTR { result = (R_Var v), kind = OP_None, args = #[] }

  (* fromC : constant -> instr *)
  fun fromC c = INSTR { result = (R_Const c), kind = OP_None, args = #[] }



  (* Terminators *)

  fun terminate (blk as T {name, body}, inst) = 
    (push(blk, inst) ; BB {name=name, body=(!body)}) 


  (* unreachable : t -> bb *)
  fun unreachable blk = 
    terminate(blk, INSTR {
        result = R_None,
        kind = OP_Unreachable,
        args = #[]
      })

  (* retVoid : t -> bb *)
  fun retVoid blk = 
    terminate(blk, INSTR {
        result = R_None,
        kind = OP_Return,
        args = #[]
      })

  (* ret : t -> instr -> bb *)
  fun ret blk inst = 
    terminate(blk, INSTR {
        result = R_None,
        kind = OP_Return,
        args = #[inst]
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
               ))
     });
     retVoid blk
    )

  (*val br : t -> var -> bb*)
  fun br blk targ =
    ( typeCheck "br" (LT.labelTy, LV.typeOf targ) ;
      terminate(blk, INSTR {
        result = R_None,
        kind = OP_Br,
        args = #[fromV targ]
      })
    )

  (*val condBr : t -> (instr * var * var) -> bb*)
  fun condBr blk = fn (cond as INSTR{result,...}, trueTarg, falseTarg) =>
    ( typeCheck "condBr" (LT.mkInt(LT.cnt 1), grabTy result) ;
      typeCheck "condBr" (LT.labelTy, LV.typeOf trueTarg) ;
      typeCheck "condBr" (LT.labelTy, LV.typeOf falseTarg) ;

      terminate(blk, INSTR {
          result = R_None,
          kind = OP_CondBr,
          args = #[cond, fromV trueTarg, fromV falseTarg]
        })
    )
  


  (* Instruction Builders *)

  (* uop : t -> unary_op -> instr -> instr *)
  fun uop blk = fn opKind => fn (arg1 as INSTR{result,...}) => let

    val tyy = grabTy result

    val reg = LV.new("r", tyy)    

    fun negateWith mode const = (INSTR {
          result= R_Var reg,
          kind = OP_Binary( mode ),
          args = #[const, arg1]
        })

    val constDoubleZero = fromC(C_Float(LT.doubleTy, 0.0))
    val constFloatZero = fromC(C_Float(LT.floatTy, 0.0))
    val constIntZero = fromC(C_Int(tyy, 0))
    
    val constNeg1 = fromC(C_Int(tyy, ~1))

  in
    (* <result> = sub i32 0, %val          ; yields i32:result = -%var 
       <result> = xor i32 %V, -1          ; yields i32:result = ~%V  *)
    push(blk,
      case (opKind, LT.node tyy)
        of (Op.Neg, Ty.T_Int _) => negateWith Op.Sub constIntZero
         | (Op.NSWNeg, Ty.T_Int _) => negateWith Op.NSWSub constIntZero
         | (Op.NUWNeg, Ty.T_Int _) => negateWith Op.NUWSub constIntZero
         | (Op.Not, Ty.T_Int _) => negateWith Op.Xor constNeg1
         
         | (Op.FNeg, Ty.T_Float) => negateWith Op.FSub constFloatZero
         | (Op.FNeg, Ty.T_Double) => negateWith Op.FSub constDoubleZero
         | _ => raise Fail "uop: incompatible types"
    )
  end
    

    

  (*  bop : t -> bin_op -> (instr * instr) -> instr *)
  fun bop blk = fn opKind => 
    fn (arg1 as INSTR{result=arg1Res,...}, arg2 as INSTR{result=arg2Res,...}) => let
      val tyy = typeCheck "bop" (grabTy arg1Res, grabTy arg2Res)
      val reg = LV.new("r", tyy)
    in
      push(blk, 
        INSTR {
          result = R_Var reg,
          kind = OP_Binary opKind,
          args = #[arg1, arg2]
        }
      )
    end


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
            (* constant int offsets must be i32 type *)
            (typeCheck "gep" (i32Ty, maybei32Ty) ; i)

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
            args = args
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
  fun cast _ = raise Fail "not implemented"


  (* call : t -> (instr * instr vector) -> instr *)
  fun call _ = raise Fail "not implemented"


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
