(* amd64MCFn.sml 
 *
 * Byte encoding for AMD64 instructions.
 *)

functor AMD64MCFn(
    structure Instr : AMD64INSTR
    structure Shuffle : AMD64SHUFFLE (* where I = Instr *)
                        where type I.Constant.const = Instr.Constant.const
                          and type I.Region.region = Instr.Region.region
                          and type I.T.Basis.cond = Instr.T.Basis.cond
                          and type I.T.Basis.div_rounding_mode = Instr.T.Basis.div_rounding_mode
                          and type I.T.Basis.ext = Instr.T.Basis.ext
                          and type I.T.Basis.fcond = Instr.T.Basis.fcond
                          and type I.T.Basis.rounding_mode = Instr.T.Basis.rounding_mode
                          and type ('s,'r,'f,'c) I.T.Extension.ccx = ('s,'r,'f,'c) Instr.T.Extension.ccx
                          and type ('s,'r,'f,'c) I.T.Extension.fx = ('s,'r,'f,'c) Instr.T.Extension.fx
                          and type ('s,'r,'f,'c) I.T.Extension.rx = ('s,'r,'f,'c) Instr.T.Extension.rx
                          and type ('s,'r,'f,'c) I.T.Extension.sx = ('s,'r,'f,'c) Instr.T.Extension.sx
                          and type I.T.I.div_rounding_mode = Instr.T.I.div_rounding_mode
                          and type I.T.ccexp = Instr.T.ccexp
                          and type I.T.fexp = Instr.T.fexp
                          (* and type I.T.labexp = Instr.T.labexp *)
                          and type I.T.mlrisc = Instr.T.mlrisc
                          and type I.T.oper = Instr.T.oper
                          and type I.T.rep = Instr.T.rep
                          and type I.T.rexp = Instr.T.rexp
                          and type I.T.stm = Instr.T.stm
                          (* and type I.addressing_mode = Instr.addressing_mode *)
                          and type I.binaryOp = Instr.binaryOp
                          and type I.bitOp = Instr.bitOp
                          and type I.cond = Instr.cond
                          and type I.fsize = Instr.fsize
                          and type I.instr = Instr.instr
                          and type I.instruction = Instr.instruction
                          and type I.isize = Instr.isize
                          and type I.move = Instr.move
                          and type I.multDivOp = Instr.multDivOp
                          and type I.operand = Instr.operand
                          and type I.shiftOp = Instr.shiftOp
                          and type I.unaryOp = Instr.unaryOp
    structure MLTreeEval : MLTREE_EVAL (* where T = Instr.T *)
                           where type T.Basis.cond = Instr.T.Basis.cond
                             and type T.Basis.div_rounding_mode = Instr.T.Basis.div_rounding_mode
                             and type T.Basis.ext = Instr.T.Basis.ext
                             and type T.Basis.fcond = Instr.T.Basis.fcond
                             and type T.Basis.rounding_mode = Instr.T.Basis.rounding_mode
                             and type T.Constant.const = Instr.T.Constant.const
                             and type ('s,'r,'f,'c) T.Extension.ccx = ('s,'r,'f,'c) Instr.T.Extension.ccx
                             and type ('s,'r,'f,'c) T.Extension.fx = ('s,'r,'f,'c) Instr.T.Extension.fx
                             and type ('s,'r,'f,'c) T.Extension.rx = ('s,'r,'f,'c) Instr.T.Extension.rx
                             and type ('s,'r,'f,'c) T.Extension.sx = ('s,'r,'f,'c) Instr.T.Extension.sx
                             and type T.I.div_rounding_mode = Instr.T.I.div_rounding_mode
                             and type T.Region.region = Instr.T.Region.region
                             and type T.ccexp = Instr.T.ccexp
                             and type T.fexp = Instr.T.fexp
                             (* and type T.labexp = Instr.T.labexp *)
                             and type T.mlrisc = Instr.T.mlrisc
                             and type T.oper = Instr.T.oper
                             and type T.rep = Instr.T.rep
                             and type T.rexp = Instr.T.rexp
                             and type T.stm = Instr.T.stm
(*    structure AsmEmitter : INSTRUCTION_EMITTER where I = Instr*)
  ) : MC_EMIT = 
  struct

    structure I = Instr
    structure C = I.C
    structure Const = I.Constant
    structure W32 = Word32
    structure W8 = Word8
    structure W = LargeWord
    structure CB = CellsBasis

    val itow  = Word.fromInt
    val wtoi  = Word.toInt

    fun const c = Int32.fromInt (Const.valueOf c)
    fun lexp le = Int32.fromInt (MLTreeEval.valueOf le : int)

    val toWord8 = Word8.fromLargeWord o LargeWord.fromLargeInt o Int32.toLarge
    val eBytes = Word8Vector.fromList 
    fun eByte i = eBytes [W8.fromInt i]
    local 
	val toLWord = (W.fromLargeInt o Int32.toLarge) 
	fun shift (w,cnt) = W8.fromLargeWord(W.>>(w, cnt))
    in
    fun eShort i16 = 
	let val w = toLWord i16
	in [shift(w, 0w0), shift(w,0w8)]
	end
    fun eLong i32 = 
	let val w = toLWord i32
	in [shift(w, 0w0), shift(w,0w8), shift(w,0w16), shift(w,0w24)] end
    end

    val regNum = CB.physicalRegisterNum

    val rsp = regNum C.rsp
    val rbp = regNum C.rbp

    fun regNumBot8 r = r mod 8

    nonfix mod    

    fun scale (n, m) = Word.toIntX(Word.<<(Word.fromInt n, Word.fromInt m))
    fun modrm {mod, reg, rm} = W8.fromInt(scale(mod,6) + scale(reg,3) + rm)
    fun sib {ss, index, base} = W8.fromInt(scale(ss,6) + scale(index,3) + base)

    type reg = int

  (* destination operands *)	
    datatype dst_opnd
      = REG_OPND of reg 
      | OPCODE_OPND of int

    fun immedOpnd (I.Immed i32) = i32
      | immedOpnd (I.ImmedLabel le) = lexp le
      | immedOpnd (I.LabelEA le) = lexp le

    local

	datatype size = Zero | Bits8 | Bits32
	fun size i = 
	    if i = 0 then Zero
	    else if Int32.<(i, 128) andalso Int32.<=(~128, i) then Bits8 
	    else Bits32

      (* register usage of an instruction *)
	type reg_info = {indexReg : reg option, baseReg : reg option}
	fun regsOfInstr (I.Direct(_, r)) = {indexReg=NONE, baseReg=SOME (regNum r)}
	  | regsOfInstr (I.Displace{base, ...}) = {indexReg=NONE, baseReg=SOME (regNum base)}
	  | regsOfInstr (I.Indexed {base=NONE, index, ...}) = {indexReg=SOME (regNum index), baseReg=NONE}
	  | regsOfInstr (I.Indexed {base=SOME b, index, ...}) = {indexReg=SOME (regNum b), baseReg=SOME (regNum index)}
	  | regsOfInstr _ = {indexReg=NONE, baseReg=NONE}

      (* to keep the destination operand at 3 bits, we truncate register operands; the upper bit goes in 
       * the rex byte.
       *)
	fun eDstOpnd (REG_OPND r) = regNumBot8 r
	  | eDstOpnd (OPCODE_OPND opc) = opc

	fun eImmedExt (dst, I.Direct (_, r)) =
	      [modrm{mod=3, reg=eDstOpnd dst, rm=regNumBot8(regNum r)}]
	  | eImmedExt (dst, I.Displace{base, disp, ...}) = let
	      val dst = eDstOpnd dst
	      val immed = immedOpnd disp
	      fun displace (mod, eDisp) = 
		  if regNum base = rsp then 
		      modrm{mod=mod, reg=dst, rm=4}::
		      sib{ss=0, index=4, base=rsp} :: eDisp immed
		  else
		      modrm{mod=mod, reg=dst, rm=regNum base} :: eDisp immed
	      in
		case size immed
		 of Zero => 
		    if regNum base = rsp then 
			[modrm{mod=0, reg=dst, rm=4}, sib{ss=0,index=4,base=rsp}]
		    else if regNum base = rbp then
			[modrm{mod=1, reg=dst, rm=rbp}, 0w0]
		    else 
			[modrm{mod=0, reg=dst, rm=regNum base}]
		  | Bits8 => displace (1, fn i => [toWord8 i])
		  | Bits32 => displace (2, eLong)
	      end

	fun eRex rb = raise Fail ""

      (* add the rex byte to an instruction over 32-bit operands *)
	fun eRex32 (rb : Word8.word) = 0wx40 + rb
      (* add the rex byte to an instruction over 64-bit operands *)
	fun eRex64 rb = eRex rb + 0wx8

	fun isExtReg r = r > 7

      (* construct the rex byte depending on the operands *)
	fun rexByte (REG_OPND dst, src) : Word8.word option = let
	      val {indexReg, baseReg} = regsOfInstr src
	      val rb1 = if isExtReg dst then 0wx4 else 0wx0
	      val rb2 = (case indexReg
			  of SOME r => if isExtReg r then rb1 + 0wx2 else rb1
			   | NONE => 0wx0
			(* end case *))
	      val rb3 = (case baseReg
			  of SOME r => if isExtReg r then rb2 + 0wx1 else rb2
			   | NONE => 0wx0
			(* end case *))
	      in
	         if rb3 = 0wx0 then NONE else SOME rb3
	      end

	fun encode32FromBytes (bytes, dst, src) = let
	      val e = eImmedExt(dst, src)
	      in
	        case rexByte (dst, src)
		 of SOME b => eRex32 b :: bytes @ e
		  | NONE => bytes @ e
	      end


    in
  (* encode an instruction with 32-bit operands *)
    fun encode32 (byte1, dst, src) = eBytes(encode32FromBytes([byte1], dst, src))
    fun encode64 x = raise Fail "todo"
    end

  (* byte encoding of an instruction *)
    fun eInstr instr = (
	  case instr
	   of I.UNARY{unOp, opnd} => (
	        case unOp
		 of I.INCL => encode32 (0wxff, OPCODE_OPND 0, opnd)
		  | I.INCQ => encode64 (0wxff, OPCODE_OPND 0, opnd)
		  | I.DECL => encode32 (0wxff, OPCODE_OPND 1, opnd)
		  | I.DECQ => encode64 (0wxff, OPCODE_OPND 1, opnd)
		  | I.NOTL => encode32 (0wxff, OPCODE_OPND 2, opnd)
		  | I.NOTQ => encode64 (0wxff, OPCODE_OPND 2, opnd)
		  | I.NEGL => encode32 (0wxff, OPCODE_OPND 3, opnd)
		  | I.NEGQ => encode64 (0wxff, OPCODE_OPND 3, opnd)
		  | _ => raise Fail "UNARY is not in DEC/INC/NEG,NOT"
	       (* esac *))
	    | _ => raise Fail "todo"
        (* end case *))

    fun emitInstr _ = raise Fail "todo"

  end
