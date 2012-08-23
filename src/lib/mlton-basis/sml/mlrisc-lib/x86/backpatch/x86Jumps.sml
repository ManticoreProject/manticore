(* X86Jumps.sml --- information to resolve jumps for runtime code generation.
 *
 *  COPYRIGHT (c) 1997 Bell Laboratories.
 *)

functor X86Jumps
  (structure Instr : X86INSTR
   structure Eval : MLTREE_EVAL (* where T = Instr.T *)
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
   structure Shuffle : X86SHUFFLE (* where I = Instr *)
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
                         and type I.fbinOp = Instr.fbinOp
                         and type I.fenvOp = Instr.fenvOp
                         and type I.fibinOp = Instr.fibinOp
                         and type I.fsize = Instr.fsize
                         and type I.funOp = Instr.funOp
                         and type I.instr = Instr.instr
                         and type I.instruction = Instr.instruction
                         and type I.isize = Instr.isize
                         and type I.move = Instr.move
                         and type I.multDivOp = Instr.multDivOp
                         and type I.operand = Instr.operand
                         and type I.shiftOp = Instr.shiftOp
                         and type I.unaryOp = Instr.unaryOp
   structure MCEmitter : MC_EMIT (* where I = Instr *)
                         where type I.addressing_mode = Instr.addressing_mode
                           and type I.ea = Instr.ea
                           and type I.instr = Instr.instr
                           and type I.instruction = Instr.instruction
                           and type I.operand = Instr.operand
  ) : SDI_JUMPS = 
struct
  structure I = Instr
  structure C = I.C
  structure Const = I.Constant

  fun error msg = MLRiscErrorMsg.error("X86Jumps",msg)

  val esp = 4
  val ebp = 5
  val branchDelayedArch = false

  fun imm8 i = ~128 <= i andalso i < 128

  fun isSdi(I.ANNOTATION{i, ...}) = isSdi i
    | isSdi(I.LIVE _)		  = true
    | isSdi(I.KILL _)		  = true
    | isSdi(I.COPY _)		  = false
    | isSdi(I.INSTR instr) = let
	  fun operand(I.ImmedLabel _) = true
	    | operand(I.LabelEA _) = true
	    | operand(I.Displace{disp, ...}) = operand disp
	    | operand(I.Indexed{disp, ...}) = operand disp
	    | operand _ = false
	  fun cmptest{lsrc, rsrc} = operand lsrc orelse operand rsrc
      in 
	  case instr
	  of I.JMP(opnd, _) => operand opnd
	   | I.JCC{opnd, ...} => operand opnd
	   | I.BINARY{src, dst, ...} => operand src orelse operand dst
	   | I.MOVE{src, dst, ...} => operand src orelse operand dst
	   | I.LEA{addr, ...} => operand addr
	   | I.CMPL arg => cmptest arg
	   | I.CMPW arg => cmptest arg
	   | I.CMPB arg => cmptest arg
	   | I.TESTL arg => cmptest arg
	   | I.TESTW arg => cmptest arg
	   | I.TESTB arg => cmptest arg
	   | I.MULTDIV{src, ...} => operand src
	   | I.MUL3{src1, ...} => operand src1
	   | I.UNARY{opnd, ...} => operand opnd
	   | I.SET{opnd, ...} => operand opnd
	   | I.CMOV{src, dst, ...} => operand src 
	   | I.PUSHL opnd => operand opnd
	   | I.PUSHW opnd => operand opnd
	   | I.PUSHB opnd => operand opnd
	   | I.POP opnd =>  operand opnd
	   | I.FSTPT opnd => operand opnd
	   | I.FSTPL opnd => operand opnd
	   | I.FSTPS opnd => operand opnd
	   | I.FSTL opnd => operand opnd
	   | I.FSTS opnd => operand opnd
	   | I.FLDT opnd => operand opnd
	   | I.FLDL opnd => operand opnd
	   | I.FLDS opnd => operand opnd
	   | I.FBINARY{src, dst, ...} => operand src orelse operand dst
	   | I.FIBINARY{src, ...} => operand src 
	   | I.FILD opnd => operand opnd
	   | I.FILDL opnd => operand opnd
	   | I.FILDLL opnd => operand opnd
	   | _ => false
      end

  fun minSize(I.ANNOTATION{i, ...}) = minSize i
    | minSize(I.LIVE _)  = 0
    | minSize(I.KILL _)  = 0
    | minSize(I.INSTR i) = 
      (case i 
	of I.JMP _ => 2
	 | I.JCC _ => 2
	 | I.LEA _ => 2
	 |  _ => 1)
    | minSize _ = error"minSize"


  fun maxSize _ = 12

  (* value of span-dependent operand *)
  fun operand(I.ImmedLabel le) = Eval.valueOf le
    | operand(I.LabelEA le) = Eval.valueOf le
    | operand _ = error "operand"
  
  val encode = MCEmitter.emitInstr

  fun sdiSize(I.ANNOTATION{i, ...}, labmap, loc) = sdiSize(i, labmap, loc)
    | sdiSize(I.LIVE _, _, _) = 0
    | sdiSize(I.KILL _, _, _) = 0

    | sdiSize(I.INSTR instr, labmap, loc) = let
	fun branch(opnd, short, long) = let
	  val offset = operand opnd - loc
	in if imm8(offset - 2) then short else long
	end
      in
	case instr
	of I.JMP(opnd, _) => branch(opnd, 2, 5)
	 | I.JCC{opnd, ...} => branch(opnd, 2, 6)
	 | _ => Word8Vector.length(encode(I.INSTR instr))
      end  (*sdiSize*)
    | sdiSize _ = error "sdiSize"

  fun expand(I.ANNOTATION{i,...}, size, loc) = expand(i, size, loc)
    | expand(I.LIVE _, _, _) = []
    | expand(I.KILL _, _, _) = []
    | expand(I.INSTR instr, size, loc) = 
       (case instr 
	of I.JMP(opnd, labs)  => [I.jmp(I.Relative(operand opnd-loc), labs)]
	 | I.JCC{cond, opnd} => 
	    [I.jcc{cond=cond, opnd=I.Relative(operand opnd-loc)}]
	 | opnd => [I.INSTR opnd])
    | expand _ = error "expand"
end

