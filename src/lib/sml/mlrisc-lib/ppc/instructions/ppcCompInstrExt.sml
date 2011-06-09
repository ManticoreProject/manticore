(* ppcCompInstrExt.sml
 *
 * COPYRIGHT (c) 2004 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * emit code for extensions to the ppc instruction set.
 *)

signature PPCCOMP_INSTR_EXT =
  sig
    structure I : PPCINSTR
    structure TS : MLTREE_STREAM (* where T = I.T *)
                   where type T.Basis.cond = I.T.Basis.cond
                     and type T.Basis.div_rounding_mode = I.T.Basis.div_rounding_mode
                     and type T.Basis.ext = I.T.Basis.ext
                     and type T.Basis.fcond = I.T.Basis.fcond
                     and type T.Basis.rounding_mode = I.T.Basis.rounding_mode
                     and type T.Constant.const = I.T.Constant.const
                     and type ('s,'r,'f,'c) T.Extension.ccx = ('s,'r,'f,'c) I.T.Extension.ccx
                     and type ('s,'r,'f,'c) T.Extension.fx = ('s,'r,'f,'c) I.T.Extension.fx
                     and type ('s,'r,'f,'c) T.Extension.rx = ('s,'r,'f,'c) I.T.Extension.rx
                     and type ('s,'r,'f,'c) T.Extension.sx = ('s,'r,'f,'c) I.T.Extension.sx
                     and type T.I.div_rounding_mode = I.T.I.div_rounding_mode
                     and type T.Region.region = I.T.Region.region
                     and type T.ccexp = I.T.ccexp
                     and type T.fexp = I.T.fexp
                     (* and type T.labexp = I.T.labexp *)
                     and type T.mlrisc = I.T.mlrisc
                     and type T.oper = I.T.oper
                     and type T.rep = I.T.rep
                     and type T.rexp = I.T.rexp
                     and type T.stm = I.T.stm
    structure CFG : CONTROL_FLOW_GRAPH  (* where I = I and  and P = TS.S.P *)
                    where type I.addressing_mode = I.addressing_mode
                      and type I.ea = I.ea
                      and type I.instr = I.instr
                      and type I.instruction = I.instruction
                      and type I.operand = I.operand
                    where type P.Client.pseudo_op = TS.S.P.Client.pseudo_op
                      and type P.T.Basis.cond = TS.S.P.T.Basis.cond
                      and type P.T.Basis.div_rounding_mode = TS.S.P.T.Basis.div_rounding_mode
                      and type P.T.Basis.ext = TS.S.P.T.Basis.ext
                      and type P.T.Basis.fcond = TS.S.P.T.Basis.fcond
                      and type P.T.Basis.rounding_mode = TS.S.P.T.Basis.rounding_mode
                      and type P.T.Constant.const = TS.S.P.T.Constant.const
                      and type ('s,'r,'f,'c) P.T.Extension.ccx = ('s,'r,'f,'c) TS.S.P.T.Extension.ccx
                      and type ('s,'r,'f,'c) P.T.Extension.fx = ('s,'r,'f,'c) TS.S.P.T.Extension.fx
                      and type ('s,'r,'f,'c) P.T.Extension.rx = ('s,'r,'f,'c) TS.S.P.T.Extension.rx
                      and type ('s,'r,'f,'c) P.T.Extension.sx = ('s,'r,'f,'c) TS.S.P.T.Extension.sx
                      and type P.T.I.div_rounding_mode = TS.S.P.T.I.div_rounding_mode
                      and type P.T.Region.region = TS.S.P.T.Region.region
                      and type P.T.ccexp = TS.S.P.T.ccexp
                      and type P.T.fexp = TS.S.P.T.fexp
                      (* and type P.T.labexp = TS.S.P.T.labexp *)
                      and type P.T.mlrisc = TS.S.P.T.mlrisc
                      and type P.T.oper = TS.S.P.T.oper
                      and type P.T.rep = TS.S.P.T.rep
                      and type P.T.rexp = TS.S.P.T.rexp
                      and type P.T.stm = TS.S.P.T.stm

    type reducer = 
      (I.instruction, I.C.cellset, I.operand, I.addressing_mode, CFG.cfg) TS.reducer

    val compileSext : 
       reducer 
	-> {stm: (I.T.stm, I.T.rexp, I.T.fexp, I.T.ccexp) PPCInstrExt.sext, 
	    an: I.T.an list} 
          -> unit
  end

functor PPCCompInstrExt (

    structure I : PPCINSTR
    structure TS  : MLTREE_STREAM (* where T = I.T *)
                    where type T.Basis.cond = I.T.Basis.cond
                      and type T.Basis.div_rounding_mode = I.T.Basis.div_rounding_mode
                      and type T.Basis.ext = I.T.Basis.ext
                      and type T.Basis.fcond = I.T.Basis.fcond
                      and type T.Basis.rounding_mode = I.T.Basis.rounding_mode
                      and type T.Constant.const = I.T.Constant.const
                      and type ('s,'r,'f,'c) T.Extension.ccx = ('s,'r,'f,'c) I.T.Extension.ccx
                      and type ('s,'r,'f,'c) T.Extension.fx = ('s,'r,'f,'c) I.T.Extension.fx
                      and type ('s,'r,'f,'c) T.Extension.rx = ('s,'r,'f,'c) I.T.Extension.rx
                      and type ('s,'r,'f,'c) T.Extension.sx = ('s,'r,'f,'c) I.T.Extension.sx
                      and type T.I.div_rounding_mode = I.T.I.div_rounding_mode
                      and type T.Region.region = I.T.Region.region
                      and type T.ccexp = I.T.ccexp
                      and type T.fexp = I.T.fexp
                      (* and type T.labexp = I.T.labexp *)
                      and type T.mlrisc = I.T.mlrisc
                      and type T.oper = I.T.oper
                      and type T.rep = I.T.rep
                      and type T.rexp = I.T.rexp
                      and type T.stm = I.T.stm
    structure CFG : CONTROL_FLOW_GRAPH (* where P = TS.S.P and I = I *)
                    where type P.Client.pseudo_op = TS.S.P.Client.pseudo_op
                      and type P.T.Basis.cond = TS.S.P.T.Basis.cond
                      and type P.T.Basis.div_rounding_mode = TS.S.P.T.Basis.div_rounding_mode
                      and type P.T.Basis.ext = TS.S.P.T.Basis.ext
                      and type P.T.Basis.fcond = TS.S.P.T.Basis.fcond
                      and type P.T.Basis.rounding_mode = TS.S.P.T.Basis.rounding_mode
                      and type P.T.Constant.const = TS.S.P.T.Constant.const
                      and type ('s,'r,'f,'c) P.T.Extension.ccx = ('s,'r,'f,'c) TS.S.P.T.Extension.ccx
                      and type ('s,'r,'f,'c) P.T.Extension.fx = ('s,'r,'f,'c) TS.S.P.T.Extension.fx
                      and type ('s,'r,'f,'c) P.T.Extension.rx = ('s,'r,'f,'c) TS.S.P.T.Extension.rx
                      and type ('s,'r,'f,'c) P.T.Extension.sx = ('s,'r,'f,'c) TS.S.P.T.Extension.sx
                      and type P.T.I.div_rounding_mode = TS.S.P.T.I.div_rounding_mode
                      and type P.T.Region.region = TS.S.P.T.Region.region
                      and type P.T.ccexp = TS.S.P.T.ccexp
                      and type P.T.fexp = TS.S.P.T.fexp
                      (* and type P.T.labexp = TS.S.P.T.labexp *)
                      and type P.T.mlrisc = TS.S.P.T.mlrisc
                      and type P.T.oper = TS.S.P.T.oper
                      and type P.T.rep = TS.S.P.T.rep
                      and type P.T.rexp = TS.S.P.T.rexp
                      and type P.T.stm = TS.S.P.T.stm
                    where type I.addressing_mode = I.addressing_mode
                      and type I.ea = I.ea
                      and type I.instr = I.instr
                      and type I.instruction = I.instruction
                      and type I.operand = I.operand
  ) : PPCCOMP_INSTR_EXT =  struct

    structure CFG = CFG
    structure T = TS.T
    structure I = I
    structure C = I.C
    structure X = PPCInstrExt
    structure TS = TS

    type stm = (T.stm, T.rexp, T.fexp, T.ccexp) X.sext

    type reducer = 
      (I.instruction, I.C.cellset, I.operand, I.addressing_mode, CFG.cfg) TS.reducer

    fun error msg = MLRiscErrorMsg.error("PPCCompInstrExt", msg)

    fun compileSext (reducer : reducer) {stm : stm, an : T.an list} = let
	  val TS.REDUCER{
		  reduceRexp, operand, emit, instrStream, addressOf, ...
		} = reducer
	  val TS.S.STREAM{emit=emitI, ...} = instrStream
	  fun emit' inst = emit(I.INSTR inst, an)
	  in
	    case stm
	     of X.STWU{src, ea} => let
		  val (base, disp) = addressOf ea
		  in
		    emit' (I.ST{
			st = I.STWU,
			rs = reduceRexp src,
			ra = base,
			d = disp,
			mem = T.Region.memory
		      })
		  end
	    (* end case *)
	  end

  end
