(* sparccomp-instr-ext.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * compiling a trivial extensions to the Sparc instruction set
 * (UNIMP instruction)
 *)
signature SPARCCOMP_INSTR_EXT = sig
    structure T : MLTREE
    structure I : SPARCINSTR (* where T = T *)
                  where type T.Basis.cond = T.Basis.cond
                    and type T.Basis.div_rounding_mode = T.Basis.div_rounding_mode
                    and type T.Basis.ext = T.Basis.ext
                    and type T.Basis.fcond = T.Basis.fcond
                    and type T.Basis.rounding_mode = T.Basis.rounding_mode
                    and type T.Constant.const = T.Constant.const
                    and type ('s,'r,'f,'c) T.Extension.ccx = ('s,'r,'f,'c) T.Extension.ccx
                    and type ('s,'r,'f,'c) T.Extension.fx = ('s,'r,'f,'c) T.Extension.fx
                    and type ('s,'r,'f,'c) T.Extension.rx = ('s,'r,'f,'c) T.Extension.rx
                    and type ('s,'r,'f,'c) T.Extension.sx = ('s,'r,'f,'c) T.Extension.sx
                    and type T.I.div_rounding_mode = T.I.div_rounding_mode
                    and type T.Region.region = T.Region.region
                    and type T.ccexp = T.ccexp
                    and type T.fexp = T.fexp
                    (* and type T.labexp = T.labexp *)
                    and type T.mlrisc = T.mlrisc
                    and type T.oper = T.oper
                    and type T.rep = T.rep
                    and type T.rexp = T.rexp
                    and type T.stm = T.stm
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
    structure CFG : CONTROL_FLOW_GRAPH (* where I = I *)
                    where type I.addressing_mode = I.addressing_mode
                      and type I.ea = I.ea
                      and type I.instr = I.instr
                      and type I.instruction = I.instruction
                      and type I.operand = I.operand


    type reducer =
	 (I.instruction, I.C.cellset, I.operand, I.addressing_mode, CFG.cfg) TS.reducer

    val compileSext :
	reducer
	-> { stm: (T.stm, T.rexp, T.fexp, T.ccexp) SparcInstrExt.sext,
	     an: T.an list }
	-> unit
end

functor SparcCompInstrExt 
  (structure I   : SPARCINSTR
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
   structure CFG : CONTROL_FLOW_GRAPH (* where I = I and P = TS.S.P *)
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
  ) : SPARCCOMP_INSTR_EXT = 
struct
    structure CFG = CFG
    structure T = TS.T
    structure TS = TS
    structure I = I
    structure C = I.C
    structure X = SparcInstrExt

    type stm = (T.stm, T.rexp, T.fexp, T.ccexp) X.sext

    type reducer =
	 (I.instruction, I.C.cellset, I.operand, I.addressing_mode, CFG.cfg) TS.reducer

    fun compileSext reducer { stm: stm, an: T.an list } = let
	val TS.REDUCER { emit, operand, reduceOperand, ... } = reducer
    in
	case stm 
	 of X.UNIMP i => emit (I.unimp {const22 = i}, an)
	  | X.SAVE (r, i, d) => emit(I.save{r=reduceOperand(operand r), i=operand i, d=reduceOperand(operand d)}, an)
	  | X.RESTORE (r, i, d) => emit(I.restore{r=reduceOperand(operand r), i=operand i, d=reduceOperand(operand d)}, an)
    end
end
