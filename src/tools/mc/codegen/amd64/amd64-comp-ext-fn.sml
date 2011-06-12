(* amd64-comp-ext-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor AMD64CompExtFn (
    structure I : AMD64INSTR (* where T.Extension = AMD64Extension *)
      where type ('s,'r,'f,'c) T.Extension.sx = ('s,'r,'f,'c) AMD64Extension.sx
      where type ('s,'r,'f,'c) T.Extension.rx = ('s,'r,'f,'c) AMD64Extension.rx
      where type ('s,'r,'f,'c) T.Extension.fx = ('s,'r,'f,'c) AMD64Extension.fx
      where type ('s,'r,'f,'c) T.Extension.ccx = ('s,'r,'f,'c) AMD64Extension.ccx 
    structure TS : MLTREE_STREAM (* where T = I.T *)
      where type ('s,'r,'f,'c) T.Extension.sx = ('s,'r,'f,'c) I.T.Extension.sx
      where type ('s,'r,'f,'c) T.Extension.rx = ('s,'r,'f,'c) I.T.Extension.rx
      where type ('s,'r,'f,'c) T.Extension.fx = ('s,'r,'f,'c) I.T.Extension.fx
      where type ('s,'r,'f,'c) T.Extension.ccx = ('s,'r,'f,'c) I.T.Extension.ccx 
          where type T.cond = I.T.cond
          where type T.fcond = I.T.fcond
          where type T.rounding_mode = I.T.rounding_mode
          where type T.div_rounding_mode = I.T.div_rounding_mode
          where type T.ext = I.T.ext
          where type T.stm = I.T.stm
          where type T.rexp = I.T.rexp
          where type T.rep = I.T.rep
          where type T.oper = I.T.oper
          where type T.fexp = I.T.fexp
          where type T.ccexp = I.T.ccexp
          where type T.mlrisc = I.T.mlrisc
          where type T.Constant.const = I.T.Constant.const
          where type T.I.div_rounding_mode = I.T.I.div_rounding_mode
          where type T.Region.region = I.T.Region.region
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
  ) : MLTREE_EXTENSION_COMP =
  struct

    structure T = TS.T
    structure TS = TS
    structure I = I
    structure CFG = CFG
    structure C = I.C

    structure CompInstrExt = AMD64CompInstrExt (
      structure I = I
      structure TS = TS
      structure CFG = CFG)

    type reducer =
	  (I.instruction,C.cellset,I.operand,I.addressing_mode,CFG.cfg) TS.reducer

    val compileSext = CompInstrExt.compileSext

    fun compileRext _ = raise Fail "AMD64CompExtFn.compileRext"
    fun compileFext _ = raise Fail "AMD64CompExtFn.compileFext"
    fun compileCCext _ = raise Fail "AMD64CompExtFn.compileCCext"

  end
