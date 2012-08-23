functor PPCGasPseudoOps 
   ( structure T : MLTREE
     structure MLTreeEval : MLTREE_EVAL (* where T = T *)
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
      ) : PSEUDO_OPS_BASIS = 

struct
  structure T = T
  structure PB = PseudoOpsBasisTyp
  structure Fmt = Format

  structure Endian = 
     PseudoOpsBig
	 (structure T = T
	  structure MLTreeEval=MLTreeEval
	  val icache_alignment = 16
	  val max_alignment = SOME 7
	  val nop = {sz=4, en=0wx60000000: Word32.word})  (* FIX:: ori 0, 0, 0 *)

  structure GasPseudoOps = 
     GasPseudoOps(structure T = T
		  val labFmt = {gPrefix="", aPrefix="L"})

  type 'a pseudo_op = (T.labexp, 'a) PB.pseudo_op
  
  fun error msg = MLRiscErrorMsg.error ("GasPseudoOps.", msg)

  val sizeOf = Endian.sizeOf
  val emitValue = Endian.emitValue
  val lexpToString = GasPseudoOps.lexpToString
  val toString = GasPseudoOps.toString
  val defineLabel = GasPseudoOps.defineLabel
end
