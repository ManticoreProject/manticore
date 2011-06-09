(* unimplemented-c-calls.sml
 *
 *   A dummy (placeholder) "implementation" of the c-calls interface.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *)
functor UnimplementedCCallsFn
	    (structure T: MLTREE
	     val impossible: string -> 'a) :> C_CALLS (* where T = T *)
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
                                                and type T.stm = T.stm =
struct
    structure T = T

    datatype c_arg 
      = ARG of T.rexp	
      | FARG of T.fexp
      | ARGS of c_arg list

    fun genCall _ = impossible "C-calls not implemented (genCall)"

    val paramAreaOffset = 0

    val naturalIntSz = 32

    datatype arg_location
      = Reg of T.ty * T.reg * T.I.machine_int option
      | FReg of T.fty * T.reg * T.I.machine_int option
      | Stk of T.ty * T.I.machine_int
      | FStk of T.fty * T.I.machine_int
      | Args of arg_location list

    fun layout _ = impossible "C-calls not implemented (layout)"

    val calleeSaveRegs : T.reg list = []
    val calleeSaveFRegs : T.reg list = []
end
