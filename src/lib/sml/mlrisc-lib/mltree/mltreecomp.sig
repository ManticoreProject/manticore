(* mltreeComp.sig --- translate mltrees to a flowgraph of target machine code.
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

(*
 * This signature describes how MLTree extensions are compiled.
 *)
signature MLTREE_EXTENSION_COMP =
sig
   structure T : MLTREE
   structure TS : MLTREE_STREAM (* where T = T *)
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
   structure I : INSTRUCTIONS
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

   (* 
    * The reducer is given to the client during the compilation of
    * the user extensions.
    *)
   type reducer = 
     (I.instruction,I.C.cellset,I.operand,I.addressing_mode,CFG.cfg) TS.reducer

   val compileSext : reducer -> {stm:T.sext, an:T.an list} -> unit
   val compileRext : reducer -> {e:T.ty * T.rext, rd:CellsBasis.cell, an:T.an list} -> unit
   val compileFext : reducer -> {e:T.ty * T.fext, fd:CellsBasis.cell, an:T.an list} -> unit
   val compileCCext : reducer -> {e:T.ty * T.ccext, ccd:CellsBasis.cell, an:T.an list} -> unit
end




signature MLTREECOMP = 
sig
   structure TS : MLTREE_STREAM
   structure I : INSTRUCTIONS 
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
   structure Gen : MLTREEGEN (* where T = TS.T *)
                   where type T.Basis.cond = TS.T.Basis.cond
                     and type T.Basis.div_rounding_mode = TS.T.Basis.div_rounding_mode
                     and type T.Basis.ext = TS.T.Basis.ext
                     and type T.Basis.fcond = TS.T.Basis.fcond
                     and type T.Basis.rounding_mode = TS.T.Basis.rounding_mode
                     and type T.Constant.const = TS.T.Constant.const
                     and type ('s,'r,'f,'c) T.Extension.ccx = ('s,'r,'f,'c) TS.T.Extension.ccx
                     and type ('s,'r,'f,'c) T.Extension.fx = ('s,'r,'f,'c) TS.T.Extension.fx
                     and type ('s,'r,'f,'c) T.Extension.rx = ('s,'r,'f,'c) TS.T.Extension.rx
                     and type ('s,'r,'f,'c) T.Extension.sx = ('s,'r,'f,'c) TS.T.Extension.sx
                     and type T.I.div_rounding_mode = TS.T.I.div_rounding_mode
                     and type T.Region.region = TS.T.Region.region
                     and type T.ccexp = TS.T.ccexp
                     and type T.fexp = TS.T.fexp
                     (* and type T.labexp = TS.T.labexp *)
                     and type T.mlrisc = TS.T.mlrisc
                     and type T.oper = TS.T.oper
                     and type T.rep = TS.T.rep
                     and type T.rexp = TS.T.rexp
                     and type T.stm = TS.T.stm

   type instrStream = (I.instruction, I.C.cellset, CFG.cfg) TS.stream  
   type mltreeStream = (TS.T.stm, TS.T.mlrisc list, CFG.cfg) TS.stream 

    (* 
     * The instruction selection phase converts an instruction stream
     * into a mltree stream.  Please see the file "instructions/stream.sig"
     * for description of the stream interface.
     *
     * Note: the mltree stream does NOT support direct instruction emission.
     * Fo equivalent functionality, you can use the emit method 
     * of the instruction stream instead.
     *)
   val selectInstructions : instrStream -> mltreeStream
end




