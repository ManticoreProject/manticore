(*
 * Generate a linear sequence of instructions
 *)
signature INSTR_GEN =
sig
   structure C   : CELLS
   structure I   : INSTRUCTIONS
   structure S   : INSTRUCTION_STREAM
   structure CFG : CONTROL_FLOW_GRAPH 

   (* sharing I.C = C *)
   (* sharing CFG.P = S.P *)
   where type P.Client.pseudo_op = S.P.Client.pseudo_op
     and type P.T.Basis.cond = S.P.T.Basis.cond
     and type P.T.Basis.div_rounding_mode = S.P.T.Basis.div_rounding_mode
     and type P.T.Basis.ext = S.P.T.Basis.ext
     and type P.T.Basis.fcond = S.P.T.Basis.fcond
     and type P.T.Basis.rounding_mode = S.P.T.Basis.rounding_mode
     and type P.T.Constant.const = S.P.T.Constant.const
     and type ('s,'r,'f,'c) P.T.Extension.ccx = ('s,'r,'f,'c) S.P.T.Extension.ccx
     and type ('s,'r,'f,'c) P.T.Extension.fx = ('s,'r,'f,'c) S.P.T.Extension.fx
     and type ('s,'r,'f,'c) P.T.Extension.rx = ('s,'r,'f,'c) S.P.T.Extension.rx
     and type ('s,'r,'f,'c) P.T.Extension.sx = ('s,'r,'f,'c) S.P.T.Extension.sx
     and type P.T.I.div_rounding_mode = S.P.T.I.div_rounding_mode
     and type P.T.Region.region = S.P.T.Region.region
     and type P.T.ccexp = S.P.T.ccexp
     and type P.T.fexp = S.P.T.fexp
     (* and type P.T.labexp = S.P.T.labexp *)
     and type P.T.mlrisc = S.P.T.mlrisc
     and type P.T.oper = S.P.T.oper
     and type P.T.rep = S.P.T.rep
     and type P.T.rexp = S.P.T.rexp
     and type P.T.stm = S.P.T.stm

   (* 
    * This function creates an instruction stream, which can be 
    * used to emit instruction into the instruction list.
    *)
   val newStream : I.instruction list ref -> 
                     (I.instruction, Annotations.annotations, 'a, CFG.cfg) S.stream

end
