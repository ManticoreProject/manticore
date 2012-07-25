(*
 * Generate a linear sequence of instructions
 *)
functor InstrGen
   (structure I      : INSTRUCTIONS
    structure Stream : INSTRUCTION_STREAM
    structure CFG    : CONTROL_FLOW_GRAPH (* where I = I and P = Stream.P *)
                       where type I.addressing_mode = I.addressing_mode
                         and type I.ea = I.ea
                         and type I.instr = I.instr
                         and type I.instruction = I.instruction
                         and type I.operand = I.operand
                       where type P.Client.pseudo_op = Stream.P.Client.pseudo_op
                         and type P.T.Basis.cond = Stream.P.T.Basis.cond
                         and type P.T.Basis.div_rounding_mode = Stream.P.T.Basis.div_rounding_mode
                         and type P.T.Basis.ext = Stream.P.T.Basis.ext
                         and type P.T.Basis.fcond = Stream.P.T.Basis.fcond
                         and type P.T.Basis.rounding_mode = Stream.P.T.Basis.rounding_mode
                         and type P.T.Constant.const = Stream.P.T.Constant.const
                         and type ('s,'r,'f,'c) P.T.Extension.ccx = ('s,'r,'f,'c) Stream.P.T.Extension.ccx
                         and type ('s,'r,'f,'c) P.T.Extension.fx = ('s,'r,'f,'c) Stream.P.T.Extension.fx
                         and type ('s,'r,'f,'c) P.T.Extension.rx = ('s,'r,'f,'c) Stream.P.T.Extension.rx
                         and type ('s,'r,'f,'c) P.T.Extension.sx = ('s,'r,'f,'c) Stream.P.T.Extension.sx
                         and type P.T.I.div_rounding_mode = Stream.P.T.I.div_rounding_mode
                         and type P.T.Region.region = Stream.P.T.Region.region
                         and type P.T.ccexp = Stream.P.T.ccexp
                         and type P.T.fexp = Stream.P.T.fexp
                         (* and type P.T.labexp = Stream.P.T.labexp *)
                         and type P.T.mlrisc = Stream.P.T.mlrisc
                         and type P.T.oper = Stream.P.T.oper
                         and type P.T.rep = Stream.P.T.rep
                         and type P.T.rexp = Stream.P.T.rexp
                         and type P.T.stm = Stream.P.T.stm
   ) : INSTR_GEN =
struct
   structure C   = I.C
   structure I   = I
   structure S   = Stream
   structure CFG = CFG

   (* Pretty stupid, eh? *)
   fun newStream(instrs) =
   let fun emit i = instrs := i :: !instrs 
       fun can'tUse _ = MLRiscErrorMsg.error("InstrGen","unimplemented")
   in  Stream.STREAM
       { beginCluster   = can'tUse,
         endCluster     = can'tUse,
         emit           = emit,
         pseudoOp       = can'tUse,
         defineLabel    = can'tUse,
         entryLabel     = can'tUse,
         comment        = can'tUse,
         annotation     = can'tUse,
         getAnnotations = can'tUse,
         exitBlock      = can'tUse
       }
   end 

end
