(* pseudo-ops.sig
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * MLRISC pseudo-ops
 * Ties together the assembler and client pseudo-ops
 *)

signature PSEUDO_OPS = sig
  structure T : MLTREE
  structure Client : CLIENT_PSEUDO_OPS (* where AsmPseudoOps.T = T *)
                     where type AsmPseudoOps.T.Basis.cond = T.Basis.cond
                       and type AsmPseudoOps.T.Basis.div_rounding_mode = T.Basis.div_rounding_mode
                       and type AsmPseudoOps.T.Basis.ext = T.Basis.ext
                       and type AsmPseudoOps.T.Basis.fcond = T.Basis.fcond
                       and type AsmPseudoOps.T.Basis.rounding_mode = T.Basis.rounding_mode
                       and type AsmPseudoOps.T.Constant.const = T.Constant.const
                       and type ('s,'r,'f,'c) AsmPseudoOps.T.Extension.ccx = ('s,'r,'f,'c) T.Extension.ccx
                       and type ('s,'r,'f,'c) AsmPseudoOps.T.Extension.fx = ('s,'r,'f,'c) T.Extension.fx
                       and type ('s,'r,'f,'c) AsmPseudoOps.T.Extension.rx = ('s,'r,'f,'c) T.Extension.rx
                       and type ('s,'r,'f,'c) AsmPseudoOps.T.Extension.sx = ('s,'r,'f,'c) T.Extension.sx
                       and type AsmPseudoOps.T.I.div_rounding_mode = T.I.div_rounding_mode
                       and type AsmPseudoOps.T.Region.region = T.Region.region
                       and type AsmPseudoOps.T.ccexp = T.ccexp
                       and type AsmPseudoOps.T.fexp = T.fexp
                       (* and type AsmPseudoOps.T.labexp = T.labexp *)
                       and type AsmPseudoOps.T.mlrisc = T.mlrisc
                       and type AsmPseudoOps.T.oper = T.oper
                       and type AsmPseudoOps.T.rep = T.rep
                       and type AsmPseudoOps.T.rexp = T.rexp
                       and type AsmPseudoOps.T.stm = T.stm

  type pseudo_op = (T.labexp, Client.pseudo_op) PseudoOpsBasisTyp.pseudo_op

  val toString : pseudo_op -> string
  val emitValue : {pOp: pseudo_op, loc: int, emit: Word8.word -> unit} -> unit
    (* identical to that in pseudo-ops-basis.sig *)

  val sizeOf : pseudo_op * int -> int
    (* identical to that in pseudo-ops-basis.sig *)

  val adjustLabels : pseudo_op * int -> bool
    (* adjust the value of labels in the pseudo_op given the current
     * location counter.
     *)
  

end

