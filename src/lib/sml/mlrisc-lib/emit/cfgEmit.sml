(* cfgEmit.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * This module takes a flowgraph and an assembly emitter module and 
 * ties them together into one.  The output is sent to AsmStream.
 *  --Allen
 *
 * TODO: Need to check for the REORDER/NOREORDER annotation on
 * blocks and call P.Client.AsmPseudoOps.toString function to
 * print out the appropriate assembler directive. -- Lal.
 *)

functor CFGEmit
  (structure E   : INSTRUCTION_EMITTER
   structure CFG : CONTROL_FLOW_GRAPH (* where I = E.I and P = E.S.P *)
                   where type I.addressing_mode = E.I.addressing_mode
                     and type I.ea = E.I.ea
                     and type I.instr = E.I.instr
                     and type I.instruction = E.I.instruction
                     and type I.operand = E.I.operand
                   where type P.Client.pseudo_op = E.S.P.Client.pseudo_op
                     and type P.T.Basis.cond = E.S.P.T.Basis.cond
                     and type P.T.Basis.div_rounding_mode = E.S.P.T.Basis.div_rounding_mode
                     and type P.T.Basis.ext = E.S.P.T.Basis.ext
                     and type P.T.Basis.fcond = E.S.P.T.Basis.fcond
                     and type P.T.Basis.rounding_mode = E.S.P.T.Basis.rounding_mode
                     and type P.T.Constant.const = E.S.P.T.Constant.const
                     and type ('s,'r,'f,'c) P.T.Extension.ccx = ('s,'r,'f,'c) E.S.P.T.Extension.ccx
                     and type ('s,'r,'f,'c) P.T.Extension.fx = ('s,'r,'f,'c) E.S.P.T.Extension.fx
                     and type ('s,'r,'f,'c) P.T.Extension.rx = ('s,'r,'f,'c) E.S.P.T.Extension.rx
                     and type ('s,'r,'f,'c) P.T.Extension.sx = ('s,'r,'f,'c) E.S.P.T.Extension.sx
                     and type P.T.I.div_rounding_mode = E.S.P.T.I.div_rounding_mode
                     and type P.T.Region.region = E.S.P.T.Region.region
                     and type P.T.ccexp = E.S.P.T.ccexp
                     and type P.T.fexp = E.S.P.T.fexp
                     (* and type P.T.labexp = E.S.P.T.labexp *)
                     and type P.T.mlrisc = E.S.P.T.mlrisc
                     and type P.T.oper = E.S.P.T.oper
                     and type P.T.rep = E.S.P.T.rep
                     and type P.T.rexp = E.S.P.T.rexp
                     and type P.T.stm = E.S.P.T.stm
  )  : ASSEMBLY_EMITTER = 
struct
  structure CFG = CFG

  fun asmEmit (Graph.GRAPH graph, blocks) = let
	val CFG.INFO{annotations=an, data, decls, ...} = #graph_info graph
	val E.S.STREAM{pseudoOp,defineLabel,emit,annotation,comment,...} = 
             E.makeStream (!an)
	fun emitIt (id, CFG.BLOCK{labels, annotations=a, align, insns, ...}) = (
              case !align of NONE => () | SOME p => (pseudoOp p);
	      List.app defineLabel (!labels); 
	      List.app emitAn (!a);
	      List.app emit (rev (!insns)))
	and emitAn a = if Annotations.toString a = "" then () else annotation(a)
	in
	  List.app emitAn (!an);
	  List.app pseudoOp (rev (!decls));
	  pseudoOp(PseudoOpsBasisTyp.TEXT);
	  List.app emitIt blocks;
	  List.app pseudoOp (rev (!data))
	  
	end
end










