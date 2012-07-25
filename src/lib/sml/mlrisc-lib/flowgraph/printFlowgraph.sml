(* printFlowgraph.sml -- print flowgraph of target machine instructions. 
 *
 * Copyright (c) 1997 Bell Laboratories.
 *)
signature PRINT_FLOWGRAPH = 
sig
   structure Asm : INSTRUCTION_EMITTER
   structure CFG : CONTROL_FLOW_GRAPH (* where I = Asm.I and P = Asm.S.P *)
                   where type I.addressing_mode = Asm.I.addressing_mode
                     and type I.ea = Asm.I.ea
                     and type I.instr = Asm.I.instr
                     and type I.instruction = Asm.I.instruction
                     and type I.operand = Asm.I.operand
                   where type P.Client.pseudo_op = Asm.S.P.Client.pseudo_op
                     and type P.T.Basis.cond = Asm.S.P.T.Basis.cond
                     and type P.T.Basis.div_rounding_mode = Asm.S.P.T.Basis.div_rounding_mode
                     and type P.T.Basis.ext = Asm.S.P.T.Basis.ext
                     and type P.T.Basis.fcond = Asm.S.P.T.Basis.fcond
                     and type P.T.Basis.rounding_mode = Asm.S.P.T.Basis.rounding_mode
                     and type P.T.Constant.const = Asm.S.P.T.Constant.const
                     and type ('s,'r,'f,'c) P.T.Extension.ccx = ('s,'r,'f,'c) Asm.S.P.T.Extension.ccx
                     and type ('s,'r,'f,'c) P.T.Extension.fx = ('s,'r,'f,'c) Asm.S.P.T.Extension.fx
                     and type ('s,'r,'f,'c) P.T.Extension.rx = ('s,'r,'f,'c) Asm.S.P.T.Extension.rx
                     and type ('s,'r,'f,'c) P.T.Extension.sx = ('s,'r,'f,'c) Asm.S.P.T.Extension.sx
                     and type P.T.I.div_rounding_mode = Asm.S.P.T.I.div_rounding_mode
                     and type P.T.Region.region = Asm.S.P.T.Region.region
                     and type P.T.ccexp = Asm.S.P.T.ccexp
                     and type P.T.fexp = Asm.S.P.T.fexp
                     (* and type P.T.labexp = Asm.S.P.T.labexp *)
                     and type P.T.mlrisc = Asm.S.P.T.mlrisc
                     and type P.T.oper = Asm.S.P.T.oper
                     and type P.T.rep = Asm.S.P.T.rep
                     and type P.T.rexp = Asm.S.P.T.rexp
                     and type P.T.stm = Asm.S.P.T.stm

   val printCFG : TextIO.outstream -> string -> CFG.cfg -> unit
end


functor PrintFlowgraph 
   (structure Asm : INSTRUCTION_EMITTER
    structure CFG : CONTROL_FLOW_GRAPH (* where I = Asm.I and P = Asm.S.P *)
                    where type I.addressing_mode = Asm.I.addressing_mode
                      and type I.ea = Asm.I.ea
                      and type I.instr = Asm.I.instr
                      and type I.instruction = Asm.I.instruction
                      and type I.operand = Asm.I.operand
                    where type P.Client.pseudo_op = Asm.S.P.Client.pseudo_op
                      and type P.T.Basis.cond = Asm.S.P.T.Basis.cond
                      and type P.T.Basis.div_rounding_mode = Asm.S.P.T.Basis.div_rounding_mode
                      and type P.T.Basis.ext = Asm.S.P.T.Basis.ext
                      and type P.T.Basis.fcond = Asm.S.P.T.Basis.fcond
                      and type P.T.Basis.rounding_mode = Asm.S.P.T.Basis.rounding_mode
                      and type P.T.Constant.const = Asm.S.P.T.Constant.const
                      and type ('s,'r,'f,'c) P.T.Extension.ccx = ('s,'r,'f,'c) Asm.S.P.T.Extension.ccx
                      and type ('s,'r,'f,'c) P.T.Extension.fx = ('s,'r,'f,'c) Asm.S.P.T.Extension.fx
                      and type ('s,'r,'f,'c) P.T.Extension.rx = ('s,'r,'f,'c) Asm.S.P.T.Extension.rx
                      and type ('s,'r,'f,'c) P.T.Extension.sx = ('s,'r,'f,'c) Asm.S.P.T.Extension.sx
                      and type P.T.I.div_rounding_mode = Asm.S.P.T.I.div_rounding_mode
                      and type P.T.Region.region = Asm.S.P.T.Region.region
                      and type P.T.ccexp = Asm.S.P.T.ccexp
                      and type P.T.fexp = Asm.S.P.T.fexp
                      (* and type P.T.labexp = Asm.S.P.T.labexp *)
                      and type P.T.mlrisc = Asm.S.P.T.mlrisc
                      and type P.T.oper = Asm.S.P.T.oper
                      and type P.T.rep = Asm.S.P.T.rep
                      and type P.T.rexp = Asm.S.P.T.rexp
                      and type P.T.stm = Asm.S.P.T.stm
   ) : PRINT_FLOWGRAPH =
struct
   structure Asm = Asm
   structure CFG = CFG
   structure C   = CFG.I.C
   structure Fmt = Format

   val i2s = Int.toString

   fun printList stream list = let
     fun pr str = TextIO.output(stream, str)
     fun iter [] = ()
       | iter [i] = pr i
       | iter (h::t) = (pr (h ^ ", "); iter t)
   in iter list
   end

   fun printCFG stream title (Cfg as Graph.GRAPH cfg) = 
   let fun pr str = TextIO.output(stream, str)
       val prList = printList stream
       val annotations = !(CFG.annotations Cfg)
       val Asm.S.STREAM{emit,pseudoOp,defineLabel,annotation,...} = 
             AsmStream.withStream stream Asm.makeStream annotations

       fun showFreq(ref w) = Fmt.format "[%f]" [Fmt.REAL w] 
       fun showEdge(blknum,e) = 
	   Fmt.format "%d:%s" [Fmt.INT blknum, Fmt.STR(CFG.show_edge e)]
       fun showSucc(_, x, e) = showEdge(x,e)
       fun showPred(x, _, e) = showEdge(x,e) 
       fun showSuccs b =
            (pr "\tsucc:     "; 
             prList (map showSucc (#out_edges cfg b)); 
             pr "\n")
       fun showPreds b =
            (pr "\tpred:     "; 
             prList (map showPred (#in_edges cfg b)); 
             pr "\n")

       fun printBlock(_, CFG.BLOCK{kind=CFG.START, id, freq, ...}) = 
           (pr (Fmt.format "ENTRY %d %s\n" [Fmt.INT id, Fmt.STR(showFreq freq)]);
            showSuccs id)
         | printBlock(_, CFG.BLOCK{kind=CFG.STOP, id, freq, ...}) = 
	   (pr (Fmt.format "EXIT %d %s\n" [Fmt.INT id, Fmt.STR(showFreq freq)]);
            showPreds id)
         | printBlock(_, CFG.BLOCK{id, align, freq, insns, annotations, 
                               labels, ...}) = 
	   (pr (Fmt.format "BLOCK %d %s\n" [Fmt.INT id, Fmt.STR(showFreq freq)]);
	    case !align of NONE => () | SOME p => (pr (CFG.P.toString p ^ "\n"));
            app annotation (!annotations);
            app defineLabel (!labels);
            (*
               pr ("\tlive in:  " ^ CellsBasis.CellSet.toString (!liveIn) ^ "\n");
               pr ("\tlive out: " ^ CellsBasis.CellSet.toString (!liveOut) ^ "\n");
             *)
            showSuccs id;
            showPreds id;
            app emit (rev (!insns)))

       fun printData() = let
         val CFG.INFO{data, ...} = #graph_info cfg
       in List.app (pr o CFG.P.toString) (rev(!data))
       end
   in
       pr(Fmt.format "[ %s ]\n" [Fmt.STR title]);
       app annotation annotations;
       (* printBlock entry; *)
       AsmStream.withStream stream (#forall_nodes cfg) printBlock;
       (* printBlock exit; *)
       AsmStream.withStream stream printData ();
       TextIO.flushOut stream
   end
end

