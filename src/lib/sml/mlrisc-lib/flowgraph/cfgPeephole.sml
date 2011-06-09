(*
 * Run peephole optimization on a cluster
 *)
functor CFGPeephole
  (structure CFG      : CONTROL_FLOW_GRAPH
   structure PeepHole : PEEPHOLE (* sharing CFG.I = PeepHole.I *)
                        where type I.addressing_mode = CFG.I.addressing_mode
                          and type I.ea = CFG.I.ea
                          and type I.instr = CFG.I.instr
                          and type I.instruction = CFG.I.instruction
                          and type I.operand = CFG.I.operand
  ) : CFG_OPTIMIZATION =
struct
   structure CFG = CFG

   val name = "Peephole optimization"

   fun run (cfg as Graph.GRAPH graph) = let
         fun opt (_, CFG.BLOCK{insns, ...}) = insns := PeepHole.peephole(rev(!insns))
	 in
	   #forall_nodes graph opt;
	   cfg
	 end

end
