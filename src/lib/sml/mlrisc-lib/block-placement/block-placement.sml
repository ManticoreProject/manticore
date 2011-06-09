(* block-placement.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *)

functor BlockPlacement 
   (structure CFG : CONTROL_FLOW_GRAPH
    structure Props : INSN_PROPERTIES (* where I = CFG.I *)
                      where type I.addressing_mode = CFG.I.addressing_mode
                        and type I.ea = CFG.I.ea
                        and type I.instr = CFG.I.instr
                        and type I.instruction = CFG.I.instruction
                        and type I.operand = CFG.I.operand
   ) : BLOCK_PLACEMENT =

struct
  structure CFG = CFG

  structure DefaultPlacement = DefaultBlockPlacement(CFG)

  structure WeightedPlacement = 
     WeightedBlockPlacementFn
	  (structure CFG = CFG 
	   structure InsnProps = Props)

  val placementFlag = MLRiscControl.mkFlag
			  ("weighted-block-placement",
			   "whether MLRISC does weighted block placement")

  fun blockPlacement(cfg as Graph.GRAPH G) =
	if !placementFlag
	  then WeightedPlacement.blockPlacement cfg
	  else DefaultPlacement.blockPlacement cfg

end
