(* amd64-gen-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Glues together the AMD64-specific code with the code generator.  Also
 * specializes register spilling.
 *)

functor AMD64GenFn (structure Spec : TARGET_SPEC) =
  struct

  (* MLRISC CFG visualization *)
    val mlriscCFGVis : bool Controls.control = Controls.genControl {
           name = "mlriscCFGVis",
	   pri  = [5, 0],   (* what is this? *)
	   obscurity = 0,
	   help = "generate dot files from the MLRISC CFG",
	   default = false
        }

    val () = ControlRegistry.register CodegenControls.registry {
	    ctl = Controls.stringControl ControlUtil.Cvt.bool mlriscCFGVis,
	    envName = NONE
	  };

    structure AMD64Frame = AMD64FrameFn (structure Spec = Spec)
    structure AMD64Constant = AMD64ConstantFn (structure AMD64Frame=AMD64Frame)
  
    structure AMD64MLT = AMD64MLTreeFn (structure AMD64Constant = AMD64Constant)
    open AMD64MLT
  
    structure C = AMD64Cells
    structure I = AMD64Instr
  
    structure AMD64PseudoOps = ManticorePseudoOpsFn (
      structure P=AMD64GasPseudoOps
      structure Spec = Spec)
  
    structure AMD64Stream = InstructionStream (AMD64PseudoOps.PseudoOps)
  
    structure AMD64AsmEmit = AMD64AsmEmitter (
      structure Instr = AMD64Instr
      structure S = AMD64Stream
      structure MLTreeEval = AMD64MLTreeEval
      structure Shuffle = AMD64Shuffle)
  
    structure AMD64CFG = ControlFlowGraph (
      structure I = AMD64AsmEmit.I
      structure GraphImpl = DirectedGraph
      structure InsnProps = AMD64Props
      structure Asm = AMD64AsmEmit)
  
    structure AMD64MLTStream = MLTreeStream (
      structure T = AMD64MLTree
      structure S = AMD64Stream )
  
    structure AMD64FlowGraph = BuildFlowgraph (
      structure Props = AMD64Props
      structure Stream = AMD64Stream
      structure CFG = AMD64CFG)
  
    structure MLTreeExtComp = AMD64CompExtFn (
      structure CFG = AMD64CFG
      structure TS = AMD64MLTStream
      structure I = AMD64Instr)

  (* literals that MLRISC introduces during instruction selection *)
    val literals : (Label.label * AMD64PseudoOps.PseudoOps.pseudo_op) list ref = ref []

    (* we negate a float by flipping its high bit with an xor instruction.  doing it
     * this way requires us to emit a constant value with this high bit set.
     * we store this constant at negateLabel.
     *)
    local
	structure PTy = PseudoOpsBasisTyp
	val negateLabel : Label.label option ref = ref NONE
    in
    (* callback to negate a float or double *)
    fun floatNegate ty = (case !negateLabel
			   of NONE => let
	      val l = Label.label "negateLitLabel" ()
	      val mask = Word64.toLargeInt (Word64.<< (0w1, Word.fromInt (ty-1)))
	      val pOp = PTy.INT {sz=ty, i=[AMD64GasPseudoOps.T.LI mask]}
	      in
		  literals := (l, pOp) :: !literals;
		  negateLabel := SOME l;
		  l
	      end
	    | SOME l => l
	   (* end case *)) 
    end (* local *)
     
    structure AMD64MLTreeComp = AMD64Gen (
      structure I = AMD64Instr
      structure MLTreeUtils = AMD64MLTreeUtils
      structure MLTreeStream = AMD64MLTStream
      structure ExtensionComp = MLTreeExtComp
      val floatNegate = floatNegate
     )

    structure AMD64SpillLoc = SpillLocFn (structure Frame=AMD64Frame)
    structure BlockPlacement = DefaultBlockPlacement (AMD64CFG)

    structure JumpChainElim = JumpChainElimFn (
	       structure CFG = AMD64CFG
	       structure InsnProps = AMD64Props
	       val chainEscapes = ref false
	       val reverseDirection = ref false)
  
    structure AMD64Shuffle = AMD64Shuffle(AMD64Instr)
  
    (* transformation to expand COPY and FCOPYs *)
    structure AMD64Expand = CFGExpandCopies (
       structure CFG = AMD64CFG
       structure Shuffle = AMD64Shuffle)
  
    (* AMD64 peephole optimization *)
  (*  structure AMD64PeepholeOpt = CFGPeephole(
       structure CFG = AMD64CFG
       structure PeepHole = AMD64Peephole(
       structure Instr = I
       structure Eval = AMD64MLTreeEval)) *)
  
    (* a function to get the frame annotation *)
    fun getFrameAn annotations = 
	(case #get AMD64SpillLoc.frameAn annotations
	  of SOME frame => frame
	   | NONE => raise Fail "unable to get frame annotation"
	(* end case *))
  
    structure Emit = CFGEmit (
	structure CFG = AMD64CFG
	structure E = AMD64AsmEmit)
				 
    local
      datatype raPhase = SPILL_PROPAGATION | SPILL_COLORING
      datatype spillOperandKind = SPILL_LOC | CONST_VAL
      structure RASpill = RASpillWithRenaming (
	  structure Asm = AMD64AsmEmit
	  structure InsnProps = AMD64Props
	  val max_dist = ref 4
	  val keep_multiple_values = ref false)
  
      fun regLoc recordSpill (frame, loc) = let
	    val fsi = AMD64SpillLoc.frameSzInfo frame
	    val spillLoc = recordSpill (fsi, loc)
	    in
	      I.Displace {
		  base = valOf AMD64Regs.fpReg, 
		  disp = I.ImmedLabel (AMD64MLTree.CONST (AMD64Constant.StackLoc {
		      frame = fsi,
		      loc   = spillLoc
		    })),
		  mem = ()
		}
	    end
      val gprLoc  = regLoc AMD64Frame.recordSpill
      val fprLoc  = regLoc AMD64Frame.recordFSpill
		    
      structure IntRA =
	struct
	  val dedicated = AMD64Regs.dedicatedRegs
	  val avail = AMD64Regs.availRegs
	  val phases = [SPILL_PROPAGATION,SPILL_COLORING]
	  fun spillInit _ = ()
	  fun spillLoc {info=frame, an, cell, id=loc} =
	      {opnd = gprLoc (frame, loc), kind = SPILL_LOC}
	end (* IntRA *)
      structure FloatRA =
	struct
	  val avail     = AMD64Regs.availFRegs
	  val dedicated = AMD64Regs.dedicatedFRegs (* empty *)
	  val phases    = [SPILL_PROPAGATION, SPILL_COLORING]
	  fun spillInit _ = ()
	  fun spillLoc (frame, an, loc) = fprLoc (frame, loc)
	end (* FloatRA *)
    in
    structure RA = AMD64RegAlloc (
	structure I = AMD64Instr
	structure Props = AMD64Props
	structure CFG = AMD64CFG
	structure Asm = AMD64AsmEmit
	structure SpillHeur = ChowHennessySpillHeur
	structure Spill = RASpill
	datatype ra_phase = datatype raPhase
	datatype spill_operand_kind = datatype spillOperandKind
	type spill_info = AMD64SpillLoc.frame
	fun beforeRA (Graph.GRAPH graph) = 
	    let val CFG.INFO{annotations, ...} = #graph_info graph
	    in
		getFrameAn (!annotations)
	    end
	structure Int = IntRA
	structure Float = FloatRA)
    end (* local *)

    structure GraphViewer = GraphViewer (Dot)
    structure CFGViewer = CFGViewer (
                structure CFG = AMD64CFG
		structure GraphViewer = GraphViewer
		structure Asm = AMD64AsmEmit)
  
    structure BackEnd : BACK_END =
      struct
	structure Spec = Spec
	structure ManticorePseudoOps = AMD64PseudoOps
	structure MLTreeComp = AMD64MLTreeComp
	structure MLTreeUtils = AMD64MLTreeUtils
	structure CFGGen = AMD64FlowGraph
	structure MTy = MLRiscTypesFn (
	    structure Spec = Spec
	    structure T = AMD64MLTree ) 
	structure LabelCode = LabelCodeFn (
	    structure MTy = MTy )
	structure SpillLoc = AMD64SpillLoc
	structure Regs = AMD64Regs
	structure Types = AMD64TypesFn (
	    structure Spec = Spec )
	structure AtomicOps = AMD64AtomicOpsFn(
	    structure MTy = MTy
	    structure I = AMD64Instr)
	structure Copy = AMD64CopyFn (
	    structure MTy = MTy
	    structure Spec = Spec
	    structure MLTreeUtils = AMD64MLTreeUtils
	    structure Cells = MLTreeComp.I.C )
	structure VarDef = VarDefFn ( 
	    structure MTy = MTy
	    structure Spec = Spec
	    structure MLTreeComp = AMD64MLTreeComp )
	structure AMD64HeapTarget = AMD64HeapTransferFn (
	    structure AMD64MLTree = AMD64MLTree
	    structure Spec = Spec
	    structure SpillLoc = SpillLoc )
	structure VProcOps = VProcOpsFn (
	    structure MTy = MTy
	    structure VarDef = VarDef
	    structure Regs = Regs
	    structure Spec = Spec
	    structure Types = Types
	    structure MLTreeComp = AMD64MLTreeComp )
	structure Alloc = Alloc64Fn (
	    structure MTy = MTy
	    structure Regs = Regs
	    structure Spec = Spec 
	    structure MLTreeComp = AMD64MLTreeComp
	    structure Types = Types
            structure VProcOps=VProcOps)
	structure Transfer = HeapTransferFn (
	    structure MTy =MTy
	    structure VarDef = VarDef
	    structure SpillLoc = SpillLoc
	    structure Copy = Copy
	    structure Regs = Regs 
	    structure Target = AMD64HeapTarget 
	    structure Alloc = Alloc
	    structure MLTreeComp = AMD64MLTreeComp
	    structure Spec = Spec 
	    structure LabelCode = LabelCode
	    structure Frame = AMD64Frame
	    structure CCall = AMD64SVID (structure T=AMD64MLTree val frameAlign = 16)
	    structure Types = Types
	    structure VProcOps = VProcOps )
	val literals = literals
      
	  fun compileCFG (cfg as Graph.GRAPH graph) = 
	      let val CFGGen.CFG.INFO{annotations, ...} = #graph_info graph
	      in 
		  case (#get AMD64SpillLoc.frameAn) (!annotations)
		   of NONE => Emit.asmEmit (cfg, #nodes graph ())
		    | SOME frame => 
		      let val cfg = RA.run cfg
			  val cfg = AMD64Expand.run cfg
			  (*val cfg = AMD64PeepholeOpt.run cfg*)
			  val (cfg, blocks) = BlockPlacement.blockPlacement cfg
			  val (cfg, blocks) = JumpChainElim.run (cfg, blocks)
		      in
			  if Controls.get mlriscCFGVis
			     then CFGViewer.view cfg
			     else ();
			  Emit.asmEmit (cfg, blocks)
		      end 
	      end (* compileCFG *)
      end (* BackEnd *)
  
    structure Gen = CodeGenFn (BackEnd)
  
  end (* AMD64CG *)
