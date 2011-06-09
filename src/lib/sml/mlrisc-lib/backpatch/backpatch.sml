(* bbsched2.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(** bbsched2.sml - invoke scheduling after span dependent resolution **)

functor BBSched2
    (structure Emitter : INSTRUCTION_EMITTER
     structure CFG     : CONTROL_FLOW_GRAPH (* where I = Emitter.I and P = Emitter.S.P *)
                         where type I.addressing_mode = Emitter.I.addressing_mode
                           and type I.ea = Emitter.I.ea
                           and type I.instr = Emitter.I.instr
                           and type I.instruction = Emitter.I.instruction
                           and type I.operand = Emitter.I.operand
                         where type P.Client.pseudo_op = Emitter.S.P.Client.pseudo_op
                           and type P.T.Basis.cond = Emitter.S.P.T.Basis.cond
                           and type P.T.Basis.div_rounding_mode = Emitter.S.P.T.Basis.div_rounding_mode
                           and type P.T.Basis.ext = Emitter.S.P.T.Basis.ext
                           and type P.T.Basis.fcond = Emitter.S.P.T.Basis.fcond
                           and type P.T.Basis.rounding_mode = Emitter.S.P.T.Basis.rounding_mode
                           and type P.T.Constant.const = Emitter.S.P.T.Constant.const
                           and type ('s,'r,'f,'c) P.T.Extension.ccx = ('s,'r,'f,'c) Emitter.S.P.T.Extension.ccx
                           and type ('s,'r,'f,'c) P.T.Extension.fx = ('s,'r,'f,'c) Emitter.S.P.T.Extension.fx
                           and type ('s,'r,'f,'c) P.T.Extension.rx = ('s,'r,'f,'c) Emitter.S.P.T.Extension.rx
                           and type ('s,'r,'f,'c) P.T.Extension.sx = ('s,'r,'f,'c) Emitter.S.P.T.Extension.sx
                           and type P.T.I.div_rounding_mode = Emitter.S.P.T.I.div_rounding_mode
                           and type P.T.Region.region = Emitter.S.P.T.Region.region
                           and type P.T.ccexp = Emitter.S.P.T.ccexp
                           and type P.T.fexp = Emitter.S.P.T.fexp
                           (* and type P.T.labexp = Emitter.S.P.T.labexp *)
                           and type P.T.mlrisc = Emitter.S.P.T.mlrisc
                           and type P.T.oper = Emitter.S.P.T.oper
                           and type P.T.rep = Emitter.S.P.T.rep
                           and type P.T.rexp = Emitter.S.P.T.rexp
                           and type P.T.stm = Emitter.S.P.T.stm
     structure Jumps   : SDI_JUMPS (* where I = CFG.I *)
                         where type I.addressing_mode = CFG.I.addressing_mode
                           and type I.ea = CFG.I.ea
                           and type I.instr = CFG.I.instr
                           and type I.instruction = CFG.I.instruction
                           and type I.operand = CFG.I.operand
     structure Props   : INSN_PROPERTIES (* where I = CFG.I *)
                         where type I.addressing_mode = CFG.I.addressing_mode
                           and type I.ea = CFG.I.ea
                           and type I.instr = CFG.I.instr
                           and type I.instruction = CFG.I.instruction
                           and type I.operand = CFG.I.operand
    ) = 
struct

  structure CFG = CFG
  structure G = Graph
  structure I = CFG.I
  structure C = I.C
  structure E = Emitter
  structure J = Jumps
  structure P = CFG.P

  fun error msg = MLRiscErrorMsg.error("BBSched",msg)

  datatype code =
      SDI of {size : int ref,		(* variable sized *)
	      insn : I.instruction}
    | FIXED of {size: int,		(* size of fixed instructions *)
		insns: I.instruction list}
   
  datatype compressed = 
      PSEUDO of P.pseudo_op
    | LABEL  of Label.label
    | CODE of  code list

  datatype cluster = CLUSTER of compressed list

  val clusterList : cluster list ref = ref []
  val dataList : P.pseudo_op list ref = ref []
  fun cleanUp() = (clusterList := []; dataList := [])

  fun bbsched(G.GRAPH{graph_info=CFG.INFO{data, ...}, ...}, blocks) = let
   fun compress [] = []
      | compress((_, CFG.BLOCK{align, labels, insns, ...}) :: rest) = let
          fun alignIt(chunks) = 
	    (case !align of NONE => chunks | SOME p => PSEUDO(p)::chunks)

	  fun mkCode(0, [], [], code) = code
	    | mkCode(size, insns, [], code) = FIXED{size=size, insns=insns}:: code
	    | mkCode(size, insns, instr::instrs, code) = let
		val s = J.minSize instr
	      in
		if J.isSdi instr then let
		    val sdi = SDI{size=ref s, insn=instr}
		  in
		    if size = 0 then 
		      mkCode(0, [], instrs, sdi::code)
		    else 
		      mkCode(0, [], instrs, 
			     sdi::FIXED{size=size, insns=insns}::code)
		  end
		else mkCode(size+s, instr::insns, instrs, code)
	      end
	in
	  alignIt
	    (map LABEL (!labels) @ 
	       CODE(mkCode(0, [], !insns, [])) :: compress rest)
	end
  in
    clusterList:=CLUSTER(compress blocks):: (!clusterList);
    dataList := !data @ !dataList
  end



  fun finish() = let
    fun labels(PSEUDO pOp::rest, pos, chgd) = 
          (P.adjustLabels(pOp, pos); labels(rest, pos+P.sizeOf(pOp,pos), chgd))
      | labels(LABEL lab::rest, pos, chgd) = 
	 if Label.addrOf(lab) = pos then labels(rest, pos, chgd)
	 else (Label.setAddr(lab, pos); labels(rest, pos, true))
      | labels(CODE code::rest, pos, chgd) = let
 	  fun doCode(FIXED{size, ...}::rest, pos, changed) = 
	        doCode(rest, pos+size, changed)
	    | doCode(SDI{size, insn}::rest, pos, changed) = let
	  	val newSize = J.sdiSize(insn, Label.addrOf, pos)
 	      in
		  if newSize <= !size then doCode(rest, !size + pos, changed)
		  else (size:=newSize; doCode(rest, newSize+pos, true))
	       end
	    | doCode([], pos, changed) = labels(rest, pos, changed)
        in doCode(code, pos, chgd)
	end
      | labels([], pos,chgd) = (pos, chgd)

    fun clusterLabels clusters = let
      fun f (CLUSTER cl, (pos, chgd)) = labels(cl, pos, chgd)
    in List.foldl f (0, false) clusters
    end

    fun fixpoint zl = let 
      val (size, changed) = clusterLabels zl
    in if changed then fixpoint zl else size
    end

    val Emitter.S.STREAM{emit,defineLabel,beginCluster,pseudoOp,...} = 
            Emitter.makeStream []

    fun emitCluster(CLUSTER(comp),loc) = let
	  fun process(PSEUDO pOp,loc) = (pseudoOp pOp; loc + P.sizeOf(pOp,loc))
	    | process(LABEL lab,loc) = (defineLabel lab; loc)
	    | process(CODE code,loc) = let
		fun emitInstrs insns = app emit insns
		fun e(FIXED{insns, size,...},loc) = (emitInstrs insns; loc+size)
		  | e(SDI{size, insn},loc) = 
		       (emitInstrs(J.expand(insn, !size, loc)); !size + loc)
	      in foldl e loc code
	      end
	in foldl process loc comp
	end

    fun initLabels(clusters) = let
      fun init(PSEUDO(p)::rest, loc) = 
	   (P.adjustLabels(p, loc); init(rest, loc + P.sizeOf(p, loc)))
	| init(LABEL lab::rest, loc) = (Label.setAddr(lab,loc); init(rest, loc))
	| init(CODE code::rest, loc) = let
 	   fun size(FIXED{size, ...}) = size
	     | size(SDI{size, ...}) = !size
          in
	    init(rest, List.foldl (fn (c, b) => size(c) + b) loc code)
          end
        | init([], loc) = loc

    in
      List.foldl 
        (fn (CLUSTER(cl), loc) => init(cl, loc)) 0 clusters
    end

    (* The dataList is in reverse order and the clusters are in reverse *)
    fun dataCluster([], acc) = CLUSTER(acc)
      | dataCluster(d::dl, acc) = dataCluster(dl, PSEUDO d::acc)
    val compressed = 
      rev (dataCluster(!dataList, []) :: !clusterList) before cleanUp()
  in
    initLabels(compressed);
    beginCluster(fixpoint (compressed));
    foldl emitCluster 0 compressed; 
    ()
  end (*finish*)
end (* bbsched2 *)


