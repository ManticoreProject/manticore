(* add-alloc-checks-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * We place allocation checks by building a graph with a node for
 * every extended block and edges representing control flow.  We
 * then compute the feedback vertex set for this graph.  We attach
 * heap checks to those blocks that are in the feedback vertex set.
 *
 * TODO:
 *	Need to get escaping functions/continuations right (in CFA?)
 *	free variables for gc/nogc jumps
 *)

functor AddAllocChecksFn (Target : TARGET_SPEC) : sig

    val transform : CFG.module -> CFG.module

  end = struct

    structure CFA = CFACFG
    structure U = CFGUtil

    structure Vertex = struct
	type vertex = CFG.label
	val compare = CFG.Label.compare
      end

    structure FB = FeedbackFn (Vertex)
    structure VSet = FB.Set
    structure ABI = Target.ABI

    val wordSzB = Word.fromLargeInt ABI.wordSzB

  (* block allocation annotation for a block *)
    val {clrFn=clrBlkAlloc, getFn=getBlkAlloc, setFn=setBlkAlloc, ...} =
	  CFG.Label.newProp (fn _ => 0w0)

  (* transitive allocation annotation for blocks *)
    val {clrFn=clrAlloc, getFn=getAlloc, peekFn=peekAlloc, setFn=setAlloc} =
	  CFG.Label.newProp (fn _ => 0w0)

  (* the amount of storage allocated by an expression *)
    fun expAlloc (CFG.E_Alloc(_, _, xs)) = wordSzB * Word.fromInt(length xs + 1)
      | expAlloc _ = 0w0

  (* annotate a block with the amount of allocation that it does *)
    fun annotateBlock (CFG.BLK{lab, body, ...}) =
	  setBlkAlloc(lab, List.foldl (fn (e, sz) => sz + expAlloc e) 0w0 body)

  (* if a block does a lot of allocation, then split it into blocks that
   * do not allocate more than the process a block from back to front.  We process the expressions in
   * the block in reverse order so that we can track live variables.
   *)
    local
      structure VSet = CFG.Var.Set
      structure VMap = CFG.Var.Map
      val allocSlopSzB = Word.fromLargeInt ABI.nurseryAllocSlopSzb
    in
    fun splitLargeBock (blk as CFG.BLK{lab, args, body, exit}) = let
	(* update the live set by adding the rhs variables and removing the lhs.
	 * Note that we track the live variables using their original names,
	 * not the fresh names that we introduce at heap checks!
	 *)
	  fun updateLive (live, exp) =
		VSet.addList(VSet.subtractList(live, U.lhsOfExp exp), U.rhsOfExp exp)
	  fun rename subst x = (case VMap.find(subst, x)
		 of NONE => x
		  | SOME x' => x'
		(* end case *))
	  fun walk ([], subst, _, _, exit', exps', blks') =
		CFG.mkBlock(lab, List.map (rename subst) args, exps', exit') :: blks'
	    | walk (exp::exps, subst, live, allocSzB, exit', exps', blks') = let
		fun continue allocSzB = let
		    (* update the live set *)
		      val live' = updateLive (live, exp)
		    (* rewrite the expression *)
		      val exp' = U.substExp subst exp
		      in
			walk (exps, subst, live', allocSzB, exit', exp' :: exps', blks')
		      end
		in
		  case expAlloc exp
		   of 0w0 => continue allocSzB
		    | nb => if (allocSzB + nb > allocSlopSzB)
			then let
			(* need to handle the case where allocSzB = 0 and nb > allocSlopSzB
			 * so that we don't get into an infinite loop!
			 *)
			  val (exps, live, exps') = if (nb > allocSlopSzB)
				then (exps, updateLive (live, exp), U.substExp subst exp)
				else (exp::exps, live, exps')
			  val live = VSet.listItems live
			  val live' = List.map CFG.Var.copy live
			(* compute the renaming substitution for the next block *)
			  val subst' = ListPair.foldl
				(fn (x, x', subst) => VMap.insert(subst, x, x'))
				  VMap.empty
				    (live, live')
			(* make the block for the expressions upto this point *)
			  val lab' = CFG.Label.new(
				  "split",
				  CFGTy.T_Block{args = List.map CFG.Var.typeOf live})
			  val block = CFG.mkBlock(lab', List.map (rename subst) live, exps', exit')
			(* make the heap check that jumps to the new block *)
			  val exit'' = CFG.HeapCheck{
				  hck = CFG.HCK_Local, szb = allocSzB, nogc = (lab', live')
				}
			  in
			    walk (exps, subst', VSet.fromList live', 0w0, exit'', [], block::blks')
			  end
			else continue (allocSzB + nb)
		  (* end case *)
		end
	  in
	    if (getBlkAlloc lab > allocSlopSzB)
	      then walk (
		List.rev body,
		VMap.empty,				(* renaming substitution *)
		VSet.fromList(U.varsOfXfer exit),	(* live *)
		0w0,					(* cumulative allocation *)
		exit,					(* exit for current block *)
		[],					(* expressions in current block *)
		[])					(* blocks that have been created *)
	      else [blk] (* no rewrite required *)
	  end
    end (* local *)

  (* construct the flow graph for a module *)
    fun makeGraph code = let
	(* return the outgoing targets of a function *)
	  fun nodeFromBlock (CFG.BLK{lab, exit, ...}) = (case CFA.labelsOf exit
                 of NONE => (lab, [])
                  | SOME ls => (lab, CFG.Label.Set.listItems ls)
                (* end case *))
          val nodeFromBlock = fn b => if Controls.get CFGOptControls.debug
                then let
		  val nd as (src, out) = nodeFromBlock b
		  val src = CFG.Label.toString src
		  val out = String.concatWith "," (List.map CFG.Label.toString out)
		  in
		    print(concat["  ", src, " -> [", out, "]\n"]);
		    nd
		  end
		else nodeFromBlock b
          fun toNode (CFG.FUNC{start,body,...}) =
		List.foldr (fn (b,rr) => (nodeFromBlock b)::rr) [] (start::body)
	  in
            if Controls.get CFGOptControls.debug
	      then print "makeGraph\n"
              else ();
	    List.foldr (fn (node,rr) => (toNode node)@rr) [] code
	  end

    fun transform (CFG.MODULE{name, externs, code}) = let
	  val graph = makeGraph code
	  val fbSet = FB.feedback graph
        (* add allocation checks as needed to a function *)
	  fun addAllocChecks hcKind = let
                val checkLabel = "Check"
	      (* compute the allocation performed by a function and annotate
	       * its label with it.
	       *)
		fun funcAlloc (CFG.FUNC{lab, start, body, ...}) = (
		    (* first annotate each block with its allocation *)
		      List.app annotateBlock (start::body);
		    (* compute transitive allocation annotations *)
		      List.app (ignore o blockAlloc) (start::body))
                and blockAlloc (CFG.BLK{lab, body, exit, ...}) = (case peekAlloc lab
		       of NONE => let
			  (* data allocated in this block *)
			    val alloc = getBlkAlloc lab
			  (* transitive allocation by a called function/continuation.  If we know
			   * the call sites, then take the maximum of the functions that are not
			   * in the feedback set.  Note that by ignoring members of the feedback
			   * set, we are safe from infinite loops.
                           * We can also ignore any escaping function because they will be
                           * annotated with their own allocation checks.
			   *)
			    val alloc = let
				  fun f (lab, sz) = if FB.Set.member(fbSet, lab) orelse CFA.isEscaping lab
					then 0w0
					else let
					  val sz' = blockAlloc (valOf(U.blockOfLabel lab))
					  in
					    Word.max(sz', sz)
					  end
				  in
				    case CFA.labelsOf exit
				     of NONE => alloc
				      | SOME labs => CFG.Label.Set.foldl f alloc labs
				    (* end case *)
				  end
			    in
			      setAlloc (lab, alloc); alloc
			    end
			| SOME alloc => alloc
		      (* end case *))
	      (* annotate each block with the amount of allocation it does *)
		val _ = List.app funcAlloc code
	      (* add allocation checks as needed *)
		fun rewrite (f, fs) = let
		      val CFG.FUNC{lab, entry, start, body=bodyBlocks} = f
		      val CFG.BLK{args, body, exit, ...} = start
		      fun needsCheck lab = (FB.Set.member(fbSet, lab) orelse CFA.isEscaping lab)
					   (* andalso (getAlloc lab > 0w0) *)
		    (* rename parameters *)
		      val (freeVars, args', orig, entry') = (case entry
			     of CFG.StdFunc{clos, ret, exh} => let
				  val clos' = CFG.Var.copy clos
				  val args' = List.map CFG.Var.copy args
				  val ret' = CFG.Var.copy ret
				  val exh' = CFG.Var.copy exh
				  in (
				    clos' :: args' @ [ret', exh'],
				    args',
				    clos :: args @ [ret, exh],
				    CFG.StdFunc{clos=clos', ret=ret', exh=exh'}
				  ) end
			      | CFG.StdCont{clos} => let
				  val clos' = CFG.Var.copy clos
				  val args' = List.map CFG.Var.copy args
				  in
				    (clos' :: args', args', clos :: args, CFG.StdCont{clos=clos'})
				  end
			      | CFG.KnownFunc{clos} => let
				  val clos' = CFG.Var.copy clos
				  val args' = List.map CFG.Var.copy args
				  in
				    (clos' :: args', args', clos :: args, CFG.KnownFunc{clos=clos'})
				  end
			   (* end case *))
		      fun convertBlock (block, freeVars, renamedArgs, allArgs) = let
			    val CFG.BLK{body, args, exit, lab} = block
			    val lab' = CFG.Label.new(
				    CFG.Label.nameOf lab ^ checkLabel,
				    CFGTy.T_Block{args = List.map CFG.Var.typeOf freeVars})
			    val heapBodyBlock = CFG.mkBlock(
				    lab, renamedArgs, [],
				    CFG.HeapCheck{
					hck = hcKind, szb = getAlloc lab, nogc = (lab', freeVars)
				      })
			    val newBlock = CFG.mkBlock(lab', allArgs, body, exit)
			    in
(* FIXME: this approach will probably produce a few unecessary limit checks,
 * since we do not combine the checks.  But this can onlu happen in the case
 * of blocks that do a lot of allocation, which should be rare.
 *)
			      annotateBlock newBlock;
			      (heapBodyBlock, splitLargeBock newBlock)
			    end
		      val export = (case CFG.Label.kindOf lab
				     of CFG.LK_Func{export, ...} => export
				      | _ => raise Fail "bogus label kind"
				   (* end case *))
		      val ((start, other), entry) = if needsCheck lab
			    then (convertBlock (start, freeVars, args', orig), entry')
			    else ((start, []), entry)
		      val body = let
			    fun doBlk (b as CFG.BLK{lab, args, ...}, rr) =
				  if needsCheck lab
				    then let
				      val args' = List.map CFG.Var.copy args
				      val (a, b) = convertBlock (b, args', args', args)
				      in
					a :: b @ rr
				      end
				    else splitLargeBock b @ rr
			    in
			      List.foldl doBlk other bodyBlocks
			    end
		      val f' = CFG.mkFunc(lab, entry, start, body, export)
		      in
			f' :: fs
		      end (* rewrite *)
		in
		  rewrite
		end
	  val code = List.foldr (addAllocChecks CFG.HCK_Local) [] code
	  val module = CFG.mkModule(name, externs, code)
	  in
	  (* recompute the census counts *)
	    Census.census module;
	    module
	  end

  end
