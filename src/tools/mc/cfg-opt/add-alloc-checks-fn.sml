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

    structure Vertex = struct
	type vertex = CFG.label
	val compare = CFG.Label.compare
      end

    structure FB = FeedbackFn (Vertex)
    structure VSet = FB.Set
    structure LSet = CFG.Label.Set
    structure ABI = Target.ABI


    fun notExtern (CFG.LK_Extern _) = false
      | notExtern _ = true
      
    val noExterns = notExtern o CFG.Label.kindOf

  (* construct the flow graph for a module *)
    fun makeGraph code = let
	(* return the outgoing targets of a function *)
	  fun nodeFromBlock (CFG.BLK{lab, exit, ...}) = (case CFA.labelsOf exit
                 of NONE => (lab, [])
                  | SOME ls => (lab, LSet.listItems (LSet.filter noExterns ls))
                (* end case *))
          val nodeFromBlock= fn b => if Controls.get CFGOptControls.debug
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

  (* label annotations *)
    val {clrFn=clrAlloc, getFn=getAlloc, peekFn=peekAlloc, setFn=setAlloc} =
	  CFG.Label.newProp (fn _ => 0w0)

  (* global alloc check label annotations *)
    val {clrFn=gClrAlloc, getFn=gGetAlloc, peekFn=gPeekAlloc, setFn=gSetAlloc} =
	  CFG.Label.newProp (fn _ => 0w0)

  (* the amount of storage allocated by an expression *)
    fun expAlloc (CFG.E_Alloc(_, _, xs)) = Word.fromLargeInt ABI.wordSzB * Word.fromInt(length xs + 1)
      | expAlloc _ = 0w0

  (* the amount of storage allocated by an expression *)
    fun gExpAlloc (CFG.E_GAlloc(_, _, xs)) = Word.fromLargeInt ABI.wordSzB * Word.fromInt(length xs + 1)
      | gExpAlloc _ = 0w0
      
    fun xferAlloc (CFG.Call{f,...}) = let
            (* this is only here because the only external label expected
               is ASM_Callec, which is hand-written assembly whose allocation
               should be accounted for. *)
            fun isExternLab v = (case CFG.Var.kindOf v
                of CFG.VK_Let(CFG.E_Label(_, lab)) => not(noExterns lab)
                 | _ => false
                (* esac *))
        in
            if isExternLab f
            then Word.fromLargeInt ABI.wordSzB * Word.fromInt(5)
            else 0w0
        end
      | xferAlloc _ = 0w0

    fun transform (CFG.MODULE{name, externs, mantiExterns, code}) = let
	  val graph = makeGraph code
	  val fbSet = FB.feedback graph
        (* add allocation checks as needed to a function *)
	  fun addAllocChecks hcKind = let
                val checkLabel = "Check"
	      (* compute the allocation performed by a function and annotate
	       * its label with it.
	       *)
		fun funcAlloc (CFG.FUNC{lab, start, body, ...}) =
		      List.app (ignore o blockAlloc) (start::body)
                and blockAlloc (CFG.BLK{lab, body, exit,...}) = (case peekAlloc lab
		       of NONE => let
			  (* transitive allocation by a called function/continuation.  If we know the
			   * call sites, then take the maximum of the functions that are not in the
			   * feedback set.  Note that by ignoring members of the feedback set, we are
			   * safe from infinite loops.
                           * We can also ignore any escaping function because they will be annotated with
                           * their own allocation checks.
			   *)
			    val alloc = let
				  fun f (lab, sz) = if FB.Set.member(fbSet, lab) orelse CFA.isEscaping lab orelse CFA.hasUnknownReturn lab
					then 0w0
					else let
					  val sz' = blockAlloc (valOf(CFGUtil.blockOfLabel lab))
					  in
					    Word.max(sz', sz)
					  end
				  in
				    case CFA.labelsOf exit
				     of NONE => 0w0
				      | SOME labs => LSet.foldl f 0w0 (LSet.filter noExterns labs)
				    (* end case *)
				  end
			  (* add in any data allocated in this block *)
			    val alloc = List.foldl (fn (e, sz) => sz + expAlloc e) alloc body
			  (* add in any data allocated by the transfer *)
			    val alloc = alloc + xferAlloc exit
			    in
			      (if Controls.get CFGOptControls.debug
			          then print (CFG.Label.toString lab ^ " allocs " ^ Word.toString alloc ^ "\n")
			          else () ;
			      setAlloc (lab, alloc) ; 
			      alloc)
			    end
			| SOME alloc => alloc
		      (* end case *))
	      (* annotate each block with the amount of allocation it does *)
		val _ = List.app funcAlloc code
	      (* add allocation checks as needed *)
		fun rewrite (f as CFG.FUNC{lab, entry, start as CFG.BLK{args, body, exit, ...}, body=bodyBlocks}, fs) = let
		      fun needsCheck lab = (FB.Set.member(fbSet, lab) 
                                    orelse CFA.isEscaping lab
                                    orelse CFA.hasUnknownReturn lab
                                    )
					   (* andalso (getAlloc lab > 0w0) *)
		      val (freeVars, args', orig, entry') = (case entry (* rename parameters *)
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
                  | CFG.StdDirectFunc{clos, exh, ret=retTy} => let
                    val clos' = CFG.Var.copy clos
                    val args' = List.map CFG.Var.copy args
                    val exh' = CFG.Var.copy exh
                  in (
                      clos' :: args' @ [exh'],
                      args',
                      clos :: args @ [exh],
                      CFG.StdDirectFunc{clos=clos', exh=exh', ret=retTy}
                  ) end
                  | CFG.KnownDirectFunc{clos, ret=retTy} => let
                    val clos' = CFG.Var.copy clos
                    val args' = List.map CFG.Var.copy args
                  in (
                      clos' :: args',
                      args',
                      clos :: args,
                      CFG.KnownDirectFunc{clos=clos', ret=retTy}
                  ) end
			   (* end case *))
		      fun convertBlock (block as CFG.BLK{body, args, exit, lab}, freeVars, renamedArgs, allArgs) = let
			    val lab' = CFG.Label.new(
				       CFG.Label.nameOf lab ^ checkLabel,
				       CFGTy.T_Block{args = List.map CFG.Var.typeOf freeVars})
			    val heapBodyBlock = CFG.mkBlock(lab, renamedArgs,
							    [], CFG.HeapCheck{
								hck = hcKind,
								szb = getAlloc lab,
								nogc = (lab', freeVars)
							   })
			    val newBlock = CFG.mkBlock(lab', allArgs, body, exit)
			    in
			      (heapBodyBlock, [newBlock])
			    end
		      val export = (case CFG.Label.kindOf lab
				     of CFG.LK_Func{export, ...} => export
				      | _ => raise Fail "bogus label kind"
				   (* end case *))
		      val ((start, other), entry) = if needsCheck lab
					   then (convertBlock (start, freeVars, args', orig), entry')
					   else ((start, []), entry)
		      val body = List.foldl (fn (b as CFG.BLK{lab, args, ...}, rr) =>
						if needsCheck lab
						then let
                        
                            val _ = if Controls.get CFGOptControls.debug
          			                then print (CFG.Label.toString lab ^ " got a check.\n")
          			                else ()
                              
							val args' = List.map CFG.Var.copy args
							val (a,b) = convertBlock (b, args', args', args)
						    in
							a::(b@rr)
						    end
						else b::rr) other bodyBlocks
		      val f' = CFG.mkFunc(lab, entry, start, body, export)
		      in
			f' :: fs
		      end (* rewrite *)
		in
		  rewrite
		end
	  val code = List.foldr (addAllocChecks CFG.HCK_Local) [] code
	  val module = CFG.mkModule(name, externs, mantiExterns, code)
	  in
	  (* recompute the census counts *)
	    Census.census module;
	    module
	  end

  end
