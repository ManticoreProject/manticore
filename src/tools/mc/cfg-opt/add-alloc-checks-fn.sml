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

    fun sizeOf ty = Target.C.wordSzB (* FIXME *)

  (* construct the flow graph for a module *)
    fun makeGraph code = let
	(* return the outgoing targets of a function *)
	  fun toNode (CFG.FUNC{lab, exit, ...}) =
		(lab, CFG.Label.Set.listItems(CFA.labelsOf exit))
(* +DEBUG *)
val toNode = fn f => let
	val nd as (src, out) = toNode f
	val src = CFG.Label.toString src
	val out = String.concatWith "," (List.map CFG.Label.toString out)
	in
	  print(concat["  ", src, " -> [", out, "]\n"]);
	  nd
	end
(* -DEBUG *)
	  in
(*DEBUG*)print "makeGraph\n";
	    List.map toNode code
	  end

  (* label annotations *)
    val {clrFn=clrAlloc, getFn=getAlloc, peekFn=peekAlloc, setFn=setAlloc} =
	  CFG.Label.newProp (fn _ => 0w0)

  (* the amount of storage allocated by an expression *)
    fun expAlloc (CFG.E_Alloc(_, xs)) = Target.C.wordSzB * Word.fromInt(length xs + 1)
      | expAlloc (CFG.E_Wrap(_, y)) = (case CFG.Var.typeOf y
	   of CFGTy.T_Raw CFGTy.T_Long => 0w12		(* include header word *)
	    | CFGTy.T_Raw CFGTy.T_Double => 0w12
	    | CFGTy.T_Raw CFGTy.T_Vec128 => 0w20
	    | CFGTy.T_Raw _ => 0w8
	    | _ => raise Fail "wrap of non-raw type"
	  (* end case *))
      | expAlloc _ = 0w0

    fun transform (CFG.MODULE{name, externs, code}) = let
	  val graph = makeGraph code
	  val fbSet = FB.feedback graph
	(* compute the allocation performed by a function and annotate
	 * its label with it.
	 *)
	  fun funcAlloc (CFG.FUNC{lab, body, exit, ...}) = (case peekAlloc lab
		 of NONE => let
		    (* transitive allocation by a called function/continuation.  If we know the
		     * call sites, then take the maximum of the functions that are not in the
		     * feedback set.  Note that by ignoring members of the feedback set, we are
		     * safe from infinite loops.
		     *)
		      val alloc = let
			    val labs = CFA.labelsOf exit
			    fun f (lab, sz) = if FB.Set.member(fbSet, lab)
				  then 0w0
				  else let
				    val sz' = funcAlloc (valOf(CFG.funcOfLabel lab))
				    in
				      Word.max(sz', sz)
				    end
			    in
			      CFG.Label.Set.foldl f 0w0 labs
			    end
		    (* add in any data allocated in this function *)
		      val alloc = List.foldl (fn (e, sz) => sz + expAlloc e) alloc body
		    (* the "run" transfer allocates a cons cell *)
		      val alloc = (case exit
			     of CFG.Run _ => alloc + 0w3 * Target.C.wordSzB
			      | _ => alloc
			    (* end case *))
		      in
			setAlloc (lab, alloc); alloc
		      end
		  | SOME alloc => alloc
		(* end case *))
	(* annotate each block with the amount of allocation it does *)
	  val _ = List.app (ignore o funcAlloc) code
	(* add allocation checks as needed *)
	  fun rewrite (f as CFG.FUNC{lab, entry, body, exit}, fs) =
		if FB.Set.member(fbSet, lab) orelse CFA.isEscaping lab
		  then let
		    val (freeVars, entry') = (case entry (* rename parameters *)
			   of CFG.StdFunc{clos, arg, ret, exh} => let
				val clos' = CFG.Var.copy clos
				val arg' = CFG.Var.copy arg
				val ret' = CFG.Var.copy ret
				val exh' = CFG.Var.copy exh
				in (
				  [clos', arg', ret', exh'],
				  CFG.StdFunc{clos=clos', arg=arg', ret=ret', exh=exh'}
				) end
			    | CFG.StdCont{clos, arg} => let
				val clos' = CFG.Var.copy clos
				val arg' = CFG.Var.copy arg
				in
				  ([clos', arg'], CFG.StdCont{clos=clos', arg=arg'})
				end
			    | CFG.KnownFunc params => let
				val params' = List.map CFG.Var.copy params
				in
				  (params', CFG.KnownFunc params')
				end
			    | CFG.Block params => let
				val params' = List.map CFG.Var.copy params
				in
				  (params', CFG.Block params')
				end
			  (* end case *))
		    val lab' = CFG.Label.new(
				Atom.atom "check",
				CFGTy.T_Code(List.map CFG.Var.typeOf freeVars))
		    val f' = CFG.mkFunc(lab, entry', [], CFG.HeapCheck{
			    szb = getAlloc lab,
			    nogc = (lab', freeVars)
			  })
		    val f'' = CFG.mkFunc(lab', CFG.Block(CFG.paramsOfConv entry), body, exit)
		    in
		      f' :: f'' :: fs
		    end
		  else f::fs
	  val code = List.foldr rewrite [] code
	  in
	    CFG.mkModule(name, externs, code)
	  end

  end
