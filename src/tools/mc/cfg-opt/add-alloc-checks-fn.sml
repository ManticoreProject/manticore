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
 *	0-CFA would allow us to construct a more complete graph.
 *)

functor AddAllocChecksFn (Target : TARGET_SPEC) : sig

    val transform : CFG.module -> CFG.module

  end = struct

    structure Vertex = struct
	type vertex = CFG.label
	val compare = CFG.Label.compare
      end

    structure FB = FeedbackFn (Vertex)
    structure VSet = FB.Set

    fun sizeOf ty = Target.wordSzB (* FIXME *)

  (* construct the flow graph for a module *)
    fun makeGraph code = let
	(* return the outgoing targets of a function *)
	  fun toNode (CFG.FUNC{lab, body, ...}) = let
		fun out (CFG.Exp(_, e), l) = (case e
		       of CFG.E_Let(_, _, e) => out(e, l)
			| CFG.E_HeapCheck _ => raise Fail "unexpected HeapCheck"
			| CFG.E_If(_, j1, j2) => VSet.addList(l, [#1 j1, #1 j2])
			| CFG.E_Switch(_, cases, dflt) => let
			    val l = (case dflt of SOME(lab, _) => VSet.add(l, lab) | _ => l)
			    fun f ((_, (lab, _)), l) = VSet.add(l, lab)
			    in
			      List.foldl f l cases
			    end
			| CFG.E_Apply(f, _) => (case CFG.Var.kindOf f
			     of CFG.VK_Let(CFG.E_Label f') => (case CFG.Label.kindOf f'
				   of CFG.Local => VSet.add(l, f')
				    | CFG.Export _ => VSet.add(l, f')
				    | _ => l
				  (* end case *))
			      | _ => l
			    (* end case *))
(* FIXME: if k is known, we should include its label in the graph. *)
			| CFG.E_Throw(k, _) => l
			| CFG.E_Goto jmp => VSet.add(l, #1 jmp)
		      (* end case *))
		in
		  (lab, VSet.listItems(out(body, VSet.empty)))
		end
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

(* FIXME: what about known continuations? *)
    fun escaping CFG.KnownFunc = false
      | escaping _ = true

    fun transform (CFG.MODULE{code, funcs, ...}) = let
	  val graph = makeGraph code
	  val fbSet = FB.feedback graph
	(* compute the allocation performed by a function and annotate
	 * its label with it.
	 *)
	  fun funcAlloc (CFG.FUNC{lab, body, ...}) = let
		fun labelAlloc lab = if FB.Set.member(fbSet, lab)
		      then 0w0
		      else funcAlloc (valOf(CFG.Label.Map.find(funcs, lab)))
		fun jumpAlloc (lab, _) = labelAlloc lab
		fun expAlloc (CFG.Exp(_, e')) = (case e'
		       of CFG.E_Let(_, rhs, e) => rhsAlloc rhs + expAlloc e
			| CFG.E_HeapCheck _ => raise Fail "unexpected HeapCheck"
			| CFG.E_If(_, j1, j2) => Word.max(jumpAlloc j1, jumpAlloc j2)
			| CFG.E_Switch(_, cases, dflt) => let
			    fun f ((_, jmp), sz) = Word.max(jumpAlloc jmp, sz)
			    val sz = List.foldl f 0w0 cases
			    in
			      case dflt of SOME jmp => Word.max(jumpAlloc jmp, sz) | _ => sz
			    end
			| CFG.E_Apply(f, _) => applyAlloc f
			| CFG.E_Throw(k, _) => applyAlloc k
			| CFG.E_Goto jmp => jumpAlloc jmp
		      (* end case *))
		and rhsAlloc (CFG.E_Alloc(ty, _)) = sizeOf ty
		  | rhsAlloc _ = 0w0
	      (* transitive allocation by a called function/continuation.  If we know the
	       * call sites, then take the maximum of the functions that are not in the
	       * feedback set.  Note that by ignoring members of the feedback set, we are
	       * safe from infinite loops.
	       *)
		and applyAlloc f = (case CFACFG.valueOf f
		       of CFACFG.LABELS labs => let
			    fun f (lab, sz) = Word.max(labelAlloc lab, sz)
			    in
			      CFG.Label.Set.foldl f 0w0 labs
			    end
			| _ => 0w0
		      (* end case *))
		val alloc = expAlloc body
		in
		  case peekAlloc lab
		   of NONE => let
			val alloc = expAlloc body
			in
			  setAlloc (lab, alloc); alloc
			end
		    | SOME alloc => alloc
		  (* end case *)
		end
	(* annotate each block with the amount of allocation it does *)
	  val _ = List.app (ignore o funcAlloc) code
	(* add allocation checks as needed *)
	  fun rewrite (f as CFG.FUNC{lab, kind, params, body}) =
		if FB.Set.member(fbSet, lab) orelse escaping kind
		  then let
		    val sz = getAlloc lab
		    in
		      CFG.FUNC{lab=lab, kind=kind, params=params, body=CFG.mkHeapCheck(sz, body)}
		    end
		  else f
	  val code = List.map rewrite code
	  in
	    CFG.mkModule code
	  end

  end
