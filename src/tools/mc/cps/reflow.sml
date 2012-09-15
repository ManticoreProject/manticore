(* reflow.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * TODO:
 *
 *)

structure Reflow : sig

    val analyze : CPS.module -> unit
    val clearInfo : CPS.module -> unit

    val bindingLocation : CPS.var -> ProgPt.ppt

    val pathExists : ProgPt.ppt * ProgPt.ppt -> bool
    val pointAnalyzed : ProgPt.ppt -> bool

    val debugFlg : bool ref

  end = struct


    val debugFlg = ref true
    structure CV = CPS.Var
    structure VSet = CV.Set
    structure ST = Stats
    structure PMap = ProgPt.Map
    structure PSet = ProgPt.Set

    (*structures needed for SCC compression*)
    structure ProgNd = struct
        type ord_key = ProgPt.ppt
        val compare = ProgPt.compare
      end

    structure SCC = GraphSCCFn (ProgNd)

    datatype reaches = REACHES of PSet.set | TOP

    (* This graph is used to answer the pathExists query above.
     * It is a reachability (closure of adjacency) map. *)
    val graph : reaches PMap.map ref = ref PMap.empty

    (* This map goes from a program point to its path-compressed
     * representative.
     *)
    val representative : ProgPt.ppt PMap.map ref = ref PMap.empty

    (* property mapping variables to binding locations *)
    val {getFn=bindingLocation : CV.var -> ProgPt.ppt, clrFn=clrBindingLocation, setFn=setBindingLocation, ...} =
          CV.newProp (fn v => raise Fail (concat[CV.toString v, ": variable missing binding location"]))

    (* property mapping function variables to their body location *)
    val {getFn=bodyLocation : CV.var -> ProgPt.ppt, clrFn=clrBodyLocation, setFn=setBodyLocation, ...} =
          CV.newProp (fn _ => ProgPt.new())

  (***** Statistics *****)
    val cntPasses		= ST.newCounter "reflow:num-passes"

  (*
   * Set the binding and body locations.
   * This operation is done in a first pass before the call information is assembled
   * because these properties must be in place before the call graph can be 
   * determined.
   *)
    fun pptOfExp (CPS.Exp (ppt, _)) = ppt
    fun setLocations (CPS.MODULE{body, ...}) = let
          fun doLambda (ppt, CPS.FB{f, params, rets, body}) = let
              fun bindToBody v = setBindingLocation (v, pptOfExp body)
          in
              setBindingLocation (f, ppt);
              setBodyLocation (f, pptOfExp body);
              List.app bindToBody params;
              List.app bindToBody rets;
              doExp body
          end
          and doExp (CPS.Exp(ppt, e)) = (case e 
                 of CPS.Let(xs, _, e) => (
                    List.app (fn (v) => setBindingLocation (v, ppt)) xs; doExp e)
                  | CPS.Fun(fbs, e) => (List.app (fn (fb) => doLambda(ppt,fb)) fbs; doExp e)
                  | CPS.Cont(fb, e) => (doLambda (ppt, fb); doExp e)
                  | CPS.If(_, e1, e2) => (doExp e1; doExp e2)
                  | CPS.Switch(_, cases, dflt) => (
		      List.app (doExp o #2) cases;
		      Option.app doExp dflt)
                  | CPS.Apply _ => ()
                  | CPS.Throw _ => ()
                (* end case *))
          in
            doLambda (ProgPt.new(), body)
          end

    fun addInfo (m, p1, p2) =
        case PMap.find (m, p1)
         of NONE => PMap.insert (m, p1, REACHES (PSet.add (PSet.empty, p2)))
          | SOME ps => (
            case ps
             of REACHES s => PMap.insert (m, p1, REACHES (PSet.add (s, p2)))
              | TOP => m)

    fun setTop (m, ppt) = PMap.insert (m, ppt, TOP)

    fun addNeighbors (CPS.MODULE{body as CPS.FB{f,...}, ...}) = let
          fun doLambda (ppt, CPS.FB{f, params, rets, body}, (m, ptlist)) =
              doExp (body, (m, ptlist))
          and doExp (CPS.Exp(ppt, e), (m, ptlist)) = (case e 
                 of CPS.Let(xs, _, e) => 
                    doExp (e, (addInfo (m, ppt, pptOfExp e), ptlist))
                  | CPS.Fun(fbs, e) => let
                        val (m, ptlist) = List.foldr (fn (fb, (m, ptlist)) => doLambda(ppt, fb, (m, ptlist))) (m, ptlist) fbs
			fun anyknown(fb as CPS.FB{f,...}, truth) = 
			    (case CFACPS.callersOf f
			      of CFACPS.Unknown => true
			       | _ => truth)
			val notknown = List.foldr anyknown false fbs (*TRUE if any fbs have unknown callersOf, FALSE otherwise *)
                    in
                        case notknown
			 of true => doExp (e, (addInfo (m, ppt, pptOfExp e), (pptOfExp e)::ptlist))
			  | false => doExp (e, (addInfo (m, ppt, pptOfExp e), ptlist))
                    end
                  | CPS.Cont(fb as CPS.FB{f,...}, e) => let
                        val (m, ptlist) = doLambda (ppt, fb, (m, ptlist))
                    in 
			(case CFACPS.callersOf f
			  of CFACPS.Unknown => doExp(e, (addInfo (m, ppt, pptOfExp e), (pptOfExp e)::ptlist))
			   | _ => doExp (e, (addInfo (m, ppt, pptOfExp e), ptlist)))
                    end
                  | CPS.If(_, e1, e2) => let
                        val m = addInfo (m, ppt, pptOfExp e1)
                        val m = addInfo (m, ppt, pptOfExp e2)
                        val (m, ptlist) = doExp (e1, (m, ptlist))
                    in
                        doExp (e2, (m, ptlist))
                    end
                  | CPS.Switch(_, cases, dflt) => let
                        val caseExps = List.map #2 cases
                        val ppts = List.map pptOfExp caseExps
                        val m = List.foldr (fn (cppt, m) => addInfo (m, ppt, cppt)) m ppts
                        val (m, ptlist) = List.foldr (fn (e, (m, ptlist)) => doExp (e, (m, ptlist))) (m, ptlist) caseExps
                    in
                        case dflt
                         of NONE => (m, ptlist)
                          | SOME e => doExp (e, (addInfo (m, ppt, pptOfExp e), ptlist))
                    end
                  | CPS.Apply (f, _, _) => (
                    case CFACPS.valueOf f
                     of CFACPS.TOP => (print (concat["Apply TOP to var: ", CV.toString f, "\n"]); (setTop (m, ppt), ptlist))
                      | CFACPS.LAMBDAS ls => let
                            val ll = VSet.listItems ls
                            val pl = List.map bodyLocation ll
                        in
                            (List.foldr (fn (fppt, m) => addInfo (m, ppt, fppt)) m pl, ptlist)
                        end
                      | CFACPS.BOT => (m, ptlist)
                      | CFACPS.TUPLE _ => raise Fail (concat[CV.toString f, " is in an application position but is a tuple according to CFA."]))
                  | CPS.Throw (k, _) => (
                    case CFACPS.valueOf k
                     of CFACPS.TOP => (print (concat["Throw TOP to var: ", CV.toString k, "\n"]); (setTop (m, ppt), ptlist))
                      | CFACPS.LAMBDAS ls => let
                            val ll = VSet.listItems ls
                            val pl = List.map bodyLocation ll
                        in
                            (List.foldr (fn (fppt, m) => addInfo (m, ppt, fppt)) m pl, ptlist)
                        end
                      | CFACPS.BOT => (m, ptlist)
                      | CFACPS.TUPLE _ => raise Fail (concat[CV.toString k, " is in an application position but is a tuple according to CFA."]))
                (* end case *))
          in
            doLambda (bindingLocation f, body, (PMap.empty, nil))
          end

    (* union returns a flag for whether or not the value changed and the result of the union *)
    fun union (TOP, r2) = (false, TOP)
      | union (_, TOP) = (true, TOP)
      | union (REACHES ps1, REACHES ps2) = let
            val cnt = PSet.numItems ps1
            val result = PSet.union (ps1, ps2)
        in
           (not(cnt=PSet.numItems result), REACHES result)
        end

    (*
     * PROBLEM:
     * Given an adjacency-list representation (vertex -> vertex list of neighbors),
     * compute the transitive closure efficiently to get the set of accessible nodes.
     *
     * One idea:
     * Transform into the NxN adjacency matrix, A
     * Compute A^n
     * Now, at each A[i,j] we have the # of n-step walks from i..j!
     * BUT, that's n NxN matrix multiplications... could keep zero/one, if that
     * speeds things up?
     * NOTE: this operation should take less than n multiplications, because this
     * graph is a forest of trees, since we've collapsed all SCCs into a single
     * node.
     *)

    (* FIXME: This implementation is far too slow. *)
    fun computeReachability map = let
        fun addEntry (ppt, reaches, (b, map)) = (
            case reaches
             of TOP => (b,map)
              | r1 as REACHES ps => let
                    fun extend (ppt', (b, map)) = (
                        case (PMap.find (map, ppt'))
                         of NONE => (b, map)
                          | SOME reaches' => (
                            case reaches'
                             of TOP => (true, PMap.insert (map, ppt, TOP))
                              | r2 as REACHES ps' => let
                                    val (changed, new) = union (r1, r2)
                                in
                                    if changed
                                    then let
                                            (*
                                             * Since we computed the closure w.r.t
                                             * all reachable items, we might as well
                                             * update those reachable items with the
                                             * result as well.
                                             *)
                                            fun update (ppt, map) =
                                                PMap.insert (map, ppt, new)
                                            val map = PSet.foldl update map ps'
                                        in
                                            (changed, PMap.insert (map, ppt, new))
                                        end
                                    else (b, map)
                                end))
                in
                    PSet.foldl extend (b, map) ps
                end)
        fun iterate map =
            PMap.foldli addEntry (false, map) map
        fun loop map = let
            val _ = ST.tick cntPasses
            val (b, map) = iterate map
        in
            if b
            then loop map
            else map
        end
    in
        loop map
    end

    fun compressSCC (p, ptlist) = let
        (* TODO:
         * This list (the places an unknown call can go to) can be reduced to just
         * those program points that define functions whose callersOf are unknown.
         * If the list is still too big, it could also be split by fun/cont types.
         *)
	fun follow pt =
            case PMap.find(p, pt)
             of SOME v => (case v
	                    of TOP => ptlist
	                     | REACHES l => PSet.listItems l)
              | NONE => []
	val components = SCC.topOrder'{roots = map #1 (PMap.listItemsi p), follow = follow}
        val _ = if !debugFlg
                then print (concat["Number of SCC components: ",
                                   Int.toString (List.length components),
                                   "\n"])
                else ()
	fun updateReps (SCC.SIMPLE nd) = PMap.insert(PMap.empty, nd, nd) 
	  | updateReps (SCC.RECURSIVE ndList) =
	    let
		val rep = hd(ndList)
	    in
		foldl (fn (n, mp) => PMap.insert(mp, n, rep)) PMap.empty ndList
	    end
	fun foldnewreps(c, mp) = PMap.unionWith (fn(a, b) => a) ((updateReps c), mp)
	val newreps = foldl foldnewreps PMap.empty components
	fun fixLH (pt, _) =
	    case PMap.find(!representative, pt)
	     of NONE => if !debugFlg
			then (print ("Oops\n"); false)
			else false
	      | SOME rep =>
		if ProgPt.compare(pt, rep) = EQUAL
		then true
		else false
	fun fixRH adjs = (* this assumes that ORD_SET just ignores duplicates, since it's a set *)
	    case adjs
	     of TOP => TOP
	      | REACHES ns => let
		    fun mapToRep pt = Option.valOf(PMap.find(!representative, pt))  
		in REACHES ((PSet.map mapToRep) ns)
		end
    in
	(representative := newreps;
	 print (concat["Number of keys in rep (should match number of program points): ", Int.toString(PMap.numItems(!representative)), "\n"]);
	 PMap.map fixRH (PMap.filteri fixLH p))
    end


    fun analyze (module as CPS.MODULE{body, ...}) = let
        val a = Time.now()
        val _ = setLocations module
        val b = Time.now()
        val (neighbors, neighborlist) = addNeighbors module
        val c = Time.now()
        val _ = if !debugFlg
                then print (concat["Number of program points: ",
                                   Int.toString (PMap.numItems neighbors),
                                   "\n"])
                else ()
        val SCCCompressed = compressSCC (neighbors, neighborlist)
        val d = Time.now()
        val reachability = computeReachability SCCCompressed
        val e = Time.now()
        val _ = if !debugFlg
                then print (concat["Set locations: ", Time.toString (Time.-(b,a)), "\n",
                                   "Add neighbors: ", Time.toString (Time.-(c,b)), "\n",
                                   "Compress SCC: ", Time.toString (Time.-(d,c)), "\n",
                                   "Compute Reachability: ", Time.toString (Time.-(e,d)), "\n"])
                else ()
    in
        graph := reachability
    end


    fun pathExists (p1, p2) = (
        case PMap.find (!graph, Option.valOf(PMap.find(!representative, p1)))
         of NONE => false
          | SOME r => (case r
                        of REACHES ps => PSet.member (ps, Option.valOf(PMap.find(!representative, p2)))
                         | TOP => true))

    fun pointAnalyzed (p) =
        case PMap.find (!representative, p)
         of SOME _ => true
          | NONE => false

    val analyze = BasicControl.mkTracePassSimple {
	    passName = "reflow",
	    pass = analyze
	  }

  (* clear reflow annotations from the variables of a module.  Note that
   * we can restrict the traversal to binding instances.
   *)
    fun clearInfo (CPS.MODULE{body, ...}) = let
          fun doLambda (CPS.FB{f, params, rets, body}) = (
                clrBindingLocation f;
                clrBodyLocation f;
                List.app clrBindingLocation params;
                List.app clrBindingLocation rets;
                doExp body)
          and doExp (CPS.Exp(_, e)) = (case e 
                 of CPS.Let(xs, _, e) => (List.app clrBindingLocation xs; doExp e)
                  | CPS.Fun(fbs, e) => (List.app doLambda fbs; doExp e)
                  | CPS.Cont(fb, e) => (doLambda fb; doExp e)
                  | CPS.If(_, e1, e2) => (doExp e1; doExp e2)
                  | CPS.Switch(_, cases, dflt) => (
		      List.app (doExp o #2) cases;
		      Option.app doExp dflt)
                  | CPS.Apply _ => ()
                  | CPS.Throw _ => ()
                (* end case *))
          in
            doLambda body;
            graph := PMap.empty
          end

  end

