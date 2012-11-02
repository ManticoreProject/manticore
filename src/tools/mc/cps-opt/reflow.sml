(* reflow.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure Reflow : sig

    val analyze : CPS.module -> unit
    val clearInfo : CPS.module -> unit

    val bindingLocation : CPS.var -> ProgPt.ppt
    val rebindingLocations : CPS.var -> ProgPt.Set.set

    val pathExists : ProgPt.ppt * ProgPt.ppt -> bool
    val pointAnalyzed : ProgPt.ppt -> bool

    val debugFlg : bool ref

  end = struct


    val debugFlg = ref true

    val () = List.app (fn ctl => ControlRegistry.register CPSOptControls.registry {
              ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
              envName = NONE
            }) [
              Controls.control {
                  ctl = debugFlg,
                  name = "reflow-debug",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "debug reflow"
                }
             ]

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
    val graph : PSet.set PMap.map ref = ref PMap.empty

    (* This map goes from a program point to its path-compressed
     * representative.
     *)
    val representative : ProgPt.ppt PMap.map ref = ref PMap.empty

    (* property mapping variables to rebinding locations, which includes both the original location and all
     * points where closures they may have been captured in are invoked *)
    val {getFn=rebindingLocations : CV.var -> PSet.set, clrFn=clrRebindingLocations, setFn=setRebindingLocationsInternal, peekFn=peekRebindingLocations, ...} =
          CV.newProp (fn v => raise Fail (concat[CV.toString v, ": variable missing rebinding locations"]))

    fun setRebindingLocation (v, loc) =
	case peekRebindingLocations v
	 of SOME s => setRebindingLocationsInternal (v, PSet.add (s, loc))
	  | NONE => setRebindingLocationsInternal (v, PSet.singleton (loc))

    (* property mapping variables to unique binding locations *)
    val {getFn=bindingLocation : CV.var -> ProgPt.ppt, clrFn=clrBindingLocationInternal, setFn=setBindingLocationInternal, ...} =
          CV.newProp (fn v => raise Fail (concat[CV.toString v, ": variable missing binding location"]))

    fun setBindingLocation (v, ppt) = (
	setRebindingLocation (v, ppt);
	setBindingLocationInternal (v, ppt))
    fun clrBindingLocation v = (
	clrRebindingLocations v;
	clrBindingLocationInternal v)

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
	(* If this target is an indirect call through a closure, then hook all of the free variables up
	 * to a fabricated program point in the graph that sits between the invocation point and the
	 * target function.
	 *)
	fun foldChainVars (map, sourcePPT, origVar, targetFuns, fvsOfTargetFuns, ptlist) = (
	    case (targetFuns, fvsOfTargetFuns)
	     of ([], _) => (map, ptlist)
	      | (f::targetFuns', fvs::fvsOfTargetFuns') => (
		if CV.same (f, origVar) orelse (VSet.isEmpty fvs)
		then foldChainVars (addInfo (map, sourcePPT, bodyLocation f), sourcePPT,
				    origVar, targetFuns', fvsOfTargetFuns', ptlist)
		else (let
			  val fvPPT = ProgPt.new()
			  val map' = VSet.foldr (fn (fv, m) => (setRebindingLocation (fv, fvPPT);
								  addInfo (m, sourcePPT, fvPPT))) map fvs
			  val map'' = addInfo (map', fvPPT, bodyLocation f)
		      in
			  (map'', ptlist)
		      end))
	      | _ => raise Fail "Called with unbalanced TARGET and TARGET_FVS lists")
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
                     of CFACPS.TOP => (setTop (m, ppt), ptlist)
                      | CFACPS.LAMBDAS ls => let
                            val ll = VSet.listItems ls
			    val freeVars = List.map FreeVars.envOfFun ll
                        in
			    foldChainVars (m, ppt, f, ll, freeVars, ptlist)
                        end
                      | CFACPS.BOT => (m, ptlist)
                      | CFACPS.TUPLE _ => raise Fail (concat[CV.toString f, " is in an application position but is a tuple according to CFA."]))
                  | CPS.Throw (k, _) => (
                    case CFACPS.valueOf k
                     of CFACPS.TOP => (setTop (m, ppt), ptlist)
                      | CFACPS.LAMBDAS ls => let
                            val ll = VSet.listItems ls
			    val freeVars = List.map FreeVars.envOfFun ll
                        in
			    foldChainVars (m, ppt, k, ll, freeVars, ptlist)
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
     * First, invert the adjacency-list representation to get a parent-list represenation.
     * While doing that, also pick out the "leaves" (i.e., those vertices with no outgoing
     * edges).
     *
     * Maintain two sets:
     * - Black is empty
     * - Grey is those leaves
     *
     * for-each v in Grey
     *  Add v to Black
     *  for-each p in parent(v)
     *   R(p) = R(p) union R(v) union [v]
     *   if all adjacents of p are Black, then mark p Grey
     *)

    fun computeReachability (map, topList) = let
        fun invert map = let
            fun addEntries (ppt, s, (m, inAll, leaves)) = let
                fun addInfo (m, p1, p2) =
                    case PMap.find (m, p1)
                     of NONE => PMap.insert (m, p1, PSet.add (PSet.empty, p2))
                      | SOME s => (
                          PMap.insert (m, p1, PSet.add (s, p2)))
            in
		case PSet.isEmpty s
                 of true => (m, inAll, ppt::leaves)
                  | false => (PSet.foldl (fn (ppt', m) =>
                                             addInfo (m, ppt', ppt)) m s,
                              inAll, leaves)
            end
        in
            PMap.foldli addEntries (PMap.empty, [], []) map
        end
            
        val (parentMap, inAll, leaves) = invert map
        val inAll = PSet.fromList inAll

        (* Add in the entries that are the parents of _all_ vertices by
	 * walking through all of the entries in the adjacency map and
	 * adding the inAll elements to their set of parents.
	 *)
        val parentMap = PMap.foldli (fn (ppt, _, m) => (
				       case PMap.find (m, ppt)
					of NONE => PMap.insert (m, ppt, inAll)
					 | SOME s => PMap.insert (m, ppt, PSet.union (s, inAll))))
                                    parentMap
                                    map

	val _ = if !debugFlg
		then (print (concat["Number of leaves: ", Int.toString (List.length leaves), "\nNumber of parents:",
		     Int.toString (PMap.numItems parentMap), "\nNumber inall: ", Int.toString (PSet.numItems inAll), "\n"]))
		else ()

        val initialGrey = PSet.fromList leaves

        fun compute (rmap, black, grey) = (
            case PSet.isEmpty grey
             of true => rmap
              | false => let
                  val next = Option.valOf (PSet.find (fn _ => true) grey)
                  val grey' = PSet.delete (grey, next)
                  val black' = PSet.add (black, next)
              in
                  case PMap.find (parentMap, next)
                   of NONE => compute (rmap, black', grey')
                    | SOME parents => (
                               let
                                   fun merge (m, p1, p2) = let
                                       val newRs = (case PMap.find (m, p2)
                                                     of NONE => PSet.add (PSet.empty, p2)
                                                      | SOME s => PSet.add (s, p2))
                                   in
                                       case PMap.find (m, p1)
                                        of NONE => PMap.insert (m, p1, newRs)
                                         | SOME s => PMap.insert (m, p1, PSet.union (s, newRs))
                                   end
                                   fun doEntry (parent, (rmap, grey)) = let
                                       val rmap = merge (rmap, parent, next)
                                       val turnGrey = (case PMap.find (map, parent)
                                                        of NONE => true
                                                         | SOME s => (not(PSet.exists (fn x => not(PSet.member (black', x))) s)))
                                       val grey = if turnGrey then PSet.add (grey, parent) else grey
                                   in
                                       (rmap, grey)
                                   end
                                   val (rmap, grey') = PSet.foldl doEntry (rmap, grey') parents
                                                                            
                               in
                                   compute (rmap, black', grey')
                               end)
              end)
	val result = compute (PMap.empty, PSet.empty, initialGrey)
    in
	if !debugFlg
	then (print (concat["Number of keys in reachability graph (should match SCC components & compressed ): ", Int.toString(PMap.numItems(result)), "\n"]))
	else ();
	result
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
              | NONE => [] (* points not appearing have no out edges *)
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


	(* BUGBUG: PMap.numItems(compressed) from this code is always zero? *)
(*	fun fixRH adjs = (* this assumes that ORD_SET just ignores duplicates, since it's a set *)
	    case adjs
	     of TOP => TOP
	      | REACHES ns => let
		    fun mapToRep pt = Option.valOf(PMap.find(!representative, pt))
		in REACHES ((PSet.map mapToRep) ns)
		end
	val compressed = PMap.map fixRH (PMap.filteri fixLH p)
 *)

	(*
	 * Computing the compressed graph:
	 * Need to have one entry in the map per representative, with a list of
	 * all the points adjacent to it.
	 * Straightforward for simple. For RECURSIVE, need to add all the adjacent points
	 * from each of the original points, but filtering out those that are not
	 * reprentatives.
	 *
	 *)
	val ptset = PSet.fromList ptlist
	fun reachesFromPoint pt =
	    case PMap.find (p, pt)
	     of SOME reaches => (case reaches
				  of TOP => ptset
				   | REACHES s => s)
	      | NONE => PSet.empty
	fun filterNonReps s =
	    PSet.filter (fn pt => not(Option.isSome (PMap.find (newreps, pt)))) s
	fun findAdj (SCC.SIMPLE pt, map) = PMap.insert (map, pt, filterNonReps (reachesFromPoint pt))
	  | findAdj (SCC.RECURSIVE pts, map) = (
	    let
		val adjacents = List.foldl (fn (pt, reaches) => PSet.union(reaches, reachesFromPoint pt)) PSet.empty pts
	    in
		PMap.insert (map, hd(pts), filterNonReps adjacents)
	    end)
	val compressed = List.foldl findAdj PMap.empty components

    in
	(representative := newreps;
	 if !debugFlg
	 then (
	     print (concat["Number of items in rep (should match number of program points + 2): ", Int.toString(PMap.numItems(!representative)), "\n"]);
	     print (concat["Number of keys in compressed graph (should match SCC components): ", Int.toString(PMap.numItems(compressed)), "\n"]))
	 else ();
	 compressed)
    end


    fun analyze (module as CPS.MODULE{body, ...}) = let
	val _ = FreeVars.clear module
	val _ = FreeVars.analyzeIgnoringJoin module
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
        val reachability = computeReachability (SCCCompressed, neighborlist)
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
         of NONE => (print (concat["How are we missing: ", ProgPt.toString p1, "\n"]); false)
          | SOME ps => (PSet.member (ps, Option.valOf(PMap.find(!representative, p2)))))

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

