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
          CV.newProp (fn v => raise Fail (concat[CV.toString v, ": variable missing body binding location"]))

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
          fun doLambda (ppt, CPS.FB{f, params, rets, body}, m) =
              doExp (body, m)
          and doExp (CPS.Exp(ppt, e), m) = (case e 
                 of CPS.Let(xs, _, e) => 
                    doExp (e, addInfo (m, ppt, pptOfExp e))
                  | CPS.Fun(fbs, e) => let
                        val m = List.foldr (fn (fb, m) => doLambda(ppt, fb, m)) m fbs
                    in
                        doExp (e, addInfo (m, ppt, pptOfExp e))
                    end
                  | CPS.Cont(fb, e) => let
                        val m = doLambda (ppt, fb, m)
                    in
                        doExp (e, addInfo (m, ppt, pptOfExp e))
                    end
                  | CPS.If(_, e1, e2) => let
                        val m = addInfo (m, ppt, pptOfExp e1)
                        val m = addInfo (m, ppt, pptOfExp e2)
                        val m = doExp (e1, m)
                    in
                        doExp (e2, m)
                    end
                  | CPS.Switch(_, cases, dflt) => let
                        val caseExps = List.map #2 cases
                        val ppts = List.map pptOfExp caseExps
                        val m = List.foldr (fn (cppt, m) => addInfo (m, ppt, cppt)) m ppts
                        val m = List.foldr (fn (e, m) => doExp (e, m)) m caseExps
                    in
                        case dflt
                         of NONE => m
                          | SOME e => doExp (e, addInfo (m, ppt, pptOfExp e))
                    end
                  | CPS.Apply (f, _, _) => (
                    case CFACPS.valueOf f
                     of CFACPS.TOP => setTop (m, ppt)
                      | CFACPS.LAMBDAS ls => let
                            val ll = VSet.listItems ls
                            val pl = List.map bodyLocation ll
                        in
                            List.foldr (fn (fppt, m) => addInfo (m, ppt, fppt)) m pl
                        end
                      | CFACPS.BOT => m
                      | CFACPS.TUPLE _ => raise Fail (concat[CV.toString f, " is in an application position but is a tuple according to CFA."]))
                  | CPS.Throw (k, _) => (
                    case CFACPS.valueOf k
                     of CFACPS.TOP => setTop (m, ppt)
                      | CFACPS.LAMBDAS ls => let
                            val ll = VSet.listItems ls
                            val pl = List.map bodyLocation ll
                        in
                            List.foldr (fn (fppt, m) => addInfo (m, ppt, fppt)) m pl
                        end
                      | CFACPS.BOT => m
                      | CFACPS.TUPLE _ => raise Fail (concat[CV.toString k, " is in an application position but is a tuple according to CFA."]))
                (* end case *))
          in
            doLambda (bindingLocation f, body, PMap.empty)
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

    (* FIXME: This implementation is far too slow. *)
    fun computeReachability map = let
        fun addEntry (ppt, reaches, (b, map)) = (
            case reaches
             of TOP => (b,map)
              | r1 as REACHES ps => let
                    fun extend (ppt', (b, map)) = (
                        case (PMap.find (map, ppt'))
                         of NONE => raise Fail "missing ppt?"
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

    (* TODO: finish implementing *)
    fun compressSCC (p) = let
    	val ptlist = map #1 (PMap.listItemsi p)
	fun follow pt = 
	    case Option.valOf(PMap.find(p, pt))
	     of TOP => ptlist
	      |REACHES l => PSet.listItems l
	val components = SCC.topOrder'{roots = ptlist, follow = follow} (*why is this a type mismatch???*)
    in
	p
    end


    fun analyze (module as CPS.MODULE{body, ...}) = let
        val _ = setLocations module
        val neighbors = addNeighbors module
        val _ = if !debugFlg
                then print (concat["Number of program points: ",
                                   Int.toString (PMap.numItems neighbors),
                                   "\n"])
                else ()
        val SCCCompressed = compressSCC neighbors
        val reachability = computeReachability SCCCompressed
    in
        graph := reachability
    end

    (*
     * TODO: this needs to look up the program points in the
     * representative maps before checking them in the graph.
     *)
    fun pathExists (p1, p2) = (
        case PMap.find (!graph, p1)
         of NONE => false
          | SOME r => (case r
                        of REACHES ps => PSet.member (ps, p2)
                         | TOP => true))


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
