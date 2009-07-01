(* arity-raising.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This transformation combines useless variable elimination with argument
 * flattening (or arity-raising).
 *)

structure ArityRaising : sig

    val transform : CPS.module -> CPS.module

  end = struct

    structure PPt = ProgPt
    structure C = CPS
    structure CV = C.Var
    structure VSet = CV.Set
    structure U = CPSUtil
    structure CTy = CPSTy
    structure CFA = CFACPS
    structure ST = Stats

  (***** controls ******)
    val enableArityRaising = ref false
    val flatteningDebug = ref false

    val () = List.app (fn ctl => ControlRegistry.register CPSOptControls.registry {
              ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
              envName = NONE
            }) [
              Controls.control {
                  ctl = enableArityRaising,
                  name = "flatten",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "enable arity raising (argument flattening)"
                },
              Controls.control {
                  ctl = flatteningDebug,
                  name = "flatten-debug",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "debug arity raising (argument flattening)"
                  }
            ]


  (***** Statistics *****)
    val cntCandidateFun		= ST.newCounter "cps-arity:candidate-fn"
    val cntFlattenedFun		= ST.newCounter "cps-arity:flattened-fn"
    val cntSelElim		= ST.newCounter "cps-arity:select-elim"
    val cntAllocElim		= ST.newCounter "cps-arity:alloc-elim"
    val cntAllocIntro		= ST.newCounter "cps-arity:alloc-intro"

    val cntEscapingFuns         = ST.newCounter "cps-arity:escaping-funs"
    val cntUselessElim          = ST.newCounter "cps-arity:useless-elim"
    val cntUselessScanPasses    = ST.newCounter "cps-arity:useless-scan-passes"


  (***** Analysis *****)

  (* access paths for variables that are derived from function parameters *)
    datatype path
      = PARAM of int		(* ith function parameter *)
      | SEL of int * path	(* selection from parameter *)

  (* a reversed path is the parameter index paired with a list of the selectors *)
    type rev_path = (int * int list)

    fun pathToString p = let
	  fun toStringList (PARAM i) = [Int.toString i, "@"]
	    | toStringList (SEL(i, p)) = Int.toString i :: "." :: toStringList p
	  in
	    concat(rev(toStringList p))
	  end

  (* reverse a path to a list of integers.  The parameter index will be the first item
   * in the resulting list.
   *)
    fun pathToList p = let
	  fun toList (PARAM i, l) = (i, l)
	    | toList (SEL(i, q), l) = toList(q, i::l)
	  in
	    toList (p, [])
	  end

  (* construct a path from a reversed representation. *)
    fun listToPath (i, l) = let
	  fun toPath ([], p) = p
	    | toPath (j::l, p) = toPath(l, SEL(j, p))
	  in
	    toPath (l, PARAM i)
	  end

    datatype path_order = PathLess | PathPrefix | PathEq | PathGreater

  (* compare two reversed paths *)
    fun compareRevPath (p, q) = let
	  fun cmp ([], []) = PathEq
	    | cmp ([], _) = PathPrefix
	    | cmp (_, []) = PathGreater
	    | cmp (i::r, j::r') = (case Int.compare(i, j)
		 of LESS => PathLess
		  | EQUAL => cmp(r, r')
		  | GREATER => PathGreater
		(* end case *))
	  in
	    cmp ((op ::)p, (op ::)q)
	  end

  (* is a reversed path a prefix of another? *)
    fun isPrefix ((i, p) : rev_path, (j, q)) = if (i = j)
	  then let
	    fun isPrefix' ([], p) = true
	      | isPrefix' (_, []) = false
	      | isPrefix' ((i : int)::r, j::r') = (i = j) andalso isPrefix'(r, r')
	    in
	      isPrefix' (p, q)
	    end
	  else false

  (* path equality *)
    fun samePath (PARAM i, PARAM j) = (i = j)
      | samePath (SEL(i, p), SEL(j, q)) = (i = j) andalso samePath(p, q)
      | samePath _ = false

  (* a lexical ordering that treats the path root as most significant *)
    fun comparePath (p, q) = let
	  val (i, p) = pathToList p
	  val (j, q) = pathToList q
	  in
	    case Int.compare(i, j)
	     of EQUAL => List.collate Int.compare (p, q)
	      | order => order
	    (* end case *)
	  end

  (* is path p derived from q? *)
    fun isDerivedFrom (p, q) = isPrefix (pathToList p, pathToList q)
	  
    structure PMap = RedBlackMapFn (
      struct
	type ord_key = path
	val compare = comparePath
      end)

    fun lookupPath (pmap, p) = (case PMap.find(pmap, p)
	   of NONE => raise Fail(concat["lookupPath(-, ", pathToString p, ")"])
	    | SOME cnt => cnt
	  (* end case *))

  (* The signature of a function f is a list of reversed paths (i.e., int lists) that represent
   * the function's flattened arguments.  It has the following properties:
   *  1) no path is derived from another in the list,
   *  2) the use counts of the paths are > 0
   *  3) for any path p in dom(pmap) with pmap(p) > 0, either p
   *     is in the list or is derived from a path in the list.
   *)
    type fun_sig = rev_path list

    fun sigToString sign = let
	  fun f [] = [")"]
	    | f [s] = [pathToString(listToPath s), ")"]
	    | f (s::r) = pathToString(listToPath s) :: ", " :: f r
	  in
	    String.concat ("(" :: f sign)
	  end

  (* compute a maximal signature of a candidate function from its pmap.  The maximal signature
   * includes any path that has a non-zero count.
   *)
    fun computeMaxSig pmap =
	  PMap.foldri (fn (p, ref 0, l) => l | (p, _, l) => (pathToList p)::l) [] pmap

  (* Compute the signature of a candidate function from its pmap. *)
    fun computeSig pmap = let
	(* filter out paths that are derived from others on the list; for this
	 * process, we rely on the ordering used to structure the pmap.
	 *)
	  fun filter ([], _, l) = List.rev l
	    | filter (p::r, q, l) = if isPrefix(q, p)
		then filter (r, q, l)
		else filter (r, p, p::l)
	  in
	    filter (computeMaxSig pmap, (~1, []), [])
	  end

  (* merge two signatures to have a common calling convention.
   * We use this operation to handle the case where there are multiple
   * possible functions that can be called at a single call site.
   * If we trivially flattened, we could end up with different parameter
   * conventions and incompatible calls. This unifies the convention
   * across the two functions.
   *)
    fun sigMeet (sig1, sig2) = let
	  fun removeDerivedPaths (p, q::qs) = if isPrefix(p, q)
		then removeDerivedPaths (p, qs)
		else q::qs
	    | removeDerivedPaths (_, []) = []
	  fun f (p::ps, q::qs, mergedSig) = (case compareRevPath(p, q)
		 of PathLess => f(ps, q::qs, p::mergedSig)
		  | PathPrefix => f (ps, removeDerivedPaths (p, qs), p::mergedSig)
		  | PathEq => f (ps, qs, p::mergedSig)
		  | PathGreater => if isPrefix (q, p)
		      then f (removeDerivedPaths (q, ps), qs, q::mergedSig)
		      else f (p::ps, qs, q::mergedSig)
		(* end case *))
	    | f (ps, [], mergedSig) = List.revAppend(ps, mergedSig)
	    | f ([], qs, mergedSig) = List.revAppend(qs, mergedSig)
	  in
	    List.rev (f (sig1, sig2, []))
	  end

    datatype call_site = SITE of {
	    enclFn : C.var option,	(* enclosing function of call site; NONE if it is not *)
					(* a candidate. *)
	    ppt : ProgPt.ppt,		(* program-point of call site *)
	    callees : C.var list,	(* the candidate functions that f can be bound to *)
	    f : C.var,			(* variable bound to the function at the call site *)
	    args : C.var list		(* arguments to call *)
	  }

    fun siteToString (SITE{enclFn, f, args, ...}) = concat[
	    CV.toString f, " (", String.concatWith ", " (List.map CV.toString args),
	    case enclFn of NONE => ")" | SOME g => concat[") in ", CV.toString g]
	  ]


  (* a derived parameter is either a local variable in the function or a potential
   * argument to some call that whose arguments will be flattened.
   *)
    datatype derived_param
      = VAR of C.var			(* a local variable derived from a formal parameter *)
      | ARG of call_site * path		(* an argument to a candidate function *)

    fun paramToString (VAR x) = CV.toString x
      | paramToString (ARG(SITE{args, ...}, path)) = let
	  val (i, l) = pathToList path
	  in
	    concat(
	      CV.toString(List.nth(args, i)) ::
		List.foldr (fn (j, l) => "."::Int.toString j::l) [] l)
	  end

  (* a finite map keyed by derived parameters *)
    structure ParamMap = RedBlackMapFn (
      struct
	type ord_key = derived_param
	fun compare (VAR x, VAR y) = CV.compare(x, y)
	  | compare (VAR _, ARG _) = LESS
	  | compare (ARG _, VAR _) = GREATER
	  | compare (ARG(SITE{ppt=ppt1, ...}, path1), ARG(SITE{ppt=ppt2, ...}, path2)) = (
	      case ProgPt.compare(ppt1, ppt2)
	       of EQUAL => comparePath (path1, path2)
		| order => order
	      (* end case *))
      end)

    (* Compute param list from the final signature,
     * vmap of the VAR->PATH mappings, and original params
     *)
    fun computeParamList (origParams, vmap, sign) = let
        fun computeParam paramPath = 
            case paramPath
             of (i, []) => List.nth (origParams, i)
              | (i, l) => let
                    val path = listToPath (i, l)
                    val vmap' = ParamMap.filter (fn (p) => comparePath (p,path) = EQUAL) vmap
                in
                    case ParamMap.firsti vmap'
                     of SOME(VAR(x), _) => x
                      | _ => raise Fail ("Signature contains invalid path")
                end
    in
        List.map computeParam sign
    end

  (* property for tracking the call sites of candidate functions; we also use this
   * property to distinguish candidate functions from non-candidate functions.
   *)
    local
      val {clrFn, setFn, peekFn, ...} = CV.newProp (fn f => ref([] : call_site list))
    in
  (* get the list of call sites of a function; [] for non-candidates *)
    fun getSites f = (case peekFn f
	   of NONE => []
	    | SOME l => !l
	  (* end case *))

  (* if a bound variable is a candidate function, then mark it
   * by initializing its call-site list
   *
   * A function f is a candidate for flattening if it satisfies the following
   * properties:
   *
   *	1) f's argument type is a non-mutable tuple
   *	2) and either
   *	      a) f has lexically known application sites
   *	      b) f has a member of a set of known functions with known call sites
   *)
    fun markCandidate f = let
	  fun mark () =
		if ((CV.appCntOf f > 0)
		orelse (case CFA.equivalentFuns f of [] => false | _ => true))
		  then (case CV.typeOf f
		     of CTy.T_Fun(tys, _) =>
			  if List.exists (fn CTy.T_Tuple(false, _) => true | _ => false) tys
			    then (
                              ST.tick cntCandidateFun;
			      setFn (f, ref []))
			    else ()
		      | _ => ()
		    (* end case *))
		  else () 
	  in
	    case CV.kindOf f
	     of C.VK_Fun _ => mark ()
	      | C.VK_Cont _ => mark ()
	      | _ => ()
	    (* end case *)
	  end
    
    (* is a function a candidate? *)
    fun isCandidate f = Option.isSome(peekFn f)

  (* add an application site to any candidate functions that are called from
   * the site.
   *)
    fun addCallSite (enclFn, ppt, f, args) = (case CFA.valueOf f
	   of CFA.LAMBDAS gs => let
		val callees = VSet.listItems gs
		val site = SITE{enclFn = enclFn, ppt = ppt, callees = callees, f = f, args = args}
		fun add g = (case (peekFn g)
		       of NONE => ()
			| SOME l => (l := site :: !l)
		      (* end case *))
		in
		  List.app add callees
		end
	    | _ => ()
	  (* end case *))
    end (* local *)

  (* Candidate info property *)
    type info = {
	  vmap : path ParamMap.map,
	  pmap : int ref PMap.map,
	  sign : fun_sig,
          params : C.var list,
          rets : C.var list,
          flat : C.var option,
          flatParams : C.var list option
	}
    local
      val {clrFn, getFn : CV.var -> info, setFn, ...} =
	    CV.newProp (fn x => raise Fail ((CV.toString x) ^ " is not a candidate"))
    in
    val clearInfo = clrFn
    val getInfo = getFn
    fun setInfo (f, vmap, pmap, params, rets, flat, flatParams) =
	  setFn (f, {vmap=vmap, pmap=pmap, sign=computeMaxSig pmap, params=params, rets=rets, flat=flat, flatParams=flatParams});
    end

(* +DEBUG *)
    fun printCandidate f = let
	  val {vmap, pmap, sign, params, flat, ...} = getInfo f
	  in
	    print(concat["candidate ", CV.toString f, " : ", sigToString sign, "\n"]);
	    print "  vmap:\n";
	    ParamMap.appi
	      (fn (x, p) => print(concat[
		  "    ", paramToString x, " --> ", pathToString p, ", cnt = ",
		  Int.toString(!(valOf(PMap.find(pmap, p)))), "\n"
		])) vmap;
	    print "  call sites:\n";
	    List.app (fn site => print(concat["    ", siteToString site, "\n"])) (getSites f);
            print (" new name: " ^ (case flat of SOME(x) => CV.toString x | _ => "NONE") ^ " param names:");
            List.app (fn v => print (CV.toString v ^ " ")) (computeParamList (params, vmap, sign)); print "\n"
	  end
(* -DEBUG *)

    fun addToRef (r, n) = r := !r + n

  (* property for tracking bindings to function bodies, used in useful
   * variable analysis
   *)
    local
      val {setFn, getFn, peekFn, ...} = CV.newProp (fn f => NONE : C.lambda option)
    in
    fun setFB (f,b) = setFn (f, SOME(b))
    fun getFB f = getFn f
    end

    (* property mapping from a variable to its defining function. This is used
     * during the flattening pass over the captured variables of a function to
     * see if there is a lexically equivalent shorter name for the same
     * access path.
     *)
    local
        val {setFn, getFn, ...} = CV.newProp (fn f => (NONE : C.var option))
    in
    fun setParent (f,p) = setFn (f, SOME(p))
    fun getParent f = getFn f
    end

    val escaping = ref ([] : C.var list)

  (* the first part of the analysis is to gather all of the candidate functions and
   * their call sites.  We also compute the initial vmaps and pmaps based on the selects
   * in the candidate-function bodies.
   *)
    fun gather (module as C.MODULE{body, ...}) = let
	(* list of candidate functions *)
	  val candidates = ref []
	(* analyse a bound function or continuation *)
	  fun analyseLambdas fbs = let
	      fun analyseFB (fb as C.FB{f, body, ...}) = (
                  setFB(f,fb) ;
                  if CFACPS.isEscaping f
                  then (ST.tick cntEscapingFuns;
                        escaping := f :: !escaping)
                  else () ;
                  if isCandidate f
		  then analyseCandidate fb
		  else walkExp body)
	      in
		List.app analyseFB fbs
	      end
	(* analyse the body of a candidate *)
	  and analyseCandidate (C.FB{f, params, rets, body, ...}) = let
	      (* construct an initial mapping from the parameters to their paths
	       * and from their paths to their use counts.
	       *)
		val (vmap, pmap) = let
		      fun f (_, [], vm, pm) = (vm, pm)
			| f (i, x::r, vm, pm) = let
			    val p = PARAM i
			    val vm = ParamMap.insert(vm, VAR x, p)
			    val pm = PMap.insert(pm, p, ref(CV.useCount x))
			    in
			      f (i+1, r, vm, pm)
			    end
		      in
			f (0, params, ParamMap.empty, PMap.empty)
		      end
                val _ = List.app (fn p => setParent (p, f)) params
	      (* analyse the body of the candidate function *)
		fun doExp (vmap, pmap, C.Exp(ppt, t), carefully) = (case t
		       of (C.Let([x], C.Select(i, y), e)) => (
                            setParent (x, f) ;
			    case ParamMap.find(vmap, VAR y)
			     of NONE => doExp(vmap, pmap, e, carefully)
			      | SOME p => let
				  val q = SEL(i, p)
				  val vmap' = ParamMap.insert(vmap, VAR x, q)
				(* decrement p's count *)
				  val cnt = lookupPath(pmap, p)
				  val _ = addToRef (cnt, ~1)
				(* either add q to the path map or update its count *)
				  val (vmap, pmap) = (case PMap.find(pmap, q)
					 of NONE => (
                                            if carefully
                                            then (addToRef (cnt, 1) ; (vmap, pmap))
                                            else (vmap', PMap.insert(pmap, q, ref(CV.useCount x))))
					  | SOME cnt => (addToRef(cnt, CV.useCount x);
                                                         (vmap', pmap))
					(* end case *))
				  in
				    doExp (vmap, pmap, e, carefully)
				  end)
			| (C.Let(_, _, e)) => doExp (vmap, pmap, e, carefully)
			| (C.Fun(fbs, e)) => (
			    analyseLambdas fbs;
			    doExp (vmap, pmap, e, carefully))
			| (C.Cont(fb, e)) => (
			    analyseLambdas [fb];
			    doExp (vmap, pmap, e, carefully))
			| (C.If(x, e1, e2)) => let
			  (* Conditional code: can't add guarded variable accesses
			   * i.e., can end up adding a param for y that was within
			   * an 'if not(null x) then let y = #1(x) else 2'
			   *)
			    val (vmap, pmap) = doExp (vmap, pmap, e1, true)
			    val (vmap, pmap) = doExp (vmap, pmap, e2, true)
			    in
                              (vmap, pmap)
			    end
			| (C.Switch(x, cases, dflt)) => let
			    val (vmap, pmap) = (case dflt
				   of SOME e => doExp(vmap, pmap, e, true)
				    | NONE => (vmap, pmap)
				  (* end case *))
			    fun doCase ((_, e), (vmap, pmap)) = doExp(vmap, pmap, e, true)
			    in
			      List.foldl doCase (vmap, pmap) cases
			    end
			| (C.Apply(g, args, _)) => (
			    addCallSite (SOME f, ppt, g, args);
			    (vmap, pmap))
			| (C.Throw(k, args)) => (
			    addCallSite (SOME f, ppt, k, args);
			    (vmap, pmap))
		      (* end case *))
		val (vmap, pmap) = doExp(vmap, pmap, body, false)
	      (* the "argument shape" of f is a list of paths such that
	       *  1) no path is derived from another in the list,
	       *  2) the use counts of the paths are > 0
	       *  3) for any path p in dom(pmap) with pmap(p) > 0, either p
	       *     is in the list or is derived from a path in the list.
	       *)
		val args = let
		    (* construct an initial list of paths with non-zero use counts in lexical
		     * order.
		     *)
		      val paths = PMap.foldri (fn (p, ref 0, l) => l | (p, _, l) => p::l) [] pmap
		    (* filter out paths that are derived from others on the list; for this
		     * process, we rely on the ordering used to structure the pmap.
		     *)
		      fun filter ([], _, l) = List.rev l
			| filter (p::r, q, l) = if isDerivedFrom(q, p)
			    then filter (r, q, l)
			    else filter (r, p, p::l)
		      in
			filter (paths, PARAM ~1, [])
		      end
		in
		  ST.tick cntCandidateFun;
		  setInfo (f, vmap, pmap, params, rets, NONE, NONE);
		  candidates := f :: !candidates
		end (* analyseCandidate *)
	(* walk an expression looking for candidates *)
	  and walkExp (C.Exp(ppt, t)) = (case t
		 of (C.Let(_, _, e)) => walkExp e
		  | (C.Fun(fbs, e)) => (
		      analyseLambdas fbs;
		      walkExp e)
		  | (C.Cont(fb, e)) => (
		      analyseLambdas [fb];
		      walkExp e)
		  | (C.If(x, e1, e2)) => (walkExp e1; walkExp e2)
		  | (C.Switch(x, cases, dflt)) => (
		      List.app (walkExp o #2) cases;
		      Option.app walkExp dflt)
		  | (C.Apply(g, args, _)) => addCallSite (NONE, ppt, g, args)
		  | (C.Throw(k, args)) => addCallSite (NONE, ppt, k, args)
		(* end case *))
	  in
	  (* first we mark the candidate functions *)
	    U.applyToBoundVars markCandidate module;
	  (* first analyse the module to determine the candidate functions, their
	   * call sites, and their parameter access patterns.
	   *)
	    analyseLambdas [body];
	    !candidates
	  end

  (* Attempt to come up with a combined signature via sigMeet. Note
   * that non-candidate calls cancel out any attempt to further simplify
   * the signature.
   *)
    fun sigOfFuns [] = raise Fail "no functions"
      | sigOfFuns [f] = #sign(getInfo f)
      | sigOfFuns (l as f::r) = let
            val canMeet = List.foldl (fn (g, b) => if (b) then (isCandidate g) else b) true l
        in
            if (canMeet) then
	        List.foldl (fn (g, sign) => sigMeet(#sign(getInfo g), sign)) (#sign(getInfo f)) r
            else
                []
        end

  (* for each candidate function, analyse the arguments of its call sites *)
    fun analyse m = let
	  val candidates = gather m
	  in
            candidates
	  end

  (***** Usefulness *****)

  (* property for tracking the usefulness of a variable. All variables are assumed
   * useless until proven otherwise. Note that variables don't transition back to
   * useless once they've been identified as useful.
   *)
    local
      val {setFn, getFn, peekFn, ...} = CV.newProp (fn f => false)
    in
    fun setUseful f = setFn (f, true)
    fun getUseful f = getFn f
    end

    
  (* property for whether or not we've processed this function yet
   * the int corresponds to the round number, since we iterate scanning
   * until a fixpoint is reached
   *)
    datatype processing_state = UNPROCESSED | INPROCESS of int | DONE of int
    local
      val {setFn, getFn, peekFn, ...} = CV.newProp (fn f => UNPROCESSED)
    in
    fun setInProcess (f,i) = setFn (f, INPROCESS(i))
    fun setDone (f,i) = setFn (f, DONE(i))
    fun getProcessState f = getFn f
    end

    fun scanUseful (m, round) = let
	  val C.MODULE{body=body,...} = m
	  val C.FB{f=main,params,rets,body} = body
	  val usefuls = ref (!escaping @ [main])
	  val changed = ref false
	  fun markUseful f = (
		if getUseful f then ()
		else (
		  if !flatteningDebug
		    then print (concat[
			CV.toString f, " is useful in round",
			Int.toString round, "\n"
		      ])
		    else ();
                  changed := true ;
                  setUseful f);
		case CV.kindOf f
		 of C.VK_Fun(_) =>
		    if not (List.exists (fn x => CV.same(x,f)) (!usefuls))
		      then usefuls := f :: !usefuls
		      else ()
		  | C.VK_Cont(_) =>
		    if not (List.exists (fn x => CV.same(x,f)) (!usefuls))
		      then usefuls := f :: !usefuls
		      else ()
		  | _ => ()
		(* end case *))
	  fun markCorresponding (a,b) = if getUseful a then markUseful b else ()
	  fun isEffectful rhs = (case rhs
		 of C.Prim (primop) => not(PrimUtil.isPure primop)
		  | C.Update _ => true
(* FIXME: should check for pure C functions *)
		  | C.CCall _ => true
		  | C.VPStore _ => true
		  | _ => false
	       (* end case *))
	  fun processLambda f = let
		val SOME(C.FB {body,params,rets,...}) = getFB f
		fun processBody () = (
		      setInProcess (f, round);
		      processExp body;
		      setDone (f, round))
		in
		  case getProcessState f
		   of UNPROCESSED => processBody()
		    | DONE(i) => if i = round then () else processBody()
		    | INPROCESS(i) =>  if i = round
			then ()
			 (* For recursive function calls, we just
                          * return empty, assuming that we will pick up
                          * any additional information from the function
                          * when we return from the DFS.
			  *)
			else processBody()
		  (* end case *)
		end
        and processExp (C.Exp (_, term)) = (case term
	    (* Scanning for usefulness is done bottom-up, so we process the body
	     * before bindings at hand.
	     *)
	       of C.Let ([var], rhs, body) => (
		    processExp body;
		    if isEffectful rhs then setUseful var else ();
		    if getUseful var then processRhs rhs else ())
		| C.Let ([], rhs, body) => (
		    processExp body;
		    processRhs rhs)
		| C.Let (_, rhs, body) => (
		    processExp body;
		    processRhs rhs)
		| C.Fun (_, body) => processExp body
		| C.Cont (_, body) => processExp body
		| C.If (var, e1, e2) => (
		    processExp e1;
		    processExp e2;
		    markUseful var)
		| C.Switch (x, cases, dflt) => (
		    List.app (fn (_, e) => processExp e) cases;
		    Option.app processExp dflt;
		    markUseful x)
		(* TODO: maybe check equivalentFns instead of just the straight binding? *)
		| C.Apply (f, args, rets) => (
		    case getFB f
		     of SOME(C.FB{params,rets=conts,...}) => let
			  val pairedArgs = ListPair.zip (params, args)
			  val pairedConts = ListPair.zip (conts, rets)
			  in
			    markUseful f;
			    processLambda f;
			    List.app markCorresponding pairedArgs ;
			    List.app markCorresponding pairedConts 
			  end
		      | NONE => (
			  List.app markUseful args;
			  List.app markUseful rets;
			  markUseful f)
		   (* end case *))	     
		| C.Throw (f, args) => (case getFB f
		     of SOME(C.FB{params,...}) => (let
			  val pairedArgs = ListPair.zip (params, args)
			  in
			    markUseful f;
			    processLambda f;
			    List.app markCorresponding pairedArgs
			  end)
		      | NONE => (List.app markUseful args; markUseful f)
		    (* end case *))
	      (* end case *))

	and processRhs rhs = (case rhs
	       of C.Var (vars) => List.app markUseful vars
		| C.Cast (_, v) => markUseful v
		| C.Const (_, _) => ()
		| C.Select (_, v) => markUseful v
		| C.Update (_, v, x) => (markUseful v; markUseful x)
		| C.AddrOf (_, v) => markUseful v
		| C.Alloc (_, vars) => List.app markUseful vars
		| C.Promote (v) => markUseful v
		| C.Prim p => PrimUtil.app markUseful p
		| C.CCall (f, args) => (markUseful f ; List.app markUseful args)
		| C.HostVProc => ()
		| C.VPLoad (_, v) => markUseful v
		| C.VPStore (_, v1, v2) => (markUseful v1; markUseful v2)
		| C.VPAddr (_, v) => markUseful v
	      (* end case *))
            
      (* We need to iterate to a fixpoint because it's frequent to have a
       * set of locals that are used in function bodies that we don't
       * realize correspond a useful variable binding until after 
       * we've already finished processing the binding for the current var
       *)
	fun process () = let
            val head = hd (!usefuls)
	    in
	      usefuls := tl (!usefuls);
	      processLambda (head) ;
	      if not(null(!usefuls))
		then process ()
	      else if !changed
		then scanUseful (m, round+1)
		else ()
	    end
	in
	  ST.tick cntUselessScanPasses;
	  process()
	end


  (***** Flattening *****)

  (* property for tracking whether a function was created during flattening.
   * this is important because we don't re-run CFA, so isEscaping is invalid
   * for it (says true even though it's false)
   *)
    local
      val {setFn, getFn, peekFn, ...} = CV.newProp (fn f => false)
    in
    fun setFlat f = setFn (f, true)
    fun isFlat f = getFn f
    end

    (* walk down the tree and:
     * - turn candidate functions into a specialized version and the
     * original, which calls the specialized with the extra arguments
     * - in the body of the specialized version, remove any variables
     *)
    fun flatten (C.MODULE{name,externs,body=(C.FB{f=main,params=modParams,rets=modRets,body=modBody})}) = let
          fun genUseless(useless, body) = let
              val typ = CV.typeOf useless
              fun cast () = let
                  val temp = CV.new ("any", CTy.T_Any)
              in
                  C.mkLet ([temp], C.Const(Literal.Enum(0w0), CTy.T_Any),
                           C.mkLet ([useless], C.Cast (typ, temp), body))
              end
          in
              case typ
               of CTy.T_Any => C.mkLet ([useless], C.Const(Literal.Enum(0w0), CTy.T_Any), body)
                | CTy.T_Enum (_) => C.mkLet ([useless], C.Const(Literal.Enum(0w0), typ), body)
                | CTy.T_Raw (_) => C.mkLet ([useless], C.Const(Literal.Enum(0w0), typ), body)
                | CTy.T_Tuple (_, _) => cast ()
                | CTy.T_Addr (_) => cast ()
                | CTy.T_Fun (_, _) => cast ()
                | CTy.T_CFun (_) => cast ()
                | CTy.T_VProc => cast()
          end
	  fun flattenApplyThrow (ppt, g, args, retArgs) = if isCandidate g
		then let 
		  val {sign, params, rets, flat=SOME(f), flatParams=SOME(fParams), ...} = getInfo g
		  fun genCall (sign, params, newArgs, progress) =
                        if null sign
			  then (case retArgs
			     of SOME(retArgs) => let
                                    val filteredRetArgs = ListPair.foldr
                                        (fn (a,b,rr) => if getUseful a
                                                        then b::rr
                                                        else rr)
                                        []
                                        (rets, retArgs)
                                in
                                    C.Exp(ppt, C.Apply(f, rev newArgs, filteredRetArgs))
                                end
			      | NONE => C.Exp(ppt, C.Throw(f, rev newArgs))
			    (* end case *))
			  else let
			    fun genResult (varBase, path) =
                                  if length path = 0
				  then genCall (tl sign, tl params, varBase :: newArgs, NONE)
				  else let
				    val newType = (case CV.typeOf varBase
					   of CTy.T_Tuple(_, types) => List.nth (types, (hd path))
					    | CTy.T_Any => raise Fail (concat[
						  "SEL into any of var: ", CV.toString varBase,
						  " probably flattening var that isn't guaranteed to be that type.\n"
						])
					    | _ => raise Fail "SEL from non tuple/any type"
					  (* end case *))
				    val newVar = CV.new ("letFlat", newType)
				    val rhs = C.Select (hd path, varBase)
				    in
				      if length path = 1
					then C.mkLet ([newVar], rhs,
					    genCall (tl sign, tl params, newVar :: newArgs, NONE))
					else C.mkLet ([newVar], rhs,
					    genCall (sign, params, newArgs, SOME (newVar, tl path)))
                                        end
			    in
			      case progress
			       of SOME(base,l) => genResult (base, l)
				| NONE => let
				    val (whichBase, path)::_ = sign
                                    val param = hd params
				    val varBase = List.nth (args, whichBase)
				    in
                                      (* Flattened functions will lose any useless parameters.
                                       * We know they only have known call sites (by construction).
                                       *)
                                      if shouldSkipUseless param
                                      then genCall (tl sign, tl params, newArgs, NONE)
				      else genResult (varBase, path)
				    end
			      (* end case *)
                            end
		  in
                    genCall (sign, fParams, [], NONE)
		  end
		else let (* Not a call to a candidate function. Keep arguments in place *)
                        fun translateArgs (v::vl, accum, final) =
                            if shouldSkipUseless v
                            then (let
                                      val useless = CV.copy v
                                  in
                                      genUseless (useless, translateArgs (vl, useless::accum, final))
                                  end)
                            else translateArgs (vl, v::accum, final)
                          | translateArgs ([], accum, final) =
                            final (rev accum)
                    in
                        case retArgs
		         of SOME(retArgs) => translateArgs (args, [],
                                           fn x => translateArgs (retArgs, [],
                                              fn y => C.Exp(ppt, C.Apply(g, x, y))))
		          | NONE => translateArgs (args, [],
                                                   fn x => C.Exp (ppt, C.Throw(g, x)))
                    end
          and useCountOfVar(encl, v) = if not(isCandidate(encl))
		then ~1
		else let
		  val {vmap, pmap,...} = getInfo encl
                  in
		    case ParamMap.find (vmap, VAR v)
		     of SOME(path) => !(valOf(PMap.find(pmap, path)))
		      | NONE => ~2
		    (* end case *)
                  end
          and shouldSkipUseless (v) = if getUseful v
		then false
		else (ST.tick (cntUselessElim); true)
          and findAlias (v, encl, i, y) =
              if not(isCandidate encl)
              then NONE
              else let
                      fun findInSign (n, s::sign, path, params) =
                          if samePath(path, listToPath s) then SOME(List.nth (params, n)) else findInSign(n+1,sign, path, params)
                        | findInSign (n, [], _, _) = NONE
                      val {vmap,sign, flatParams=SOME(newParams), ...} = getInfo encl
                  in
                      case ParamMap.find (vmap, VAR (v))
                       of SOME(path) => findInSign(0, sign, path, newParams)
                        | NONE => (* look up path in defining function of selectee *)
                          (case getParent y
                            of SOME (p) =>
                               if not(isCandidate p)
                               then NONE
                               else let
                                       val {vmap, pmap, sign, flatParams=SOME(newParams),...} = getInfo p
                                   in
                                       case ParamMap.find (vmap, VAR (y))
                                        of SOME(path) => let
                                               val fullPath = SEL(i, path)
                                           in
                                               findInSign(0, sign, fullPath, newParams)
                                           end
                                         | NONE => NONE
                                       
                                   end
                             | NONE => NONE
                          (* end case *))
                  end
	  and walkExp(encl, newParams, C.Exp(ppt,e)) = (case e
		 of (C.Let([v], rhs, e)) => (
		    (* If v has been promoted to a param or its
		     * use count is now zero, omit it
		     *)
		      if List.exists (fn x => CV.same(x, v)) newParams
		      orelse useCountOfVar (encl, v) = 0
		      orelse shouldSkipUseless (v)
			then walkExp (encl, newParams, e)
			else (case rhs
                               of C.Select(i, y) => (
                                  case findAlias (v, encl, i, y)
                                   of SOME(v') => C.mkLet ([v], C.Var ([v']),
                                                           walkExp (encl, newParams, e))
                                    | NONE => C.mkLet([v], rhs, walkExp (encl, newParams, e))
                                  (* end case *))
                                | _ => C.mkLet([v], rhs, walkExp (encl, newParams, e))
                             (* end case *)))
		  | (C.Let(vars, rhs, e)) =>
		      if List.exists (fn v => List.exists (fn x => CV.same(x, v)) newParams) vars
			then raise Fail ("Can't lift variable from multi-bind on LHS of let")
			else C.Exp(ppt, C.Let(vars, rhs, walkExp (encl, newParams, e)))
		  | (C.Fun(fbs, e)) => let
		      val newfuns = List.foldr (fn (f,rr) => handleLambda (f,false) @ rr) [] fbs
                      val newfuns' = List.filter (fn (C.FB{f,...}) => not(shouldSkipUseless f)) newfuns
		      in
                        if null (newfuns')
                        then walkExp (encl, newParams, e)
                        else C.mkFun (newfuns', walkExp (encl, newParams, e))
		      end
		  | (C.Cont(fb, e)) => (case handleLambda (fb, true)
		       of [fl as C.FB{f=fl',...}, stb as C.FB{f=stb',...}] => let
			    val inner = (if shouldSkipUseless stb'
					 then walkExp (encl, newParams, e)
					 else C.mkCont (stb, walkExp (encl, newParams, e)))
			    in
                              if shouldSkipUseless fl'
				then inner
				else C.mkCont(fl, inner)
			    end
			| [single as C.FB{f=single',...}] => (
			    if shouldSkipUseless single'
			      then walkExp (encl, newParams, e)
			      else C.mkCont(single, walkExp (encl, newParams, e)))
			| _ => raise Fail "Invalid return from handleLambda"
		      (* end case *))
		  | (C.If(x, e1, e2)) =>
		      C.Exp(ppt, C.If(x, walkExp (encl, newParams, e1), walkExp (encl, newParams, e2)))
		  | (C.Switch(x, cases, dflt)) =>
		      C.Exp(ppt, C.Switch(x,
			List.map (fn (tag,exp) => (tag,walkExp (encl, newParams, exp))) cases,
			Option.map (fn (f) => walkExp (encl, newParams, f)) dflt))
		  | (C.Apply(g, args, rets)) => flattenApplyThrow (ppt, g, args, SOME(rets))
		  | (C.Throw(k, args)) => flattenApplyThrow (ppt, k, args, NONE)
		(* end case *))

        (* Returns flattened version of candidate functions *)
        and handleLambda(func as C.FB{f, params, rets, body}, isCont) =
	      if not (isCandidate f)
                then [C.FB{f=f, params=params, rets=rets, body=walkExp (f, params, body)}]
		else let
		  val {vmap, pmap, params, rets, sign, ...} = getInfo f
		  val newParams = computeParamList (params, vmap, sign)
                  val newParams = List.filter getUseful newParams
                  val newRets = List.filter getUseful rets
		  val newType = CTy.T_Fun (List.map CV.typeOf newParams, List.map CV.typeOf newRets)
		  val flat = CV.new ("flat"^CV.nameOf f, newType)
                  val _ = setFlat (flat)
                  val _ = if getUseful f then setUseful flat else ()
		  val _ = setInfo (f, vmap, pmap, params, rets, SOME flat, SOME newParams)
		  val body = walkExp (f, newParams, body)
		(* Create a stub with the old name that just SEL's and jumps to the flat version. *)
		  val paramCopy = List.map CV.copy params
		  val retsCopy = List.map CV.copy rets
                  val _ = ListPair.app (fn (a,b) => if getUseful a then setUseful b else ()) (params, paramCopy)
                  val _ = ListPair.app (fn (a,b) => if getUseful a then setUseful b else ()) (rets, retsCopy)
		  val stub = flattenApplyThrow (
			ProgPt.new(), f, paramCopy,
			if not(isCont) then SOME retsCopy else NONE) 
		  val stubLambda = C.mkLambda (C.FB{f=f, params=paramCopy, rets=retsCopy, body=stub})
		  val lambda = C.mkLambda(C.FB{f=flat, params=newParams, rets=newRets, body=body})
		  in
                        setFB (f, stubLambda);
                        setFB (flat, lambda);
                        [lambda, stubLambda]
		  end
	in
	  C.MODULE{
	      name=name, externs=externs,
	      body = C.mkLambda(C.FB{
                  f=main,params=modParams,rets=modRets,
                  body= walkExp (main, modParams, modBody)
		})
	    }
	end
    (*
     * Flatten+useless elim can leave around useCnt=0 params on flattened functions.
     * This removes them, cleans up the types, and cleans up the call sites.
     *)
    fun cleanupParams (C.MODULE{name,externs,body=(C.FB{f=main,params=modParams,rets=modRets,body=modBody})}) = let
        fun cleanupExp (exp as C.Exp(ppt, e)) = (
            case e
             of C.Let (vars, rhs, e) => C.mkLet (vars, rhs, cleanupExp e)
              | C.Fun (lambdas, body) => C.mkFun(List.map cleanupLambda lambdas, cleanupExp body)
              | C.Cont (f, body) => C.mkCont (cleanupLambda f, cleanupExp body)
              | C.If (v, e1, e2) => C.mkIf (v, cleanupExp e1, cleanupExp e2)
              | C.Switch (v, cases, body) => C.mkSwitch(v, List.map (fn (tag,e) => (tag, cleanupExp e))
                                                                    cases, Option.map cleanupExp body)
              | C.Apply (f, args, retArgs) => (
                if not(isFlat f) 
                then exp
                else case getFB f
                      of SOME(C.FB{params,rets,...}) =>
                         C.mkApply(f,
                                   ListPair.foldr (fn (a,b,rr) => if CV.useCount a > 0 then b::rr
                                                                  else (Census.decUseCnt b ; rr))
                                   [] (params, args),
                                   ListPair.foldr (fn (a,b,rr) => if CV.useCount a > 0 then b::rr
                                                                  else (Census.decUseCnt b ; rr))
                                   [] (rets, retArgs))
                                   
                       | NONE => exp
                )
              | C.Throw (k, args) => (
                if not (isFlat k) 
                then exp
                else case getFB k
                      of SOME(C.FB{params,...}) => 
                         C.mkThrow(k,
                                   ListPair.foldr (fn (a,b,rr) => if CV.useCount a > 0 then b::rr
                                                                  else (Census.decUseCnt b ; rr))
                                                  [] (params, args))
                       | NONE => exp
                (* end case *)))
        and cleanupLambda (lambda as C.FB{f, params, rets, body}) =
            if not (isFlat f) 
            then C.mkLambda(C.FB{f=f,params=params,rets=rets,body=cleanupExp body})
            else let
                    val params' = List.filter (fn p => (CV.useCount p) > 0) params
                    val rets' = List.filter (fn p => (CV.useCount p) > 0) rets
		    val newType = CTy.T_Fun (List.map CV.typeOf params', List.map CV.typeOf rets')
                    val _ = CV.setType (f, newType)
                in
                    
                    C.mkLambda(C.FB{f=f,params=params',rets=rets',body=cleanupExp body})
                end
    in
        C.MODULE{
	name=name, externs=externs,
	body = C.mkLambda(C.FB{
                          f=main,params=modParams,rets=modRets,
                          body= cleanupExp (modBody)
		         })
	}
    end

  (***** Transformation *****)

    fun transform m = if !enableArityRaising
	  then let
	    val candidates = analyse m
            val _ = scanUseful (m, 0)
	    val m' = flatten m
	    in
	      if !flatteningDebug
		then List.app printCandidate candidates
		else ();
(* FIXME: should mainain census counts! *)
	      Census.census m';
              cleanupParams m'
	    end
	  else m

  end
