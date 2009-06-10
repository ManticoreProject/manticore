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
          flat : C.var option
	}
    local
      val {clrFn, getFn : CV.var -> info, setFn, ...} =
	    CV.newProp (fn x => raise Fail ((CV.toString x) ^ " is not a candidate"))
    in
    val clearInfo = clrFn
    val getInfo = getFn
    fun setInfo (f, vmap, pmap, params, flat) =
	  setFn (f, {vmap=vmap, pmap=pmap, sign=computeMaxSig pmap, params=params, flat=flat});
    end

(* +DEBUG *)
    fun printCandidate f = let
	  val {vmap, pmap, sign, params, flat} = getInfo f
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

  (* the first part of the analysis is to gather all of the candidate functions and
   * their call sites.  We also compute the initial vmaps and pmaps based on the selects
   * in the candidate-function bodies.
   *)
    fun gather (module as C.MODULE{body, ...}) = let
	(* list of candidate functions *)
	  val candidates = ref []
	(* analyse a bound function or continuation *)
	  fun analyseLambdas fbs = let
	      fun analyseFB (fb as C.FB{f, body, ...}) = if isCandidate f
		    then analyseCandidate fb
		    else walkExp body
	      in
		List.app analyseFB fbs
	      end
	(* analyse the body of a candidate *)
	  and analyseCandidate (C.FB{f, params, body, ...}) = let
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
	      (* analyse the body of the candidate function *)
		fun doExp (vmap, pmap, C.Exp(ppt, t)) = (case t
		       of (C.Let([x], C.Select(i, y), e)) => (
			    case ParamMap.find(vmap, VAR y)
			     of NONE => doExp(vmap, pmap, e)
			      | SOME p => let
				  val q = SEL(i, p)
				  val vmap = ParamMap.insert(vmap, VAR x, q)
				(* decrement p's count *)
				  val cnt = lookupPath(pmap, p)
				  val _ = addToRef (cnt, ~1)
				(* either add q to the path map or update its count *)
				  val pmap = (case PMap.find(pmap, q)
					 of NONE => PMap.insert(pmap, q, ref(CV.useCount x))
					  | SOME cnt => (addToRef(cnt, CV.useCount x); pmap)
					(* end case *))
				  in
				    doExp (vmap, pmap, e)
				  end)
			| (C.Let(_, _, e)) => doExp (vmap, pmap, e)
			| (C.Fun(fbs, e)) => (
			    analyseLambdas fbs;
			    doExp (vmap, pmap, e))
			| (C.Cont(fb, e)) => (
			    analyseLambdas [fb];
			    doExp (vmap, pmap, e))
                        (* Conditional code: can't add guarded variable accesses
                         * i.e. can end up adding a param for y that was within
                         * an 'if not(null x) then let y = #1(x) else 2' *)
			| (C.If(x, e1, e2)) => let
			    val _ = walkExp e1
			    val _ = walkExp e2
			    in
                              (vmap, pmap)
			    end
(*			    val (vmap, pmap) = doExp(vmap, pmap, e1)
			    in
			      doExp(vmap, pmap, e2)
			    end *)
			| (C.Switch(x, cases, dflt)) => (
			    List.app (fn (_, e) => walkExp e) cases;
			    Option.app walkExp dflt;
			    (vmap, pmap))
			| (C.Apply(g, args, _)) => (
			    addCallSite (SOME f, ppt, g, args);
			    (vmap, pmap))
			| (C.Throw(k, args)) => (
			    addCallSite (SOME f, ppt, k, args);
			    (vmap, pmap))
		      (* end case *))
		val (vmap, pmap) = doExp(vmap, pmap, body)
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
		  setInfo (f, vmap, pmap, params, NONE);
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

  (* analyse a call site inside a candidate function. *)
    fun analyseCallSite (SITE{enclFn=NONE, ...}) = false
      | analyseCallSite (site as SITE{enclFn=SOME g, ppt, callees, f, args}) = let
	  val changed = ref false
	  fun doParam ((i, p), (vmap, pmap)) = let
		fun doArg ([], _, _, vmap, pmap) = (vmap, pmap)
		  | doArg (i::r, x, path, vmap, pmap) = (case CV.kindOf x
		       of C.VK_Let(C.Alloc(CTy.T_Tuple(false, _), xs)) =>
			    doArg (r, List.nth(xs, i), SEL(i, path), vmap, pmap)
			| _ => (case ParamMap.find(vmap, VAR x)
			     of SOME q => let (* this argument is derivable from a parameter *)
				(* have we already done the bookkeeping for x? *)
				  val vmap = (case ParamMap.find(vmap, ARG(site, path))
					 of NONE => (changed := true;
					      addToRef(lookupPath(pmap, q), ~1);
					      ParamMap.insert(vmap, ARG(site, path), q))
					  | SOME _ => vmap
					(* end case *))
				  in
				    followPath (r, SEL(i, q), SEL(i, path), vmap, pmap)
				  end
			      | NONE => (vmap, pmap)
			    (* end case *))
		      (* end case *))
		and followPath ([], _, _, vmap, pmap) = (vmap, pmap)
		  | followPath (i::r, srcPath, dstPath, vmap, pmap) = (
		      case ParamMap.find(vmap, ARG(site, dstPath))
		       of NONE => let
			    val vmap = ParamMap.insert(vmap, ARG(site, dstPath), srcPath)
			    in
			      followPath (r, SEL(i, srcPath), SEL(i, dstPath), vmap, pmap)
			    end
			| SOME q => followPath (r, q, SEL(i, dstPath), vmap, pmap)
		      (* end case *))
		in
		  doArg (p, List.nth(args, i), PARAM i, vmap, pmap)
		end
        (*
          (*DEBUG*)val gSig = sigOfFuns [g]
          val () = print(concat["* analyseCallSite (", siteToString site, ")\n"])
         *)
	  val {vmap, pmap, params, ...} = getInfo g
	  val (vmap, pmap) = List.foldl doParam (vmap, pmap) (sigOfFuns callees)
	  in
        (*            if !changed then print(concat["  ", sigToString gSig, "  -->  ", sigToString(sigOfFuns [g]), "\n"]) else (); *)
	    if !changed
	    then (setInfo(g, vmap, pmap, params, NONE); true)
	    else false
	  end

  (* for each candidate function, analyse the arguments of its call sites *)
    fun analyse m = let
	  val candidates = gather m
	  val sites = List.foldr (fn (f, s) => getSites f @ s) [] candidates
	  fun analLp ([], false) = ()
	    | analLp ([], true) = analLp (sites, false)
	    | analLp (site::r, flg) = analLp (r, analyseCallSite site orelse flg)
	  in
	    analLp (sites, false) ;
            candidates
	  end

  (***** Flattening *****)

    (* walk down the tree and:
     * - turn candidate functions into a specialized version and the
     * original, which calls the specialized with the extra arguments
     * - in the body of the specialized version, remove any variables
     *)
    fun flatten (C.MODULE{name,externs,body=(C.FB{f,params,rets,body})}) = let
	  fun flattenApplyThrow (ppt, g, args, rets) = if isCandidate g
		then let 
		  val {sign, flat=SOME(f), ...} = getInfo g
		  fun genCall (sign, newArgs, progress) =
                        if null sign
			  then (case rets
			     of SOME(rets) => C.Exp(ppt, C.Apply(f, rev newArgs, rets))
			      | NONE => C.Exp(ppt, C.Throw(f, rev newArgs))
			    (* end case *))
			  else let
			    fun genResult (varBase, path) = if length path = 0
				  then genCall (tl sign, varBase :: newArgs, NONE)
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
				    val _ = CV.setKind(newVar, C.VK_Let rhs)
				    in
				      if length path = 1
					then C.mkLet ([newVar], rhs,
					    genCall (tl sign, newVar :: newArgs, NONE))
					else C.mkLet ([newVar], rhs,
					    genCall (tl sign, newArgs, SOME (newVar, tl path)))
                                        end
			    in
			      case progress
			       of SOME(base,l) => genResult (base, l)
				| NONE => let
				    val (whichBase, path)::_ = sign
				    val varBase = List.nth (args, whichBase)
				    in
				      genResult (varBase, path)
				    end
			      (* end case *)
                            end
		  in
                    genCall (sign, [], NONE)
		  end
		else (case rets
		   of SOME(rets) => C.Exp(ppt, C.Apply(g, args, rets))
		    | NONE => C.Exp(ppt, C.Throw(g, args))
		  (* end case *))
	  and walkExp(newParams, C.Exp(ppt,e)) = (case e
		 of (C.Let([v], rhs, e)) => ( (* If v has been promoted to a param, omit the let *)
		      if List.exists (fn x => CV.same(x, v)) newParams
			then walkExp (newParams, e)
			else C.Exp(ppt, C.Let([v], rhs, walkExp (newParams, e))))
		  | (C.Let(vars, rhs, e)) =>
		      if List.exists (fn v => List.exists (fn x => CV.same(x, v)) newParams) vars
			then raise Fail ("Can't lift variable from multi-bind on LHS of let")
			else C.Exp(ppt, C.Let(vars, rhs, walkExp (newParams, e)))
		  | (C.Fun(fbs, e)) => let
		      val newfuns = List.foldr (fn (f,rr) => handleLambda (f,false) @ rr) [] fbs
		      in
			C.Exp(ppt, C.Fun(newfuns, walkExp (newParams, e)))
		      end
		  | (C.Cont(fb, e)) => (case handleLambda (fb, true)
		       of [fl, stb] => C.Exp(ppt, C.Cont(fl, C.mkCont (stb, walkExp (newParams, e))))
			| [single] => C.Exp(ppt, C.Cont(single, walkExp (newParams, e)))
			| _ => raise Fail "Invalid return from handleLambda"
		      (* end case *))
		  | (C.If(x, e1, e2)) =>
		      C.Exp(ppt, C.If(x, walkExp (newParams, e1), walkExp (newParams, e2)))
		  | (C.Switch(x, cases, dflt)) =>
		      C.Exp(ppt, C.Switch(x,
			List.map (fn (tag,exp) => (tag,walkExp (newParams, exp))) cases,
			Option.map (fn (f) => walkExp (newParams, f)) dflt))
		  | (C.Apply(g, args, rets)) => 
		      flattenApplyThrow (ppt, g, args, SOME(rets))
		  | (C.Throw(k, args)) =>
		      flattenApplyThrow (ppt, k, args, NONE)
		(* end case *))

        (* Returns flattened version of candidate functions *)
        and handleLambda(func as C.FB{f, params, rets, body}, isCont) =
	      if not (isCandidate f)
                then [C.FB{f=f, params=params, rets=rets, body=walkExp (params, body)}]
		else let
		  val {vmap, pmap, params, sign, flat} = getInfo f
		  val newParams = computeParamList (params, vmap, sign)
		  val _ = List.app (fn x => CV.setKind (x, C.VK_Param func)) newParams
		  val newType = CTy.T_Fun (List.map CV.typeOf newParams, List.map CV.typeOf rets)
		  val flat = CV.new ("flatFun", newType)
		  val _ = setInfo (f, vmap, pmap, params, SOME(flat))
		  val body = walkExp (newParams, body)
		(* Create a stub with the old name that just SEL's and jumps to the flat version. *)
		  val paramCopy = List.map CV.copy params
		  val retsCopy = List.map CV.copy rets
		  val stub = flattenApplyThrow (ProgPt.new(), f, paramCopy,
						if not(isCont) then SOME retsCopy else NONE)
				 
		  val stubLambda = C.FB{f=f, params=paramCopy, rets=retsCopy, body=stub}
		  val lambda = C.FB{f=flat, params=newParams, rets=rets, body=body}
		  val _  = CV.setKind (flat, if not(isCont) then C.VK_Fun lambda else C.VK_Cont lambda)                                        
		  in
                    [lambda, stubLambda]
		  end
	in
	  C.MODULE{
	      name=name,externs=externs,
	      body = C.mkLambda(C.FB{f=f, params=params, rets=rets, body=walkExp (params, body)})
	    }
	end

  (***** Transformation *****)

    fun transform m = if !enableArityRaising
	  then let
	    val candidates = analyse m
	    val m' = flatten m
	    in
	      if !flatteningDebug
		then List.app printCandidate candidates
		else ();
	      m'
	    end
	  else m

  end
