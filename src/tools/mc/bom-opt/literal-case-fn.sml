(* literal-case-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

signature CASE_DOMAIN =
  sig

    type label	(* the type of values labeling the cases *)

  (* comparisons for labels *)
    val equal : (label * label) -> bool
    val greater : (label * label) -> bool

  (* successor/predecessor of a label; returns NONE when there is no immediate
   * successor/predecessor
   *)
    val succ : label -> label option
    val pred : label -> label option

  (* generate a three-way comparison against a label *)
    val genCmpTest : {
	    arg : BOM.var,
	    key : label,
	    ltAct : BOM.exp,
	    eqAct : BOM.exp,
	    gtAct : BOM.exp
	  } -> BOM.exp

  (* generate an equality test against a label *)
    val genEqTest : {
	    arg : BOM.var,
	    key : label,
	    eqAct : BOM.exp,
	    neqAct : BOM.exp
	  } -> BOM.exp

  end

functor LiteralCaseFn (C : CASE_DOMAIN) :> sig

    type label = C.label

    val convert : {
	    arg : BOM.var,
	    arcs : (label * BOM.exp) list,
	    default : BOM.exp
	  } -> BOM.exp

  end = struct

    structure B = BOM
    structure BV = B.Var
    structure BTy = BOMTy

    type label = C.label

  (* NOTE: these options should be defined elsewhere and should also be settable
   * from the command-line.
   *)
    val minBinSearchNum = 3
    val useBinarySearch = true

    val sort = ListMergeSort.sort (fn ((l1, e1 : B.exp), (l2, e2)) => C.greater(l1, l2))

  (* generate BOM code for a linear search *)
    fun genLinearSearch (x, arcs, default) = let
	  fun gen [] = default
	    | gen [(key, act)] = C.genEqTest{arg=x, key=key, eqAct=act, neqAct=default}
	    | gen ((key, act)::rest) =
		C.genEqTest{arg=x, key=key, eqAct=act, neqAct=gen rest}
	  in
	    gen arcs
	  end

    type range = {min : label option, max : label option}

    fun splitRange ({min, max}, x) = ({min=min, max=C.pred x}, {min=C.succ x, max=max})

  (* does an ordered list of values cover a range? *)
    fun covered (vs, {min=SOME min, max = SOME max}) = let
	  fun cmp ([], min) = C.greater(min, max)
	    | cmp ((v, _)::r, min) = C.equal(v, min) andalso cmp(r, valOf(C.succ min))
	  in
	    cmp (vs, min)
	  end
      | covered _ = false

  (* generate BOM code for a match tree from a sorted list of arcs *)
    fun genBinarySearch (x, nCases, arcs, default) = let
	  fun gen (_, _, []) = default
	    | gen (_, rng, arcs as [(key, act)]) =
		if covered(arcs, rng)
		  then act
		  else C.genEqTest{arg=x, key=key, eqAct=act, neqAct=default}
	    | gen (_, rng, arcs as [(key1, act1), (_, act2)]) = if covered(arcs, rng)
		then C.genEqTest {arg = x, key = key1, eqAct = act1, neqAct = act2}
		else genSplit (2, rng, arcs)
	    | gen (nCases, rng, arcs) = genSplit (nCases, rng, arcs)
	  and genSplit (nCases, rng, arcs) = let
		val mid = nCases div 2
		fun split (0, arc::rhs, lhs) = (List.rev lhs, arc, rhs)
		  | split (i, arc::rhs, lhs) = split (i-1, rhs, arc::lhs)
		val (lhs, (key, act), rhs) = split(mid, arcs, [])
		val (leftRng, rightRng) = splitRange(rng, key)
		in
		  C.genCmpTest {
		      arg = x, key = key,
		      ltAct = gen (mid, leftRng, lhs),
		      eqAct = act,
		      gtAct = gen (nCases-(mid+1), rightRng, rhs)
		    }
		end
	  in
	    gen (nCases, {min=NONE, max=NONE}, arcs)
	  end

    fun convert {arg, arcs, default} = let
	  val arcs = sort arcs
	  val nCases = List.length arcs
	  in
	    if ((nCases >= minBinSearchNum) andalso useBinarySearch) 
(* NOTE: eventually, we need to check for using a jump table *)
	      then let
                    val dfltCont = BV.new ("kDflt", BTy.T_Cont([]))
                    val fb = B.mkLambda {f=dfltCont, params=[], exh=[], body=default}
                    val default' = B.mkThrow (dfltCont, [])
                in
                    B.mkCont(fb, genBinarySearch (arg, nCases, arcs, default'))
                end
	      else genLinearSearch (arg, arcs, default)
	  end

  end
