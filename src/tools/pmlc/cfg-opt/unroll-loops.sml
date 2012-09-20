(* unroll-loops.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure CFA = CFACFG

structure Sexp = struct
datatype t = Atom of string
       | List of t list

fun pair str sexps = List [ Atom str, List sexps ]
fun pair' str sexp = List [ Atom str, sexp ]

fun ofList ofItem = List o List.map ofItem

fun ofOption _ NONE = Atom "NONE"
  | ofOption ofAlpha (SOME alpha) = pair' "SOME" (ofAlpha alpha)

val ofLabel = Atom o CFG.Label.toString
val ofLabelSet = ofList ofLabel o CFG.Label.Set.listItems
fun ofLabelMap ofItem = List o CFG.Label.Map.foldli (fn (k, v, acc) => List [ofLabel k, ofItem v] :: acc) []

val ofLiteral = Atom o Literal.toString

val ofVar = Atom o CFG.Var.toString

fun ofTail (x as (headLabel, loopSet, CFG.BLK {lab = tailLabel, ...})) =
    pair "tail" [pair' "head" (ofLabel headLabel),
		 pair' "body" (ofLabelSet loopSet),
		 pair' "tail" (ofLabel tailLabel)]



fun toString (Atom str) = str
  | toString (List sexps) = "(" ^ (String.concatWith " " (List.map toString sexps)) ^ ")"

end

structure UnrollLoops : sig

    val transform : CFG.module -> CFG.module

end = struct

infixr 6 >>=
infixr 6 >>|

fun t >>= f = Option.mapPartial f t
fun t >>| f = Option.map f t

structure LS = CFG.Label.Set
structure LM = CFG.Label.Map

fun debug sexp : unit =
    if false then () else TextIO.print (Sexp.toString sexp ^ "\n")

val cfgReallyUnrollLoops : bool Controls.control = Controls.genControl {
						   name = "really-unroll-loops",
						   pri = [0, 0],
						   obscurity = 0,
						   help = "run the loop-unrolling pass (which doesn't actually work yet)",
						   default = false
						   }

val _ = ControlRegistry.register CFGOptControls.registry {
	ctl = Controls.stringControl ControlUtil.Cvt.bool cfgReallyUnrollLoops,
	envName = NONE
	}


(* For the first try, we will unroll loops of the form
     fun f n = if n = 0 then ... else f (n - 1)
   The tail must be unique, the conditional must be CFG.If
*)

fun analyze (m as CFG.MODULE {code, ...}) = let

    (* the root node of the whole program, the 'main' function *)
    val n0 : CFG.label = case code
	      of [] => raise Fail "unroll-loops: empty program"
	       | CFG.FUNC {start = CFG.BLK {lab, ...}, ...} :: _ => lab

    val callersOf : LS.set LM.map = let
	fun addCalleeOf caller (callee, acc) =
	    case LM.find (acc, callee)
	     of NONE => LM.insert (acc, callee, LS.singleton caller)
	      | SOME callers => LM.insert (acc, callee, LS.add (callers, caller))

	fun doBlk (CFG.BLK {lab = caller, exit, ...}, acc : LS.set LM.map) =
	    case CFA.labelsOf exit
	     of NONE => acc
	      | SOME calleesSet => LS.foldr (addCalleeOf caller) acc calleesSet

	fun doFunc (CFG.FUNC {start, body, ...}, acc) =
	    List.foldr doBlk acc (start::body)
    in
	List.foldr doFunc LM.empty code
    end

    val dominatorsOf : LS.set LM.map = let
	val nodes = LM.foldli (fn (key, _, acc) => key::acc) [] callersOf
	val all_nodes = List.foldl LS.add' (LS.singleton n0) nodes

	(* initialize dom to Top : all possible dominators (no false negatives, many false positives) *)
	val dom = List.foldl (fn (n, acc) => LM.insert (acc, n, all_nodes))
			     (LM.insert (LM.empty, n0, LS.singleton n0))
			     nodes

	fun findOrEmpty (map, n) = Option.getOpt (LM.find (map, n), LS.empty)

	fun iter dom = let
	    (* val _ = debugSetMap dom *)

	    val (continue, dom') = let
		fun f (n, n_doms, (continue, acc)) =
		    if CFG.Label.compare (n, n0) = EQUAL
		    then (continue, LM.insert (acc, n, n_doms))
		    else

			let
			    val n_doms' =
				case LS.listItems (findOrEmpty (callersOf, n))
				 of [] => LS.empty
				  | car::cdr =>
				    List.foldl (fn (pred, acc) =>
						   LS.intersection (findOrEmpty (dom, pred), acc))
					       (findOrEmpty (dom, car))
					       cdr

			    val n_doms'' = LS.add (n_doms', n)
			in
			    (continue orelse not (LS.equal (n_doms, n_doms'')),
			     LM.insert (acc, n, n_doms''))
			end
	    in
		LM.foldli f (false, LM.empty) dom
	    end

	in
	    if continue then iter dom' else dom'
	end
    in
	iter dom
    end

    val () = debug (Sexp.ofLabelMap Sexp.ofLabelSet dominatorsOf)

    (* A back-edge is a jump from dominated to dominator *)
    val backedges : (CFG.label * CFG.label) list = let
	fun backedgesFrom this (callee, (seen, acc)) = let
	    val acc =
		case LM.find (dominatorsOf, this)
		 of NONE => acc
		  | SOME dominatorsSet =>  if LS.member (dominatorsSet, callee) then (this, callee)::acc else acc
	in
	    iter (callee, seen, acc)
	end
	and iter (this, seen, acc) =
	    if LS.member (seen, this) then (seen, acc) else
	    case CFGUtil.blockOfLabel this >>= (fn (CFG.BLK {exit, ...}) => CFA.labelsOf exit)
		 of NONE => (seen, acc)
		  | SOME calleesSet =>
		    LS.foldl (backedgesFrom this) (LS.add (seen, this), acc) calleesSet

	val (_, retval) = iter (n0, LS.empty, [])
    in
	retval
    end

    val () = debug (Sexp.ofList (fn (a, b) => Sexp.List [ Sexp.ofLabel a, Sexp.ofLabel b]) backedges)

    val naturalLoops : LS.set list LM.map = let
	fun closure (x, acc) =
	    if LS.member (acc, x) then acc else
	    LS.foldl closure (LS.add (acc, x)) (Option.getOpt (LM.find (callersOf, x), LS.empty))

	fun f ((n, h), acc) = let
	    val existingLoops = Option.getOpt (LM.find (acc, h), [])
	    val newLoop = closure (n, LS.singleton h)
	in
	    LM.insert (acc, h, newLoop :: existingLoops)
	end
    in
	List.foldl f LM.empty backedges
    end

    val () = debug (Sexp.ofLabelMap (Sexp.ofList Sexp.ofLabelSet) naturalLoops)

    (* We call the tail of a loop the block where we decide whether to
       jump back to the head.  Note that the tail need not be unique. *)
    (* returns (headLabel * loopSet * tailBlock) option *)
    fun tailOfLoop headLabel loopSet : (CFG.label * LS.set * CFG.block) option = let

	(* follow the caller-of relation until we find a block that can decide to jump out of the loop. *)
	fun findConditionalJump (_, (NONE, seen)) = (NONE, seen)
	  | findConditionalJump (lab, (SOME acc, seen)) =
	    if not (LS.member (loopSet, lab)) orelse LS.member (seen, lab) then (SOME acc, seen) else let
		val seen' = LS.add (seen, lab)
	    in
		case CFGUtil.blockOfLabel lab
		 of NONE => (SOME acc, seen')
		  | SOME (blk as CFG.BLK {exit, ...}) =>
		    case CFA.labelsOf exit
		     of NONE => (SOME acc, seen')
		      | SOME jumps =>
			(* if numItems is not None, then numItems >= 1 because we got to this node by following the caller-of relation. *)
			if LS.numItems jumps = 1 then
			    iter (lab, seen')
			else (SOME ((headLabel, loopSet, blk) :: acc), seen')
	    end
	and iter (label, seen) =
	    case LM.find (callersOf, label)
	     of NONE => (NONE, seen)
	      | SOME callersSet => LS.foldl findConditionalJump (SOME [], seen) callersSet
    in
	case iter (headLabel, LS.empty)
	 of (SOME [block], _) => SOME block
	  | _ => NONE (* TODO handle non-unique tails *)
    end

    fun findI32EqArgs (CFG.BLK {exit = CFG.If (Prim.I32Eq (a, b), _, _), ...}) = SOME (a, b)
      | findI32EqArgs _ = NONE

    datatype variable_origin = LitVal of Literal.literal
			     | Unchanged of CFG.var

    fun variable_origin_toSexp (LitVal lit) = Sexp.pair' "LitVal" (Sexp.Atom (Literal.toString lit))
      | variable_origin_toSexp (Unchanged var) = Sexp.pair' "Unchanged" (Sexp.Atom (VarRep.toString var))

    datatype 'a lattice = Just of 'a
			| Top
			| Bottom

    fun labelOfBlock (CFG.BLK {lab, ...}) = lab

    fun getVarOrigin (headLabel, loopSet, tailBlock) var = let

	fun iter (calleeLabel, CFG.BLK {body, args, lab, ...}, var) : variable_origin lattice = let

	    fun isVar var' = EQUAL = VarRep.compare (var, var')
	    fun isCalleeLabel label = EQUAL = CFG.Label.compare (calleeLabel, label)

	    fun safeNth i xs = SOME (List.nth (xs, i)) handle Subscript => NONE

	    fun indexOf pred xs : int option = let
		fun iter (i, []) = NONE
		  | iter (i, car::cdr) = if pred car then SOME i else iter (i + 1, cdr)
	    in
		iter (0, xs)
	    end

	    fun varNameInCaller (callerLabel, argIndex) : variable_origin lattice =
		case CFGUtil.blockOfLabel callerLabel
		 of NONE => Top
		  | SOME (callerBlock as CFG.BLK {exit, ...}) => let
			val args =
			    case exit
			     of (CFG.StdApply {args, ...}
			      | CFG.StdThrow {args, ...}
			      | CFG.Apply {args, ...}
			      | CFG.Goto (_, args)
			      | CFG.HeapCheck {nogc = (_, args), ...}
			      | CFG.HeapCheckN {nogc = (_, args), ...}
			      | CFG.AllocCCall {ret = (_, args), ...}) => SOME args
			      | CFG.If (_, (label, args), (label', args')) =>
				SOME (if isCalleeLabel label then args else args')
			      | CFG.Switch (_, jumps, default) =>
				case (List.find (isCalleeLabel o #1 o #2) jumps) (* >>= (safeNth argIndex) *)
				 of SOME (_, (_, args)) => SOME args
				  | NONE => (default >>| #2)
		    in
			case args >>= (safeNth argIndex)
			 of NONE => (TextIO.print "varNameInCaller returns Top\n"; Top)
			  | SOME var =>
			    if EQUAL = CFG.Label.compare (headLabel, callerLabel) then
				Just (Unchanged var)
			    else iter (calleeLabel, callerBlock, var)
		    end

	    fun meet  _ (_, Bottom) : variable_origin lattice = Bottom
	      | meet argIndex (callerLabel, acc) =
		case (varNameInCaller (callerLabel, argIndex), acc)
		 of (origin, Top) => origin
		  | (Top, acc) => acc
		  | (Bottom, _) => Bottom
		  | (_, Bottom) => Bottom
		  | (Just a, Just b) =>
		    case (a, b)
		     of (LitVal _, Unchanged _) => Bottom
		      | (Unchanged _, LitVal _) => Bottom
		      | (LitVal literal, LitVal literal') =>
			if Literal.same (literal, literal') then
			    Just (LitVal literal)
			else Bottom
		      | (Unchanged var, Unchanged var') =>
			if EQUAL = VarRep.compare (var, var') then
			    Just (Unchanged var)
			else Bottom

	    fun findConst (CFG.E_Const (var, literal, _)) = isVar var
	      | findConst _ = false

	in
	    if EQUAL = VarRep.compare (headLabel, lab) andalso Option.isSome (List.find isVar args) then
		Just (Unchanged var)
	    else
		case List.find findConst body
		 of SOME (CFG.E_Const (var, literal, _)) => Just (LitVal literal)
		  | SOME _ => Bottom
		  | NONE =>
		    case indexOf isVar args
		     of NONE => Top
		      | SOME i =>
			case LM.find (callersOf, lab)
			 of NONE => Top
			  | SOME callerLabelSet => LS.foldl (meet i) Top callerLabelSet
	end
    in
	case iter (labelOfBlock tailBlock, tailBlock, var)
	 of Just x => SOME x
	  | (Top | Bottom) => NONE
    end

    val tails : (CFG.label * CFG.Label.Set.set * CFG.block) list = let
	fun debugTails (x as (headLabel, loopSet, CFG.BLK {lab = tailLabel, ...})) = let
	    val sexp = [Sexp.pair' "head" (Sexp.ofLabel headLabel),
			Sexp.pair' "body" (Sexp.ofLabelSet loopSet),
			Sexp.pair' "tail" (Sexp.ofLabel tailLabel)]
	    val () = TextIO.print (Sexp.toString (Sexp.pair "tail" sexp) ^ "\n")
	in
	    x
	end

	fun getTails (headNode, loopSetList, acc) =
	    List.mapPartial (Option.map debugTails o tailOfLoop headNode) loopSetList :: acc
    in
	List.concat (LM.foldli getTails [] naturalLoops)
    end

    datatype loop_condition = I32Eq of {var : CFG.var,
				    comparisonLiteral : Literal.literal}

    fun Sexp_ofLoopCondition (I32Eq {var, comparisonLiteral}) =
	Sexp.pair "I32Eq" [Sexp.ofVar var, Sexp.ofLiteral comparisonLiteral]

    val tailsWithI32Eq : {headLabel : CFG.label, loopSet : CFG.Label.Set.set, tailBlock : CFG.block, condition : loop_condition} list = let
	fun f (args as (headLabel, loopSet, tailBlock)) =
	    case findI32EqArgs tailBlock
	      of NONE => NONE
	       | SOME (a, b) =>
		 case (getVarOrigin args a, getVarOrigin args b)
		  of (SOME a, SOME b) =>
		     (case (a, b)
		       of ((Unchanged var, LitVal lit)
			| (LitVal lit, Unchanged var)) =>
 			  (SOME {headLabel = headLabel,
				 loopSet = loopSet,
				 tailBlock = tailBlock,
				 condition = I32Eq {var = var, comparisonLiteral = lit}
			  })
			| _ => NONE)
		   | _ => NONE

	fun debug x = let
	    fun toSexp {headLabel, loopSet, tailBlock = CFG.BLK {lab = tailLabel, ...}, condition} =
		Sexp.List [Sexp.pair' "headLabel" (Sexp.ofLabel headLabel),
			   Sexp.pair' "loopSet" (Sexp.ofLabelSet loopSet),
			   Sexp.pair' "tailBlock(label)" (Sexp.ofLabel tailLabel),
			   Sexp.pair' "condition" (Sexp_ofLoopCondition condition)]
	    val _ = TextIO.print (((Sexp.toString o Sexp.pair' "tailswithI32Eq" o toSexp) x) ^ "\n")
	in
	    x
	end
    in
	List.map debug (List.mapPartial f tails)
    end

in
    ()
end

fun transform m = let
    val () = if Controls.get cfgReallyUnrollLoops then analyze m else ()
in
    m
end
end


