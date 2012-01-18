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

fun toString (Atom str) = str
  | toString (List sexps) = "(" ^ (String.concatWith " " (List.map toString sexps)) ^ ")"

fun pair str sexps = List [ Atom str, List sexps ]
fun pair' str sexp = List [ Atom str, sexp ]
end

signature SOLVER_PARAM = sig
    type ord_key
    type graph
    val compare : ord_key * ord_key -> order
    val toSexp : ord_key -> Sexp.t
end

functor Solver (Ord : SOLVER_PARAM) = struct

structure LabelSet = BinarySetFn(Ord)

(* figure out how to put this into the LabelSet structure *)
fun LabelSet_toSexp (set : LabelSet.set) : Sexp.t = let
    val items = LabelSet.foldl (fn (item, acc) => Ord.toSexp item :: acc) [] set
in
    Sexp.List ((Sexp.Atom "LabelSet.set")::items)
end

fun LabelSet_ofList (list : LabelSet.item list) : LabelSet.set =
    List.foldl LabelSet.add' LabelSet.empty list

structure LabelMap = BinaryMapFn(Ord)

fun LabelMap_toSexp (itemToSexp : 'a -> Sexp.t) (labelmap : 'a LabelMap.map) : Sexp.t = let
    val items = LabelMap.foldli (fn (key, value, acc) =>
                    (Sexp.List [Ord.toSexp key, itemToSexp value])::acc)
                [] labelmap
in
    Sexp.List ((Sexp.Atom "LabelMap.map")::items)
end

fun LabelMap_ofAList (alist : (Ord.ord_key * 'a) list) : 'a LabelMap.map =
    List.foldl LabelMap.insert' LabelMap.empty alist

fun debug (toSexp : 'a -> Sexp.t) dom : 'a =
    let
        fun addNewline s = s ^ "\n"
        val () = (TextIO.print o addNewline o Sexp.toString o toSexp) dom
    in
        dom
    end


fun debugSetMap dom = debug (LabelMap_toSexp LabelSet_toSexp) dom
fun debugPairList list =
    debug (Sexp.List o List.map (fn (i, i') => Sexp.List [ Ord.toSexp i, Ord.toSexp i' ])) list

(* (k, (v1, v2, ...)) in callers means blocks v1, v2, ... can jump to block k *)
fun callers callees_alist : LabelSet.set LabelMap.map = let
    fun f (key, value, acc) = let
	fun f' (v, acc) =
	    case LabelMap.find (acc, v)
	     of NONE => LabelMap.insert (acc, v, LabelSet.singleton key)
	      | SOME set => LabelMap.insert (acc, v, LabelSet.add (set, key))
    in
	LabelSet.foldl f' acc value
    end

    (* (k, (v1, v2, ...)) in callees means block k can jump to any of v1, v2, ... *)
    val callees : LabelSet.set LabelMap.map =
        (LabelMap_ofAList o List.map (fn (key, values) => (key, LabelSet_ofList values))) callees_alist
in
    LabelMap.foldli f LabelMap.empty callees
end


(* (k, (v1, v2, ...)) in dominators (n0, nodes, callers) means block k is
                    dominated by v1, v2, ... in the call graph rooted at n0. *)
fun dominators (n0, callers) : LabelSet.set LabelMap.map = let

    val nodes = LabelMap.foldli (fn (key, _, acc) => key::acc) [] callers
    val all_nodes = List.foldl LabelSet.add' (LabelSet.singleton n0) nodes

    (* initialize dom = {(n0, {n0})} union { (n, all_nodes) : n in all_nodes } *)
    val dom = List.foldl (fn (n, acc) => LabelMap.insert (acc, n, all_nodes))
             (LabelMap.insert (LabelMap.empty, n0, LabelSet.singleton n0)) nodes

    fun findOrEmpty (map, n) =
    case LabelMap.find (map, n)
     of NONE => LabelSet.empty
      | SOME set => set

    fun iter dom = let
	(* val _ = debugSetMap dom *)

	val (continue, dom') = let
            fun f (n, n_doms, (continue, acc)) =
		if Ord.compare (n, n0) = EQUAL
		then (continue, LabelMap.insert (acc, n, n_doms))
		else

		    let
			val n_doms' =
			    case LabelSet.listItems (findOrEmpty (callers, n))
			     of [] => LabelSet.empty
			      | car::cdr =>
				List.foldl (fn (pred, acc) =>
					       LabelSet.intersection (findOrEmpty (dom, pred), acc))
					   (findOrEmpty (dom, car))
					   cdr

			val n_doms'' = LabelSet.add (n_doms', n)
		    in
			(continue orelse not (LabelSet.equal (n_doms, n_doms'')),
			 LabelMap.insert (acc, n, n_doms''))
		    end
	in
            LabelMap.foldli f (false, LabelMap.empty) dom
	end

    in
	if continue then iter dom' else debugSetMap dom'
    end
in
    iter dom
end

fun immediateDominators dominators = let
    val dominators = let
	fun safeDelete (key, set) = LabelSet.delete (set, key) handle NotFound => set
    in
	LabelMap.mapi safeDelete dominators
    end

    fun safeHd l = SOME (List.hd l) handle Empty => NONE

    fun f ([], dom) = safeHd (LabelSet.listItems dom)
      | f ((car :: cdr), dom) =
	case LabelMap.find (dominators, car)
	 of NONE => f (cdr, dom)
	  | SOME doms => f (cdr, LabelSet.difference (dom, doms))
in
    LabelMap.map (fn dom => f (LabelSet.listItems dom, dom)) dominators
end

(* (caller, callee) list *)
fun backedges (n0, callees_of : LabelSet.set LabelMap.map, dominators_of : LabelSet.set LabelMap.map) : (Ord.ord_key * Ord.ord_key) list = let
    fun backedgesFrom this (callee, (seen, acc)) = let
	fun pair a b = Sexp.List [ Sexp.Atom a, b ]
	fun toSexp (this, seen, callee, acc) =
	    Sexp.List [ Sexp.Atom "backedgesFrom",
			pair "this" (Ord.toSexp this),
			pair "seen" (LabelSet_toSexp seen),
			pair "callee" (Ord.toSexp callee),
			pair "acc" (Sexp.List (List.map (fn (a, b) => Sexp.List [Ord.toSexp a, Ord.toSexp b]) acc)) ]
	(* val _ = debug toSexp (this, seen, callee, acc) *)

	val acc =
	    case LabelMap.find (dominators_of, this)
	     of NONE => acc
	      | SOME dominators => if LabelSet.member (dominators, callee) then (this, callee)::acc else acc
    in
	iter (callee, seen, acc)
    end
    and iter (this, seen, acc) = let
	fun pair a b = Sexp.List [ Sexp.Atom a, b ]
	fun toSexp (this, seen, acc) =
	    Sexp.List [ Sexp.Atom "iter",
			pair "this" (Ord.toSexp this),
			pair "seen" (LabelSet_toSexp seen),
			pair "acc" (Sexp.List (List.map (fn (a, b) => Sexp.List [Ord.toSexp a, Ord.toSexp b]) acc)) ]
	(* val _ = debug toSexp (this, seen, acc) *)
    in
	if LabelSet.member (seen, this) then (seen, acc) else let
	    (* val _ = TextIO.print (Sexp.toString (Ord.toSexp this) ^ " not in " ^ Sexp.toString (LabelSet_toSexp seen)) *)
	    val dominators = Option.getOpt (LabelMap.find (dominators_of, this), LabelSet.empty)
	in
	    case LabelMap.find (callees_of, this)
	     of NONE => (seen, acc)
	      | SOME callees => LabelSet.foldl (backedgesFrom this) (LabelSet.add (seen, this), acc) callees
	end
    end
in
    #2 (iter (n0, LabelSet.empty, []))
end

(* Given a back edge n -> h, where h dominates n, we say x is in the
   natural loop whose head is h iff there is a path P from x to n that
   does not include h.  (This implies that h dominate x also.)
 *)

fun naturalLoops callers_of backedges = let
    fun closure (x, acc) = let
	(* val _ = debug LabelSet_toSexp acc *)
    in
	if LabelSet.member (acc, x) then acc else
	  LabelSet.foldl closure (LabelSet.add (acc, x)) (Option.getOpt (LabelMap.find (callers_of, x), LabelSet.empty))
    end

    fun f ((n, h), acc) = let
	val cdr = Option.getOpt (LabelMap.find (acc, h), [])
	val car = closure (n, LabelSet.singleton h)
    in
	LabelMap.insert (acc, h, car :: cdr)
    end
in
    List.foldl f LabelMap.empty backedges
end

fun transform code = let

    fun all_nodes callers = LabelMap.foldli (fn (key, _, acc) => key::acc) [] callers
in
    code
end
end


structure UnrollLoops : sig

    val transform : CFG.module -> CFG.module

end = struct

infixr 6 >>=

fun t >>= f = Option.mapPartial f t


(* For the first try, we will unroll loops of the form
     fun f n = if n = 0 then ... else f (n - 1)
   The tail must be unique, the conditional must be CFG.If
*)

(* We call the tail of a loop the block where we decide whether to
jump back to the head.  Note that the tail need not be unique. *)

fun tailOfLoop callers_of headLabel (loop : CFG.Label.Set.set)
    : (CFG.label * CFG.Label.Set.set * CFG.block) option = let
    val () = TextIO.print ("tailOfLoop on " ^ VarRep.toString headLabel ^ "\n")

    fun find_if (_, (NONE, seen))  = (NONE, seen)
      | find_if (lab, (SOME acc, seen)) = (TextIO.print ("find_if on " ^ VarRep.toString lab ^ "\n");
	if not (CFG.Label.Set.member (loop, lab)) then
	    (TextIO.print (VarRep.toString lab ^ "not in loop\n");
	     (SOME acc, seen))
	else if CFG.Label.Set.member (seen, lab) then
	    (TextIO.print (VarRep.toString lab ^ "already seen\n");
	     (SOME acc, seen))
	else ( (* Sorry for the indentation; I don't know why Emacs insists on this *)
	    case (CFGUtil.blockOfLabel lab)
		     >>= (fn blk as CFG.BLK {exit, ...} =>
			     CFA.labelsOf exit
					  >>= (fn jumps =>
						  (* if numItems is not None, then numItems >= 1 because
												 we got to this node by following the caller-of relation. *)
						  if CFG.Label.Set.numItems jumps = 1 then
						      SOME (iter (lab, CFG.Label.Set.add (seen, lab)))
						  else (TextIO.print ("returning " ^ VarRep.toString lab ^ "\n");
					       SOME (SOME ((headLabel, loop, (blk : CFG.block)) :: acc), CFG.Label.Set.add (seen, lab)))))
	     of NONE => (TextIO.print ("NONE on find_if"); (SOME acc, CFG.Label.Set.add (seen, lab)))
	      | SOME x => x)
					  )

    and iter (label, seen)
	: ((CFG.label * CFG.Label.Set.set * CFG.block) list option * CFG.Label.Set.set) = let
	val () = TextIO.print ("iter on " ^ VarRep.toString label ^ "\n");
    in
	case CFG.Label.Map.find (callers_of, label)
	 of NONE => (NONE, seen)
	  | SOME callers => CFG.Label.Set.foldl find_if (SOME [], seen) callers
    end
in
    case #1 (iter (headLabel, CFG.Label.Set.empty))
     of SOME [block] => SOME block (* TODO handle non-unique tails *)
      | _ => NONE
end

fun getN0 code =
    case code
     of [] => raise Fail "unroll-loops: empty program"
      | CFG.FUNC {start = CFG.BLK{lab, ...} , ...} :: _ => lab
(* end case *)

(* construct the flow graph for a module, copy-pasted from AddAllocChecksFn *)
(* (root node * (node * callee-node list) list) *)
fun makeGraph code : (CFG.label * CFG.label list) list = let
    (* return the outgoing targets of a function *)
    fun nodeFromBlock (CFG.BLK{lab, exit, ...}) =
        (case CFA.labelsOf exit
          of NONE => (lab, [])
           | SOME ls => (lab, CFG.Label.Set.listItems ls)
        (* end case *))
    fun toNode (CFG.FUNC{start,body,...}) =
        List.foldr (fn (b,rr) => (nodeFromBlock b)::rr) [] (start::body)
in
    List.foldr (fn (node,rr) => (toNode node)@rr) [] code
end

structure CFGSolver = Solver(struct
                 type ord_key = CFG.label
                 type graph = CFG.func list
                 val compare = VarRep.compare
                 fun toSexp v = Sexp.Atom (VarRep.toString v)

                 end)

structure IntSolver = Solver(struct
                type ord_key = int
                type graph = (ord_key * (ord_key * ord_key list) list)
                val compare = Int.compare
                fun toSexp i = Sexp.Atom (Int.toString i)
                end)

(* Appel Tiger book: Fig. 18.1 *)
val test = (1,
        [(1, [2]),
         (2, [3, 4]),
         (3, [2]),
         (4, [2, 5, 6]),
         (5, [7, 8]),
         (6, [7]),
         (7, [11]),
         (8, [9]),
         (9, [8, 10]),
         (10, [5, 12]),
         (11, [12]),
         (12, [])])

fun List_toSexp toSexp list = Sexp.List (List.map toSexp list)

fun debugTails (x as (headLabel, loopSet, CFG.BLK {lab = tailLabel, ...})) = let
    val sexp = [Sexp.pair "head" [Sexp.Atom (VarRep.toString headLabel)],
		Sexp.pair' "body" (List_toSexp (Sexp.Atom o VarRep.toString) (CFG.Label.Set.listItems loopSet)),
		Sexp.pair "tail" [Sexp.Atom (VarRep.toString tailLabel)]]
    val () = TextIO.print (Sexp.toString (Sexp.pair "tail" sexp) ^ "\n")
in
    x
end


fun transform m = let
    fun intMapOfAList alist = IntSolver.LabelMap_ofAList (List.map (fn (k, v) => (k, IntSolver.LabelSet_ofList v)) alist)
    fun intOption_toSexp NONE = Sexp.Atom "NONE"
      | intOption_toSexp (SOME i) = Sexp.List  [ Sexp.Atom "SOME", Sexp.Atom (Int.toString i)]
    fun labelOption_toSexp NONE = Sexp.Atom "NONE"
      | labelOption_toSexp (SOME l) = Sexp.List  [ Sexp.Atom "SOME", Sexp.Atom (VarRep.toString l)]

    val _ =
	if false then
	    let
		val callers = IntSolver.callers (#2 test)
		val dominators = (IntSolver.debugSetMap o IntSolver.dominators) (#1 test, callers)
		val idoms = IntSolver.debug (IntSolver.LabelMap_toSexp intOption_toSexp) (IntSolver.immediateDominators dominators)
		val backedges = IntSolver.debugPairList (IntSolver.backedges (#1 test, intMapOfAList (#2 test), dominators))
		val naturalLoops = (IntSolver.debug (IntSolver.LabelMap_toSexp (List_toSexp IntSolver.LabelSet_toSexp)) o IntSolver.naturalLoops callers) backedges

	    in () end
	else
	    let
		fun getCode (CFG.MODULE {code, ...}) = code
		val code = getCode m
		val n0 = getN0 code
		val callees_alist = makeGraph code
		val callees = CFGSolver.LabelMap_ofAList (List.map (fn (k, v) => (k, CFGSolver.LabelSet_ofList v)) callees_alist)
		val callers = CFGSolver.callers callees_alist
		val dominators = ((* CFGSolver.debugSetMap o *) CFGSolver.dominators) (n0, callers)
		val idoms = (* CFGSolver.debug (CFGSolver.LabelMap_toSexp labelOption_toSexp) *) (CFGSolver.immediateDominators dominators)
		val backedges = (* CFGSolver.debugPairList *) (CFGSolver.backedges (n0, callees, dominators))

		fun alistOfSetMap (headNode, loopSetList, acc) =
		    CFG.Label.Map.insert (acc, headNode, List.map (CFGSolver.LabelSet.foldl CFG.Label.Set.add' CFG.Label.Set.empty) loopSetList)
		val naturalLoops : CFG.Label.Set.set list CFG.Label.Map.map = (CFGSolver.LabelMap.foldli alistOfSetMap CFG.Label.Map.empty
				    o CFGSolver.debug (CFGSolver.LabelMap_toSexp (List_toSexp CFGSolver.LabelSet_toSexp))
				    o CFGSolver.naturalLoops callers)
				       backedges
		fun fromCFGSolverSetMap setMap = let
		    fun f (key, valueSet, acc) =
			CFG.Label.Map.insert (acc, key, CFGSolver.LabelSet.foldl CFG.Label.Set.add' CFG.Label.Set.empty valueSet)
		in
		    CFGSolver.LabelMap.foldli f CFG.Label.Map.empty setMap
		end
		val callers : CFG.Label.Set.set CFG.Label.Map.map = fromCFGSolverSetMap callers
		fun getTails (headNode, loopSetList) =
		    List.mapPartial (Option.map debugTails o tailOfLoop callers headNode) loopSetList
		val tails = CFG.Label.Map.mapi getTails naturalLoops
	    in
		()
	    end
in
    m
end
end


