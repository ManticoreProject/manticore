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
end

signature SOLVER_PARAM = sig
    type ord_key
    type graph
    val compare : ord_key * ord_key -> order
    val toSexp : ord_key -> Sexp.t

    (* (root node * (node * callees) list) *)
    val makeGraph : graph -> (ord_key * (ord_key * ord_key list) list)
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

(* (k, (v1, v2, ...)) in dominators (n0, nodes, callers) means block k is
				    dominated by v1, v2, ... in the call graph rooted at n0. *)
fun dominators code : LabelSet.set LabelMap.map = let

    (* TODO can SML not do pattern-matching here? *)
    val tmp = Ord.makeGraph code
    val n0 = #1 tmp
    val callees_alist = #2 tmp

    fun debug dom : LabelSet.set LabelMap.map =
	let
	    fun addNewline s = s ^ "\n"
	    val () = (TextIO.print o addNewline o Sexp.toString o LabelMap_toSexp LabelSet_toSexp) dom
	in
	    dom
	end

    (* (k, (v1, v2, ...)) in callers means blocks v1, v2, ... can jump to block k *)
    val callers : LabelSet.set LabelMap.map =
	let
	    fun f (key, value, acc) =
		let
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

    val _ = debug callers

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
	val _ = debug dom

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
	if continue then iter dom' else debug dom'
    end
in
    iter dom
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

structure CFGSolver = Solver(struct
			     type ord_key = CFG.label
			     type graph = CFG.func list
			     val compare = VarRep.compare

			     fun toSexp v = Sexp.Atom (VarRep.toString v)

			     (* construct the flow graph for a module, copy-pasted from AddAllocChecksFn *)
			     (* TODO find a way to call directly from AddAllocChecksFn instead of copy-pasting *)
			     fun makeGraph code = let
				 fun getN0 code =
				     case code
				      of [] => raise Fail "unroll-loops: empty program"
				       | CFG.FUNC {start = CFG.BLK{lab, ...} , ...} :: _ => lab
				 (* end case *)

				 (* return the outgoing targets of a function *)
				 fun nodeFromBlock (CFG.BLK{lab, exit, ...}) =
				     (case CFA.labelsOf exit
				       of NONE => (lab, [])
					| SOME ls => (lab, CFG.Label.Set.listItems ls)
				     (* end case *))
				 fun toNode (CFG.FUNC{start,body,...}) =
				     List.foldr (fn (b,rr) => (nodeFromBlock b)::rr) [] (start::body)
			     in
				 (getN0 code, List.foldr (fn (node,rr) => (toNode node)@rr) [] code)
			     end
			     end)

structure IntSolver = Solver(struct
				type ord_key = int
				type graph = (ord_key * (ord_key * ord_key list) list)
				val compare = Int.compare
				fun toSexp i = Sexp.Atom (Int.toString i)
				fun makeGraph graph = graph
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

fun transform m = let
    val () = (TextIO.print o Sexp.toString o IntSolver.LabelMap_toSexp IntSolver.LabelSet_toSexp o IntSolver.dominators) test
in
    m
end
end
