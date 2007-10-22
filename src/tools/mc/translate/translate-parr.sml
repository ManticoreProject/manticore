(* translate-parr.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure TranslateParr (* : sig

  end *) = struct

    structure A = AST
    structure V = Var
    structure B = BOM
    structure BV = B.Var
    structure BTy = BOMTy
    structure Lit = Literal
    structure E = TranslateEnv

    val trTy = TranslateTypes.tr

    structure Nat = LargeWord
    type nat      = Nat.word
    val natZero   = Nat.fromInt 0
    val natOne    = Nat.fromInt 1

    val maxLeafSize = Nat.fromInt 16

  (* max : nat * nat -> nat *)
    val max = Nat.max

  (* add1 : nat -> nat *)
    fun add1 n = Nat.+ (n, natOne)

  (* sub1 : nat -> nat *)
    fun sub1 n = Nat.- (n, natOne)

    datatype 'a leaf 
      = Lf of (nat * 'a list)

    datatype 'a rope 
      = Leaf of 'a leaf
      | Cat  of (nat * nat * 'a rope * 'a rope)
		(* length, depth, left, right *)

  (* Note the length and depth of ropes are precomputed, so length and depth *)
  (* are constant-time calculations. *)

    val emptyRope = Leaf (Lf (natZero, []))

  (* ropeLen : 'a rope -> nat *)
  (* Returns the number of elements of data at the leaves of the given rope. *)
    fun ropeLen (Leaf (Lf (len, _))) = len
      | ropeLen (Cat (len, _, _, _)) = len

  (* ropeDepth : 'a rope -> nat *)
  (* Returns the depth (length of the longest path to a leaf) of given rope. *)
  (* Note that by definition a leaf has depth 0. *)
    fun ropeDepth (Leaf _) = natZero
      | ropeDepth (Cat (_, d, _, _)) = d

  (* isEmpty : 'a rope -> bool *)
    fun isEmpty r = (ropeLen r = natZero)

  (* takeDrop : 'a list * nat -> 'a list * 'a list *)
  (* A function to do a "take" and a "drop" in one pass. *)
  (* ex: takeDrop ([5,6,7,8,9],2) --> ([5,6],[7,8,9]) *)
  (* Does not raise an exception if it runs out of things to take. *)
    fun takeDrop (xs, n) =
	let fun build (_, ekat, []) = (rev ekat, [])
	      | build (n, ekat, x::drop) = 
		  if n = natZero
		  then (rev ekat, drop)
		  else build (sub1 n, x::ekat, drop)
	in
	    build (n, [], xs)
	end
  
  (* mergeLeaves : 'a leaf * 'a leaf -> 'a rope *)
  (* This function merges two leaves into a single leaf if the total *)
  (* number of data elements does not exceed maxLeafSize. *)
  (* Otherwise, it packs maxLeafSize-worth of data into the a left leaf, *)
  (* the rest into a right leaf, and concatenates them. *)
    fun mergeLeaves (Lf (len1, xs1), Lf (len2, xs2)) =
	let val totalLen = len1 + len2
	    val xs = xs1 @ xs2
	in
	    if totalLen <= maxLeafSize
	    then Leaf (Lf (totalLen, xs))
	    else let val (leftData, rightData) = takeDrop (xs, maxLeafSize)
		     val left  = Lf (maxLeafSize, leftData) 
		     val right = Lf (totalLen - maxLeafSize, rightData)
		 in
		     Cat (totalLen, natOne, Leaf left, Leaf right)
		 end
	end
  
  (* cat : 'a rope * 'a rope -> 'a rope *)
  (* This constructor is not smart at all, i.e., does *no* analysis of its args. *)
  (* It is appropriate for use when a tree is known in advance to be balanced. *)
    fun cat (r1, r2) = 
	let val len = ropeLen r1 + ropeLen r2
	    val d = add1 (max (ropeDepth r1, ropeDepth r2))
	in
	    Cat (len, d, r1, r2)
	end

  (* mkCat : 'a rope * 'a rope -> 'a rope *)
  (* This is a slightly smart constructor for ropes. *)
    fun mkCat (r1, r2) =
	if isEmpty r1 then r2
	else if isEmpty r2 then r1
	else (case (r1, r2)
		of (Leaf L1, Leaf L2) => mergeLeaves (L1, L2)
		 | _ => cat (r1, r2)
	       (* end case *))

  (* smartCat : 'a rope * 'a rope -> 'a rope *)
  (* A smart constructor (i.e., with balancing) for ropes. *)
    fun smartCat (r1, r2) = raise Fail "todo: smartCat"
  
  (* ropeFromExpList : A.exp list -> A.exp rope *)
  (* Consumes a list of expressions and produces an ideally-balanced *)
  (* rope from them. *)
    fun ropeFromExps es =
	  let (* makeLeaf : 'a list -> 'a rope *)
	      (* pre: length xs does not exceed maxLeafSize *)
	      fun makeLeaf xs =
		  let val len = Nat.fromInt (length xs)
		  in
		      Leaf (Lf (len, xs))
		  end
	      (* makeLeaves : 'a list -> 'a rope list *)
	      (* To chop up a list and put the data into leaves. *)
	      fun makeLeaves xs =
		  let fun b ([], acc) = rev acc
			| b (xs, acc) =
		          let val (xsFront, xsBack) = takeDrop (xs, maxLeafSize)
			      val leaf = makeLeaf xsFront
			  in
			      case xsBack
			       of [] => rev (leaf :: acc)
				| _ => b (xsBack, leaf :: acc)
			  end
		  in
		      b (xs, [])
		  end
	      (* catPairs : 'a rope list -> 'a rope list *)
	      (* Concatenate the ropes in a list, two at a time. *)
	      fun catPairs [] = []
		| catPairs [r] = [r]
		| catPairs (r1::r2::rs) = mkCat (r1, r2) :: catPairs rs  
	      (* ropeFromRopeList : 'a rope list -> 'a rope *)
	      (* Concatenate ropes in a list *)
	      fun ropeFromRopeList []  = emptyRope
		| ropeFromRopeList [r] = r
		| ropeFromRopeList rs  = ropeFromRopeList (catPairs rs)
	  in
	      ropeFromRopeList (makeLeaves es)
	  end
	  
  (* trExp : (env * A.exp -> 'a) -> env * A.exp -> 'a *)
    fun translate trExp (env, exp) = 
	(case exp
	  of A.PArrayExp (es, t) => 
	     let val r = ropeFromExps es
	     in
		 raise Fail "todo: TranslateParr.translate"
	     end
	   | e => trExp (env, e)
	(* end case *))
	
  end
