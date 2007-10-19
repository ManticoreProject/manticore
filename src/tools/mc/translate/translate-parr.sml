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
  fun max (m,n) = if Nat.>(m,n) then m else n

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

  val emptyRope = Leaf (Lf (natZero, []))

  (* ropeLen : 'a rope -> nat *)
  fun ropeLen (Leaf (Lf (len, _))) = len
    | ropeLen (Cat (len, _, _, _)) = len

  (* ropeDepth : 'a rope -> nat *)
  fun ropeDepth (Leaf _) = natZero
    | ropeDepth (Cat (_, d, _, _)) = d

  (* isEmpty : 'a rope -> bool *)
  fun isEmpty r = (ropeLen r = natZero)

  (* takeDrop : 'a list * nat -> 'a list * 'a list *)
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
  fun mergeLeaves (Lf (len1, xs1), Lf (len2, xs2)) =
      let val totalLen = len1 + len2
	  val xs = xs1 @ xs2
      in
	  if LargeWord.<= (totalLen, maxLeafSize)
	  then Leaf (Lf (totalLen, xs))
	  else let val (leftData, rightData) = takeDrop (xs, maxLeafSize)
		   val left  = Lf (maxLeafSize, leftData) 
		   val right = Lf (totalLen - maxLeafSize, rightData)
	       in
		   Cat (totalLen, natOne, Leaf left, Leaf right)
	       end
      end

  (* smartCat : 'a rope * 'a rope -> 'a rope *)
  fun smartCat (r1, r2) = raise Fail "todo: smartCat"

  (* mkCat : 'a rope * 'a rope -> 'a rope *)
  fun mkCat (r1, r2) =
      if isEmpty r1 then r2
      else if isEmpty r2 then r1
      else (case (r1, r2)
	      of (Leaf L1, Leaf L2) => mergeLeaves (L1, L2)
	       | _ => let val len  = ropeLen r1 + ropeLen r2
			  val dpth = add1 (max (ropeDepth r1, ropeDepth r2))
		      in
			  Cat (len, dpth, r1, r2)
		      end
	     (* end case *))

  (* catPairs : 'a rope list -> 'a rope list *)
  fun catPairs [] = []
    | catPairs [r] = [r]
    | catPairs (r1::r2::rs) = mkCat (r1, r2) :: catPairs rs

  (* ropeFromRopeList : 'a rope list -> 'a rope *)
  fun ropeFromRopeList []  = emptyRope
    | ropeFromRopeList [r] = r
    | ropeFromRopeList rs  = ropeFromRopeList (catPairs rs)

  (* makeLeaf : 'a list -> 'a leaf *)
  fun makeLeaf xs = 
      let val len = Nat.fromInt (length xs)
      in
	  Lf (len, xs)
      end
	  
  (* makeLeaves : 'a list -> 'a leaf list *)
  fun makeLeaves xs = 
      let fun build ([], acc) = acc
	    | build (xs, acc) = 
	        let val (xsFront, xsBack) = takeDrop (xs, maxLeafSize)
		    val leaf = makeLeaf xsFront
		in
		    case xsBack
		      of [] => rev (leaf :: acc)
		       | _ => build (xsBack, leaf :: acc)
		end
      in
	  build (xs, [])
      end

  (* trExp : (env * A.exp -> 'a) -> env * A.exp -> 'a *)
  fun translate trExp (env, exp) = 
      (case exp
	 of A.PArrayExp (es, t) => 
	      let val r = (ropeFromRopeList o map Leaf o makeLeaves) es
	      in
		  raise Fail "todo: TranslateParr.translate"
	      end
	  | e => trExp (env, e)
        (* end case *))

  end
