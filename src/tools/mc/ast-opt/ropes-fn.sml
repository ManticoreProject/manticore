(* ropes-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A prototype implementation of ropes in SML.
 * N.B. Not directly used in the compiler.
 *)

signature ARCH = sig
  val cacheLineSizeBytes : int
  val wordSizeBytes : int
end

functor RopesFn (Arch : ARCH) (* : ROPES *) = 
   
  struct

    structure A = AST
    structure T = Types

    val debugging = ref false
    fun dprint s = if !debugging then print s else ()

    datatype 'a rope
      = Leaf of int * 'a list
      | Concat of int * 'a rope * 'a rope

    val maxLeafSize = Arch.cacheLineSizeBytes div Arch.wordSizeBytes

    (* empty : 'a rope *)
    val empty = Leaf (0, [])

    (* isLeaf : 'a rope -> bool *)
    fun isLeaf (Leaf _) = true
      | isLeaf _ = false

    (* ropeLen : 'a rope -> int *)
    fun ropeLen (Leaf (n, _)) = n
      | ropeLen (Concat (n, _, _)) = n

    (* max : int * int -> int *)
    fun max (m:int, n) = if m>n then m else n

    (* ropeDepth : 'a rope -> int *)
    (* A leaf has depth 0. *)
    fun ropeDepth (Leaf _) = 0
      | ropeDepth (Concat (_, r1, r2)) = 1 + max (ropeDepth r1, ropeDepth r2)

    (* isEmpty : 'a rope -> bool *)
    fun isEmpty r = (ropeLen(r) = 0)

    (* singleton : 'a -> 'a rope *)
    fun singleton x = Leaf (1, [x])

    (* leaves : 'a rope -> 'a rope list *)
    (* post: all ropes in the output are leaves *)
    fun leaves (L as Leaf _) = [L]
      | leaves (Concat (_, r1, r2)) = leaves r1 @ leaves r2

    (* concat : 'a rope * 'a rope -> 'a rope *)
    fun concat (r1, r2) =
	  if isEmpty r1 then r2
	  else if isEmpty r2 then r1
	  else Concat(ropeLen(r1)+ropeLen(r2),r1,r2)

    (* mergeLeaves : int * 'a list * 'a list -> 'a rope *)
    (* Combines two leaves into one when possible. *)
    (* post: output leaves never exceed maxLeafSize *)
    fun mergeLeaves (r1 as Leaf(n1,xs1), r2 as Leaf(n2,xs2)) =
	  let val n = n1+n2
	  in
	      if n <= maxLeafSize then
		  Leaf(n,xs1@xs2)
	      else
		  Concat(n,r1,r2)
	  end
      | mergeLeaves _ = raise Fail "mergeLeaves: both arguments must be leaves"

    (* smartBuild : 'a rope * 'a rope -> 'a rope *)
    fun smartBuild (r1 as Leaf _, r2 as Leaf _) = mergeLeaves (r1, r2)
      | smartBuild (r1 as Leaf(n1,xs1), r2 as Concat(n2,r2L,r2R)) =
	  (case r2L
	     of Leaf _ => 
		  let val r = mergeLeaves (r1, r2L)
		  in
		      Concat (n1+n2,r,r2R)
		  end
	      | _ => Concat (n1+n2,r1,r2)
 	    (* end case *))
      | smartBuild (r1 as Concat(n1,r1L,r1R), r2 as Leaf(n2, xs2)) =
	  (case r1R
	     of Leaf _ =>
		  let val r = mergeLeaves (r1R, r2)
		  in
		      Concat (n1+n2, r1R, r2)
		  end
	      | _ => Concat (n1+n2, r1, r2)
	  (* end case *))
      | smartBuild (r1 as Concat _, r2 as Concat _) = 
	  Concat (ropeLen(r1)+ropeLen(r2), r1, r2)

    (* fib : int -> int *)
    fun fib n =
	let fun f (n (* >= 2 *), penult, ult) =
	        if n=2 then penult + ult
		else f (n-1, ult, penult + ult)
	in
	    if n<0 then raise Fail "fib: negative argument"
	    else if n=0 then 0
	    else if n=1 then 1
	    else f (n, 0, 1)
	end
	
    (* fibfloor : int -> int *)
    (* Compute the index of the greatest lower Fibonacci number of the arg. *)
    (* Note: If the argument is 1, the result is 2. *)
    (* FIXME This could be implemented more efficiently. *)
    fun fibfloor n =
	let fun find f = if fib(f) > n then f-1 else find(f+1)
	in
	    dprint "fibfloor\n";
	    if n<1 then raise Fail "fibfloor: n<1"
	    else find 2
	end
	
    (* concatAllUpTo : int * 'a rope list -> 'a rope *)
    (* note: 1-based *)
    fun concatAllUpTo (n, rs) = 
	let (* forgiving take *)
	    fun take' (xs, n) = 
		let val len = List.length xs
		in
		    dprint "take'\n";
		    if n >= len then xs else List.take (xs, n)
		end
	in
	    dprint "concatAllUpTo\n";
	    foldr smartBuild empty (take' (rs, n))
	end
				  
    (* insert : 'a rope * 'a rope list -> 'a rope list *)
    fun insert (leaf, ropes) =
	let val greatestLowerFib = fibfloor (ropeLen leaf)
	    val bigCat = concatAllUpTo (greatestLowerFib-1, ropes)
	    val r = smartBuild (bigCat, leaf)
	    fun build (n, acc) = if n=0 then acc else build (n-1, empty::acc)
	    (* forgiving drop *) 
	    fun drop' (xs, n) = (dprint "drop'\n";
				 if (length xs) <= n then [] else List.drop (xs, n))
	in
	    dprint "insert\n";
	    if isEmpty(leaf) then ropes
	    else (build (greatestLowerFib-1, [r])) @ drop' (ropes, greatestLowerFib-1)
	end

    (* isBalanced : 'a rope -> bool *)
    fun isBalanced r = isEmpty(r) orelse false (* conservative *)
		       
    (* balance : 'a rope -> 'a rope *)
    fun balance r = 
	let fun bal ([], acc) = foldr smartBuild empty (rev acc)
	      | bal (leaf::leaves, acc) = bal (leaves, insert (leaf, acc))
	in
	    dprint "balance\n";
	    if isBalanced(r) then r else bal (leaves r, [])
	end
	
    (* smartConcat : 'a rope * 'a rope -> 'a rope *)
    fun smartConcat (r1, r2) = (balance o smartBuild) (r1, r2)
			       
    (* sublist : 'a list * int * int -> 'a list *)
    fun sublist (xs, start, len) = 
	  let fun build ([], n, acc) = 
		    if n=0 
		    then rev acc
		    else raise Fail "sublist: not enough elements"
		| build (x::xs, n, acc) = build (xs, n-1, x::acc)
	  in
	      build (List.drop (xs, start), len, [])
	  end

    (* subrope : 'a rope * int * int -> 'a rope *)
    (* FIXME: Decide whether to be forgiving or unforgiving of *)
    (*        indices outside range. *)
    fun subrope (r as Leaf(n,xs), start, len) =
	  if start < 0 then
	      raise Fail  "subrope: negative start index"
	  else if start=0 andalso len>=n then
	      r
	  else
	      Leaf (len, sublist (xs,start,len))
      | subrope (r as Concat(n,r1,r2), start, len) =
	  let val left = if start<0 then
			     raise Fail "subrope: negative start index"
			 else if start=0 andalso ropeLen(r1)>=len then
			     r1
			 else
			     subrope (r1, start, len)
	      val right = if start<=ropeLen(r2) andalso (start+len)>=n then
			      r2
			  else
			      subrope (r2, start-ropeLen(r1), len-ropeLen(left))
	  in
	      if n > len then
		  r
	      else
		  Concat(len, left, right)
	  end

    (* toList : 'a rope -> 'a list *)
    fun toList (Leaf (_, xs)) = xs
      | toList (Concat (_, r1, r2)) = toList r1 @ toList r2

    (* divUp : int * int -> int *)
    (* Computes the ceiling of the first int over the other. *)
    (* ex: divUp (7, 10)  => 1 *)
    (* ex: divUp (10, 10) => 1 *)
    (* ex: divUp (11, 10) => 2 *)
    fun divUp (m, n) = (m div n) + (if (m mod n) = 0 then 0 else 1)

    (* twoToThe : int -> int *)
    fun twoToThe n = 
	  let fun build (0, acc) = acc
		| build (n, acc) = build (n-1, acc*2)
	  in
	      build (n, 1)
	  end

    (* log2 : int -> int *)
    (* Computes the ceiling of the log_2 of the argument. *)
    (* ex: log2 7 => 3 *)
    (* ex: log2 8 => 3 *)
    (* ex: log2 9 => 4 *) 
    fun log2Up n = ceil (Math.log10(real(n)) / Math.log10(2.0))

    (* takedrop : 'a list * int -> 'a list * 'a list *)
    fun takedrop (xs, n) =
	let fun td (_, front, []) = (rev front, [])
	      | td (n, front, rear) =
		  if n=0 then
		      (rev front, rear)
		  else
		      td (n-1, hd rear :: front, tl rear)
	in
	    td (n, [], xs)
	end

    (* fromList : 'a list -> 'a rope *)
    fun fromList xs =
	  let fun mkBatches [] = []
		| mkBatches xs =
		    if length xs < maxLeafSize then
			[xs]
		    else
			cleave xs
	      and cleave xs = 
		    let fun takedrop (_, front, []) = [(rev front)]
			  | takedrop (n, front, rear) =
			      if n=0 then
				  rev front :: mkBatches rear
			      else 
				  takedrop (n-1, hd rear :: front, tl rear)
		    in
			takedrop (maxLeafSize, [], xs)
		    end
	  in
	      fromBatches (mkBatches xs)
	  end

    (* fromBatches : 'a list list -> 'a rope *)
    (* pre: the size of a batch does not exceed maxLeafSize *)
    and fromBatches [] = empty
      | fromBatches [xs] = Leaf (length xs, xs)
      | fromBatches xss =
	  let val nBatches = length xss
	      val depth = log2Up nBatches
	      val leavesOnLeft = twoToThe (depth-1)
	      val (leftBatches, rightBatches) = takedrop (xss, leavesOnLeft)
	      val rL = fromBatches leftBatches
	      val rR = fromBatches rightBatches
	  in
	      concat (rL, rR)
	  end

    (* copies : 'a -> int -> 'a list *)    
    (* pre: n is not negative *)
    fun copies x n = if n<0 then raise Fail "copies: n is negative"
		     else if n=0 then []
		     else List.tabulate (n, fn _ => x)

    (* toString : ('a -> string) -> 'a rope -> string *)
    fun toString show r =
	  let val rootString = "C<"
	      val spaces = copies " "
	      val indenter = String.concat (spaces (String.size rootString))
	      (* indent : string list -> string list *)
	      val indent = map (fn s => indenter ^ s) 
	      (* build : 'a rope -> string list *)
	      fun build (Leaf (_, xs)) = 
		    let fun b ([], acc) = "]"::acc
			  | b ([x], acc) = b ([], (show x)::acc)
			  | b (x::xs, acc) = b (xs, ","::(show x)::acc)
		    in
			[(String.concat o rev) (b (xs, ["["]))]
		    end
		| build (Concat (_, r1, r2)) = 
		    let val ss1 = build r1
			val ss2 = build r2
		    in
			(indent ss1) @ (rootString :: (indent ss2))
		    end
	  in
	      String.concatWith "\n" (build r @ ["\n"])
	  end

    (**** tests located in module RopeTests ****)
			      
  end (* functor *)
