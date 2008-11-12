(* ropes-fn.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A prototype implementation of ropes in SML.
 *)

functor RopesFn (

  structure S : SEQ

  val maxLeafSize : int

  ) (*: ROPES*) = struct

    structure S = S
    type 'a seq = 'a S.seq

    datatype 'a rope
      = CAT of (int *                  (* depth *)
		int *                  (* length *)
		'a rope *              (* left subtree *)
		'a rope                (* right subtree *))
      | LEAF of (int *                 (* length *)
		 'a seq                (* sequence *))

    val maxLeafSize = maxLeafSize

    val empty = LEAF(0, S.empty)

    fun isEmpty (LEAF (0, _)) = true
      | isEmpty (CAT (_, 0, _, _)) = true
      | isEmpty _ = false

    fun length r = (
	  case r
	   of LEAF (len, _) => len
	    | CAT(_, len, _, _) => len
          (* end case *))

    fun depth r = (
	  case r
	   of LEAF (_, _) => 0
	    | CAT(depth, _, _, _) => depth
          (* end case *))

    fun fib n = let
	  fun ff (0, u, p) = u
	    | ff (n, u, p) = ff (n-1, u+p, u)
        in
	  case Int.compare (n, 1)
	   of LESS => 0
	    | EQUAL => 1
	    | GREATER => ff (n, 0, 1)
        end  

  (* the index of the smallest fibonacci number greater than len.
   *   e.g., balancerLen 34 = 8
   *)
    fun balancerLen len = let
	  fun lp n =
	        if fib n > len
		   then n
		else lp (n + 1)
          in
	    lp 0 - 2
	  end

  (* at each position the balancer contains
   *   - an inclusive lower bound on the rope length that may inhabit the 
   *      location (the inclusive lower bound is fib(n+1) where n is the index 
   *      of that spot)
   *   - an exclusive upper bound
   *   - some rope or none
   *)
    type 'a balancer = (int * int * 'a rope option) list

  (* takes a rope length, and returns a rope balancer *)
    fun mkInitialBalance len = 
	  List.tabulate(balancerLen len, fn n => (fib(n + 2), fib(n + 3), NONE))

    fun concat (r1, r2) = 
     (if isEmpty r1 then r2
      else if isEmpty r2 then r1
      else CAT(1 + Int.max(depth r1, depth r2), length r1 + length r2, r1, r2))

  (* concatenate all ropes in the balancer *)
    fun catAll balancer = (
	case balancer
	 of nil => empty
	  | (_, _, SOME r) :: nil => r
	  | (_, _, NONE) :: nil => empty
	  | (_, _, SOME r) :: rs => concat(catAll rs, r)
	  | (_, _, NONE) :: rs => catAll rs
        (* end case *))

  (* insert a rope into a balancer. we maintain the invariant that the length of the rope
   * at position i is < #1(balancer[i]) (the includsive lower bound). see Boehm '95 for more
   * detail.
   *) 
    fun insert (r, balancer) = (
	  case balancer
	   of nil => raise Fail "empty balancer"
	    | (lb, ub, NONE) :: balancer => 
	      if length r >= lb andalso length r < ub
	         then (lb, ub, SOME r) :: balancer
	      else (lb, ub, NONE) :: insert(r, balancer)
	    | (lb, ub, SOME r') :: balancer =>
	      (lb, ub, NONE) :: insert(concat(r', r), balancer)
        (* end case *))

  (* takes a rope and returns the list of leaves in order *)
    fun leaves r = (
	  case r
	   of LEAF _ => r :: nil
	    | CAT (_, _, r1, r2) => leaves r1 @ leaves r2
          (* end case *))

  (* balance a rope. this operation is O(n*log n) in the number of leaves *)
    fun balance r = catAll(List.foldl insert (mkInitialBalance(length r)) (leaves r))

    fun toSeq r = (
	  case r
	   of LEAF(_, s) => s
	    | CAT(_, _, r1, r2) => S.concat(toSeq r1, toSeq r2)
          (* end case *))

    fun fromList xs = let
      val len = List.length xs
      in
	if len <= maxLeafSize
	then LEAF (len, S.fromList xs)
	else raise Fail "todo"
      end

  (* toString : ('a -> string) -> 'a rope -> string *)
    fun toString show r = let
      fun copies thing n = List.tabulate (n, fn _ => thing)
      val rootString = "C<"
      val spaces = copies " "
      val indenter = String.concat (spaces (String.size rootString))
      (* indent : string list -> string list *)
      val indent = map (fn s => indenter ^ s) 
      (* build : 'a rope -> string list *)
      fun build (LEAF (_, xs)) = let 
            fun b ([], acc) = "]"::acc
	      | b ([x], acc) = b ([], (show x)::acc)
	      | b (x::xs, acc) = b (xs, ","::(show x)::acc)
            in
	      [(String.concat o rev) (b (S.toList xs, ["["]))]
            end
	| build (CAT (_, _, r1, r2)) = let 
            val ss1 = build r1
	    val ss2 = build r2
	    in
	      (indent ss1) @ (rootString :: (indent ss2))
	    end
    in
      String.concatWith "\n" (build r @ ["\n"])
    end

  end
