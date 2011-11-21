(* test-rope.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Ropes for Standard ML.
 * 
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 * n.b. Uncomment sections of sources.cm as well as the ROPE signature in rope-fn 
 *      to run these tests.
 *)

structure TestRope = struct

(* non-specific utilities *)

  fun println s = (print s; print "\n")

  fun check c = println ("Test " ^ (if c then "passed." else "failed."))

  val itos = Int.toString

(* test ropes with the dummy implementation ListSeq *)

  structure R = RopeImplFn (
    structure S = ListSeq 
    val maxLeafSize= 2
  )

  fun prRope show r = print (R.toString show r)

  fun spineRope (lo, hi) = let
    val len = hi - lo + 1
    fun f n = lo + n
    in
      if len <= R.maxLeafSize
      then R.fromSeq (List.tabulate (len, f))
      else R.appendWithoutBalancing (R.fromSeq (List.tabulate (R.maxLeafSize, f)),
				     spineRope (lo + R.maxLeafSize, hi))
    end

  fun invert (r as R.Leaf _) = r
    | invert (R.Cat (d, n, rL, rR)) = R.Cat (d, n, invert rR, invert rL)

  fun doubleSpineRope (lo, hi) = 
    R.appendWithoutBalancing (invert(spineRope(lo, hi div 2)), spineRope(hi div 2 + 1, hi))

  fun testBal mkRope n = let
    val r = mkRope (0, n-1)
    val b = R.balance r
    in
      check(R.isBalanced b);
      print "---------- before balancing ----------\n";
      prRope itos r;
      print "---------- after balancing  ----------\n";
      prRope itos b;
(*
      print (String.concat ["---------- the depth before balancing was ",
			    itos (R.depth r),
			    ", after was ",
			    itos (R.depth b),
			    "\n"]);
      print (String.concat ["---------- the expected depth of the balanced rope is <= ",
			    itos (2 + (ceil (Math.ln (real (R.length r)) / Math.ln 2.0))),
			    "\n"])
*)
      ()
    end

  val t  = testBal spineRope
  val t' = testBal doubleSpineRope

  fun testSub n = let
        val ls = List.tabulate(n, fn n => n)
        val r = R.fromSeq ls
        in
          check(List.all (fn i => i = R.sub(r, i)) ls)
        end

  fun testCut n i = let
        val ls = List.tabulate(n, fn n => n)
	val (ls1, ls2) = ListSeq.cut(ls, i+1)
	val r = R.fromSeq ls
	val (r1, r2) = R.cut(r, i+1)
        in
          prRope itos r;
	  print "r1\n";
          prRope itos r1;
	  print "r2\n";
          prRope itos r2;
          check(List.all (fn i => i = R.sub(r1, i)) ls1);
          check(List.all (fn i => i = R.sub(r2, i-R.length r1)) ls2);
	  R.length r2
        end

  fun even n = (n mod 2) = 0

  fun explode' s = List.map Char.toString (String.explode s)

  val alphabetRope = R.fromString "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  val prIntRope = prRope itos
  val prCharRope = prRope Char.toString

  fun test 0 = t 7
    | test 1 = t 14
    | test 2 = t' 16
    | test 3 = let
        val r = R.appendWithoutBalancing (R.singleton 0, R.singleton 1)
        in
          prRope itos r
        end
    | test 4 = let
        val r = R.appendWithoutBalancing (R.singleton 0, spineRope (1, 6))
        in
          prRope itos r
        end
    | test 5 = let
        val r = R.appendWithoutBalancing (spineRope (0, 1),
					  R.singleton 2)
        in
          prRope itos r
        end
    | test 6 = let
        val rs = map R.singleton [1,2,3,4,5,6,7,8]
	val r = List.foldr R.appendWithoutBalancing (R.singleton 0) rs        
        in
          prRope itos r
        end
    | test 7 = t 9
    | test 8 = prRope itos (spineRope (0, 3))
    | test 9 = t 100
    | test 10 = check (R.foldl (op ^) "" (R.fromSeq ["a","b","c","d","e"]) = "edcba")
    | test 11 = check (R.foldr (op ^) "" (R.fromSeq ["a","b","c","d","e"]) = "abcde")
    | test 12 = prRope itos (R.fromList [1,2,3,4,5,6,7])
(*    | test 13 = prCharRope (R.subrope (alphabetRope, 2, 5))
    | test 14 = prCharRope (R.subrope (alphabetRope, 0, 5))
*)
    | test 15 = prCharRope (R.rev alphabetRope)
    | test 16 = prCharRope (R.take (alphabetRope, 3))
    | test 17 = prCharRope (R.drop (alphabetRope, 3))
    | test 18 = let
        val (abc, rest) = R.cut (alphabetRope, 3)
        in
	  List.app prCharRope [abc, rest]
        end
    | test 19 = prIntRope (R.map ord alphabetRope)
    | test 20 = let
        fun p c = if even (ord c) then SOME c else NONE
        in
          prCharRope (R.mapPartial p alphabetRope)
        end
    | test 21 = prCharRope (R.filter (fn c => c = #"A" orelse c = #"Z") alphabetRope)
    | test 22 = let
        val (rT, rF) = R.partition (even o ord) alphabetRope
        in 
	  List.app prCharRope [rT, rF]
        end
    | test 23 = R.app (println o Char.toString) alphabetRope
    | test 24 = (case R.find (fn c => c = #"X") alphabetRope
		   of SOME #"X" => println "SOME X"
		    | SOME _ => println "Huh?"
		    | NONE => println "NONE"
		   (* end case *))
    | test 25 = check (R.exists (even o ord) alphabetRope)
    | test 26 = check (not (R.all (even o ord) alphabetRope))
    | test 27 = prCharRope (R.tabulate (20, fn n => chr (n + 100)))
    | test n  = println ("No such test: " ^ itos n)		

end
