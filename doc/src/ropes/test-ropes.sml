(* test-ropes.sml
 *)

structure TestRopes = struct

  (* log : real -> (real -> real) *)
    fun log base x = Math.ln x / Math.ln base

  (* ceilingLg : int -> int *)
  (* The ceiling of the log_2 of the input. *)
    val ceilingLg = ceil o log 2.0 o real

    val itos = Int.toString


  structure R = RopesFn (structure S = ListSeq 
                         val sizeL1CacheLine= 2
			 val wordSize = 32
			 val ceilingLg = ceilingLg)

  type 'a rope = 'a R.rope

    fun prope show r = print (R.toString show r)

(* int * int -> int rope *)
  fun spineRope (lo, hi) = let
    val len = hi - lo + 1
    fun f n = lo + n
    in
      if len <= R.maxLeafSize
      then R.fromSeq (List.tabulate (len, f))
      else R.concatWithoutBalancing (R.fromSeq (List.tabulate (R.maxLeafSize, f)),
				     spineRope (lo + R.maxLeafSize, hi))
    end

  fun invert r = (
        case r
	 of R.LEAF(_, _) => r
	  | R.CAT(depth, len, r1, r2) => R.CAT(depth, len, invert r2, invert r1)
        (* end case *))

  fun doubleSpineRope (lo, hi) = 
    R.concatWithoutBalancing (invert(spineRope(lo, hi div 2)), spineRope(hi div 2 + 1, hi))

  fun println s = (print s; print "\n")

  fun check c = if not c then raise Fail "" else ()

  fun testBal mkRope n = let
    val r = mkRope (0, n-1)
    val b = R.balance r
    in
      check(R.isBalanced b);
      print "---------- before balancing ----------\n";
      prope itos r;
      print "---------- after balancing  ----------\n";
      prope itos b;
(*      print (String.concat ["---------- the depth before balancing was ",
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

  fun testSplitAt n i = let
        val ls = List.tabulate(n, fn n => n)
	val (ls1, ls2) = ListSeq.splitAt(ls, i)
	val r = R.fromSeq ls
	val (r1, r2) = R.splitAt(r, i)
        in
          prope itos r;
	  print "r1\n";
          prope itos r1;
	  print "r2\n";
          prope itos r2;
          check(List.all (fn i => i = R.sub(r1, i)) ls1);
          check(List.all (fn i => i = R.sub(r2, i-R.length r1)) ls2);
	  R.length r2
        end

  fun test 0 = t 7
    | test 1 = t 14
    | test 2 = t' 16
    | test 3 = let
        val r = R.concatWithoutBalancing (R.singleton 0, R.singleton 1)
        in
          prope itos r
        end
    | test 4 = let
        val r = R.concatWithoutBalancing (R.singleton 0, spineRope (1, 6))
        in
          prope itos r
        end
    | test 5 = let
        val r = R.concatWithoutBalancing (spineRope (0, 1),
			  R.singleton 2)
        in
          prope itos r
        end
    | test 6 = let
        val rs = map R.singleton [1,2,3,4,5,6,7,8]
	val r = List.foldr R.concatWithoutBalancing (R.singleton 0) rs        
        in
          prope itos r
        end
    | test 7 = t 9
    | test 8 = prope itos (spineRope (0, 3))
    | test 9 = t 100
    | test n = (println ("No such test: " ^ itos n);
		raise Fail "")

end
