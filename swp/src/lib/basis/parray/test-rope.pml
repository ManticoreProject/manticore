structure TestRope = struct


    structure S = ListSeq
    structure R = Rope
    val print = Print.print
    val itos = Int.toString
    fun not b = if b then false else true

    val sizeL1CacheLine= 2
    val wordSize = 32
    val ceilingLg = Int.ceilingLg
		    
  type 'a rope = 'a R.rope

    fun prope show r = print (R.toString show r)

(* int * int -> int rope *)
  fun spineRope (lo, hi) = let
    val len = hi - lo + 1
    fun f n = lo + n
    in
      if len <= R.maxLeafSize
      then R.fromSeq (R.S.tabulate (len, f))
      else R.concatWithoutBalancing (R.fromSeq (R.S.tabulate (R.maxLeafSize, f)),
				     spineRope (lo + R.maxLeafSize, hi))
    end

  fun invert r = (
        case r
	 of R.LEAF _ => r
	  | R.CAT(depth, len, r1, r2) => R.CAT(depth, len, invert r2, invert r1)
        (* end case *))

  fun doubleSpineRope (lo, hi) = 
    R.concatWithoutBalancing (invert(spineRope(lo, hi div 2)), spineRope(hi div 2 + 1, hi))

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

(*
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
*)

end
