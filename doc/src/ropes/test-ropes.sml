(* test-ropes.sml
 *)

structure TestRopes = struct

  structure ListSeq : SEQ = struct
    type 'a seq = 'a list
    val empty = []
    fun singleton s = [s]
    val null = null
    val length = length
    val sub = List.nth
    val concat = op @
    fun splitAt _ = raise Fail "todo"
    fun fromList x = x
    fun toList x = x 
    val take = List.take
    val drop = List.drop
  end

  structure R = RopesFn (structure S = ListSeq 
                         val maxLeafSize = 2
			 val maxDepth = 32)

  type 'a rope = 'a R.rope

(* int * int -> int rope *)
  fun spineRope (lo, hi) = let
    val len = hi - lo + 1
    fun f n = lo + n
    in
      if len <= R.maxLeafSize
      then R.fromList (List.tabulate (len, f))
      else R.concat (R.fromList (List.tabulate (R.maxLeafSize, f)),
		     spineRope (lo + R.maxLeafSize, hi))
    end

  fun invert (r as R.LEAF(_, _)) = r
    | invert (R.CAT(depth, len, r1, r2)) = R.CAT(depth, len, invert r2, invert r1)

  fun doubleSpineRope (lo, hi) = R.concat(invert(spineRope(lo, hi div 2)), spineRope(hi div 2 + 1, hi))

  fun println s = (print s; print "\n")

  fun prope show r = print (R.toString show r)

  val itos = Int.toString

  fun t n = let
    val r = spineRope (0, n-1)
    val b = R.balance r
    in
      print "---------- before balancing ----------\n";
      prope itos r;
      print "---------- after balancing  ----------\n";
      prope itos b
    end

  fun t' n = let
    val r = doubleSpineRope (0, n-1)
    val b = R.balance r
    in
      print "---------- before balancing ----------\n";
      prope itos r;
      print "---------- after balancing  ----------\n";
      prope itos b
    end

  fun test 0 = t 7
    | test 1 = t 14
    | test 2 = t' 16
    | test 3 = let
        val r = R.concat (R.singleton 0, R.singleton 1)
        in
          prope itos r
        end
    | test 4 = let
        val r = R.concat (R.singleton 0, spineRope (1, 6))
        in
          prope itos r
        end
    | test 5 = let
        val r = R.concat (spineRope (0, 1),
			  R.singleton 2)
        in
          prope itos r
        end
    | test 6 = let
        val rs = map R.singleton [1,2,3,4,5,6,7,8]
	val r = List.foldr R.concat (R.singleton 0) rs        
        in
          prope itos r
        end
    | test 7 = t 9
    | test 8 = prope itos (spineRope (0, 3))
    | test n = (println ("No such test: " ^ itos n);
		raise Fail "")

end
