structure ListQuicksort =
  struct

    structure K = Int

    fun lessThan x y = (
	  case K.compare(x, y)
	   of LESS => true
	    | _ => false
          (* end case *))

    fun greaterThan x y = (
	  case K.compare(x, y)
	   of GREATER => true
	    | _ => false
          (* end case *))

    fun equal x y = (
	  case K.compare(x, y)
	   of EQUAL => true
	    | _ => false
          (* end case *))

    fun quicksort xs = (
	  case xs
	   of nil => nil
	    | p :: xs => let
		  val lt = List.filter (greaterThan p) xs
		  val eq = p :: List.filter (equal p) xs
		  val gt = List.filter (lessThan p) xs
	          in
		    quicksort lt @ eq @ quicksort gt
		  end
          (* end case *))

  end

structure Main =
  struct

    fun randomList n = List.tabulate(n, fn _ => Rand.inRangeInt(0, 10000))

    fun benchMergesort (seqSz, n, sort) = let
	  val r = randomList n
	  val b = Time.now ()
          val r = sort r
	  val e = Time.now ()
          in
	    Print.printLn(Time.toString (e-b));
	    ()
	  end

    val _ = benchMergesort(PrimIO.readInt(), PrimIO.readInt(), ListQuicksort.quicksort)

  end
