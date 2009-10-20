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


    val dfltN = 100000
	
    fun main (_, args) =
	let
	    val n = (case args
		      of arg :: _ => Option.getOpt (Int.fromString arg, dfltN)
		       | _ => dfltN)
	    fun doit () =
		let		
		    val x = List.tabulate(n, fn _ => Rand.inRangeInt (0, 10000))
		in 
		    ListQuicksort.quicksort x
		end
		
	in
	    RunSeq.run doit
	end

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
