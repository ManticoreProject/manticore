(* rope-tests.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure RopeTests = struct

  structure A = struct
    val cacheLineSizeBytes = 16
    val wordSizeBytes = 4
  end

  structure Ropes  = RopesFn(A)
  structure Ranges = RangesFn(A)

  (* range : int * int -> int list *)
  fun range (lo, hi) =
        let fun build (n, acc) = if (n<lo) then acc
				 else build (n-1, n::acc)
	in
	    build (hi, [])
	end
		      
  val r0 = Ropes.fromList (range (1, 15))

  (* spinal : int * int -> int rope *)
  fun spinal (lo, hi) = foldr Ropes.concat Ropes.empty (map Ropes.singleton (range (lo, hi)))

  (* ir2s : int rope -> string *)
  val ir2s = Ropes.toString Int.toString

  (* balanceTest : bool -> int -> unit *)
  fun balanceTest showTrees n =
      let val r = if n<=0 then Ropes.empty else spinal (1, n)
	  val r' = Ropes.balance r
	  val pr = if showTrees then print o ir2s else ignore
      in
	  print ("Balance Test: " ^ (Int.toString n) ^ "\n\n");
	  pr r;
	  print "(* balancing... *)\n\n";
	  pr r';
	  print "Depth: ";
	  print (Int.toString (Ropes.ropeDepth(r')));
	  print "\n";
	  print "Length: ";
	  print (Int.toString (Ropes.ropeLen(r')));
	  print "\n";
	  print "F_{n}: ";
	  print (Int.toString (Util.fib (Ropes.ropeDepth(r'))));
	  print "\n"
      end

  fun test 0 = print (ir2s r0)
    | test 1 = 
        let val rng = Ranges.mkRange (1, 20, 1)
	    val ro = Ranges.toRope rng
	in
	    (print (Ranges.ropeToString ro);
	     print "\n")
	end
    | test 2 = 
        let val rng = Ranges.mkRange (1, 150, 3)
	    val ro = Ranges.toRope rng
	in
	    (print (Ranges.ropeToString ro);
	     print "\n")
	end
    | test 3 = 
        let val rng = Ranges.mkRange (1, ~20, 1)
	    val ro = Ranges.toRope rng
	in
	    (print (Ranges.ropeToString ro);
	     print "\n")
	end
    | test _ = print "No such test.\n"

end
