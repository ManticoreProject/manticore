(* merge2.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Merge (*: sig

    val sort : int list -> int list

  end*) = struct

    fun split (inCh, outCh1, outCh2) = let
	  fun loop (NONE, _, _) = (
		PrimChan.send (outCh1, NONE); PrimChan.send (outCh2, NONE))
	    | loop (x, out1, out2) = (
		PrimChan.send (out1, x);
		loop (PrimChan.recv inCh, out2, out1))
	  in
	    loop (PrimChan.recv inCh, outCh1, outCh2)
	  end

    fun merge (inCh1, inCh2, outCh : int option PrimChan.chan) = let
	  fun copy (fromCh, toCh) = let
		fun loop v = (
		      PrimChan.send (toCh, v);
		      if (isSome v) then loop(PrimChan.recv fromCh) else ())
		in
		  loop (PrimChan.recv fromCh)
		end
	  fun merge' (from1, from2) = (
		case (from1, from2)
		 of (NONE, NONE) => PrimChan.send (outCh, NONE)
		  | (_, NONE) => (
		      PrimChan.send (outCh, from1); copy (inCh1, outCh))
		  | (NONE, _) => (
		      PrimChan.send (outCh, from2); copy (inCh2, outCh))
		  | (SOME a, SOME b) =>
		      if (a < b)
			then (
			  PrimChan.send (outCh, from1);
			  merge' (PrimChan.recv inCh1, from2))
			else (
			  PrimChan.send (outCh, from2);
			  merge' (from1, PrimChan.recv inCh2))
		(* end case *))
	  in
	    merge' (PrimChan.recv inCh1, PrimChan.recv inCh2)
	  end

    fun mergeSort () = let
	  val ch = PrimChan.new()
	  fun sort () = (
		case (PrimChan.recv ch)
		 of NONE => ()
		  | v1 => (case (PrimChan.recv ch)
		       of NONE => PrimChan.send(ch, v1)
			| v2 => let
			    val ch1 = mergeSort()
			    val ch2 = mergeSort()
			    in
			      PrimChan.send (ch1, v1); PrimChan.send (ch2, v2);
			      split (ch, ch1, ch2);
			      merge (ch1, ch2, ch)
			    end
		      (* end case *))
		(* end case *);
		PrimChan.send (ch, NONE))
	  in
	    spawn sort();
	    ch
	  end

    fun sort l = let
	  val ch = mergeSort()
	  fun collect (NONE, l) = rev l
	    | collect (SOME x, l) = collect(PrimChan.recv ch, x::l)
	  in
	    app (fn x => PrimChan.send(ch, SOME x)) l;
	    PrimChan.send(ch, NONE);
	    collect (PrimChan.recv ch, [])
	  end

  end

structure Main =
  struct

    fun timeit n = let
	  val l = List.tabulate (n, fn i => n-i)
	  val t0 = Time.now()
	  val l' = Merge.sort l
	  val t = (Time.now() - t0)
	  in
	    TextIO.print (String.concat [
		Int.toString n, " items sorted in ", Time.toString t,
		" seconds\n"
	      ])
	  end

  end

val _ = Main.timeit 1024
