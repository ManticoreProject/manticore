(* merge.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Merge : sig

    val sort : int list -> int list

  end = struct

    fun split (inCh, outCh1, outCh2) = let
	  fun loop (NONE, _, _) = (
		CML.send (outCh1, NONE); CML.send (outCh2, NONE))
	    | loop (x, out1, out2) = (
		CML.send (out1, x);
		loop (CML.recv inCh, out2, out1))
	  in
	    loop (CML.recv inCh, outCh1, outCh2)
	  end

    fun merge (inCh1, inCh2, outCh : int option CML.chan) = let
	  fun copy (fromCh, toCh) = let
		fun loop v = (
		      CML.send (toCh, v);
		      if (isSome v) then loop(CML.recv fromCh) else ())
		in
		  loop (CML.recv fromCh)
		end
	  fun merge' (from1, from2) = (
		case (from1, from2)
		 of (NONE, NONE) => CML.send (outCh, NONE)
		  | (_, NONE) => (
		      CML.send (outCh, from1); copy (inCh1, outCh))
		  | (NONE, _) => (
		      CML.send (outCh, from2); copy (inCh2, outCh))
		  | (SOME a, SOME b) =>
		      if (a < b)
			then (
			  CML.send (outCh, from1);
			  merge' (CML.recv inCh1, from2))
			else (
			  CML.send (outCh, from2);
			  merge' (from1, CML.recv inCh2))
		(* end case *))
	  in
	    merge' (CML.recv inCh1, CML.recv inCh2)
	  end

    fun mergeSort () = let
	  val ch = CML.channel()
	  fun sort () = (
		case (CML.recv ch)
		 of NONE => ()
		  | v1 => (case (CML.recv ch)
		       of NONE => CML.send(ch, v1)
			| v2 => let
			    val ch1 = mergeSort()
			    val ch2 = mergeSort()
			    in
			      CML.send (ch1, v1); CML.send (ch2, v2);
			      split (ch, ch1, ch2);
			      merge (ch1, ch2, ch)
			    end
		      (* end case *))
		(* end case *);
		CML.send (ch, NONE))
	  in
	    CML.spawn sort;
	    ch
	  end

    fun sort l = let
	  val ch = mergeSort()
	  fun collect (NONE, l) = rev l
	    | collect (SOME x, l) = collect(CML.recv ch, x::l)
	  in
	    app (fn x => CML.send(ch, SOME x)) l;
	    CML.send(ch, NONE);
	    collect (CML.recv ch, [])
	  end

  end

structure Main =
  struct

    fun testit () = let
	  val l = List.tabulate (32, fn i => 32-i)
	  fun sort () = let
		val l' = Merge.sort l
		fun pr x = TextIO.print(" " ^ Int.toString x)
		in
		  TextIO.print "Input list  = [";
		  List.app pr l;
		  TextIO.print " ]\n";
		  TextIO.print "Sorted list = [";
		  List.app pr l';
		  TextIO.print " ]\n"
		end
	  in
	    RunCML.doit (sort, NONE)
	  end

    fun timeit n = let
	  val l = List.tabulate (n, fn i => n-i)
	  fun sort () = let
		val t0 = Time.now()
		val l' = Merge.sort l
		val t = Time.-(Time.now(), t0)
		in
		  TextIO.print (String.concat [
		      Int.toString n, " items sorted in ", Time.toString t,
		      " seconds\n"
		    ])
		end
	  in
	    RunCML.doit (sort, NONE)
	  end

    fun main _ = (timeit 32768; OS.Process.success)

  end
