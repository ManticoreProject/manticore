(* merge2.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Merge (*: sig

    val sort : int list -> int list

  end*) = struct

    val vps = VProcExtras.vprocs()
    val nVps = List.length vps

    fun spawnOn (f, id)= let
	  val id = id mod nVps
	  in
	    VProcExtras.spawnOn f (List.nth(vps, id))
	  end

    fun split (inCh, outCh1, outCh2) = let
	  fun loop (x, out1, out2) = (case x
		 of Option.NONE => (
		      PrimChan.send (outCh1, Option.NONE);
		      PrimChan.send (outCh2, Option.NONE))
		  | _ => (
		      PrimChan.send (out1, x);
		      loop (PrimChan.recv inCh, out2, out1))
		(* end case *))
	  in
	    loop (PrimChan.recv inCh, outCh1, outCh2)
	  end

    fun merge (inCh1, inCh2, outCh : int Option.option PrimChan.chan) = let
	  fun copy (fromCh, toCh) = let
		fun loop v = (
		      PrimChan.send (toCh, v);
		      if (Option.isSome v) then loop(PrimChan.recv fromCh) else ())
		in
		  loop (PrimChan.recv fromCh)
		end
	  fun merge' (from1, from2) = (
		case (from1, from2)
		 of (Option.NONE, Option.NONE) => PrimChan.send (outCh, Option.NONE)
		  | (_, Option.NONE) => (
		      PrimChan.send (outCh, from1); copy (inCh1, outCh))
		  | (Option.NONE, _) => (
		      PrimChan.send (outCh, from2); copy (inCh2, outCh))
		  | (Option.SOME a, Option.SOME b) =>
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

    fun mergeSort loc = let
	  val ch = PrimChan.new()
	  fun sort () = (
		case (PrimChan.recv ch)
		 of Option.NONE => ()
		  | v1 => (case (PrimChan.recv ch)
		       of Option.NONE => PrimChan.send(ch, v1)
			| v2 => let
			    val ch1 = mergeSort(2*loc)
			    val ch2 = mergeSort(2*loc+1)
			    in
			      PrimChan.send (ch1, v1); PrimChan.send (ch2, v2);
			      split (ch, ch1, ch2);
			      merge (ch1, ch2, ch)
			    end
		      (* end case *))
		(* end case *);
		PrimChan.send (ch, Option.NONE))
	  in
	    spawnOn (sort, loc);
	    ch
	  end

    fun sort l = let
	  val ch = mergeSort 1
	  fun collect (msg, l) = (case msg
		 of Option.NONE => List.rev l
		  | Option.SOME x => collect(PrimChan.recv ch, x::l)
		(* end case *))
	  in
	    List.app (fn x => PrimChan.send(ch, Option.SOME x)) l;
	    PrimChan.send(ch, Option.NONE);
	    collect (PrimChan.recv ch, [])
	  end

  end

structure Main =
  struct

    fun testit () = let
	  val l = List.tabulate (32, fn i => 32-i)
	  val l' = Merge.sort l
	  fun pr x = Print.print(" " ^ Int.toString x)
	  in
	    Print.print "Input list  = [";
	    List.app pr l;
	    Print.print " ]\n";
	    Print.print "Sorted list = [";
	    List.app pr l';
	    Print.print " ]\n"
	  end

    fun timeit n = let
	  val l = List.tabulate (n, fn i => n-i)
	  val t0 = Time.now()
	  val l' = Merge.sort l
	  val t = (Time.now() - t0)
	  in
	    Print.print (String.concat [
		Int.toString n, " items sorted in ", Time.toString t,
		" seconds on ", Int.toString Merge.nVps, " processors\n"
	      ])
	  end

  end

val _ = Main.testit()
val _ = Main.timeit 1024
