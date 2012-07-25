(* one2one.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * PML version of ping-pong benchmark for one processor.
 *)

structure OneToOne (*: sig

    val run : int -> unit

  end*) = struct

    fun run n = let
	  val ch = PrimChan.new()
	  fun ping i = if (i < n)
		then let
		  val _ = PrimChan.send(ch, i)
		  val j = i+1
		  in
		    ping (j)
		  end
		else ()
	  fun pong i = if (i < n)
                then let
		  val msg = PrimChan.recv ch
		  val j = i+1
		  in
		    pong (j)
		  end
                else ()
	  in
	    spawn (pong(0));
	    ping 0
	  end

  end

structure Main = struct

    fun timeit n = let
	  val t0 = Time.now()
	  val () = OneToOne.run n
	  val t = (Time.now() - t0)
	  in
	    Print.print (String.concat[
		Int.toString n, " messages received\n"
	      ])
	  end

  end

val _ = Main.timeit 5000000
