(* parallel-pp1.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * PML version of ping-pong benchmark for one processor.
 *)

structure ParallelPingPong (*: sig

    val run : int -> unit

  end*) = struct

    val nprocs = 8

    fun run n = let
	  fun pingpong () = let
		val ch = PrimChan.new()
		val cv = CVar.new()
		fun ping i = if (i < n)
		      then let
			val _ = PrimChan.send(ch, i)
			val ack = PrimChan.recv ch
			in
			  ping ack
			end
		      else ()
		fun pong () = let
		      val msg = PrimChan.recv ch + 1
		      in
			PrimChan.send (ch, msg);
			if (msg < n) then pong() else CVar.signal cv
		      end
		in
		  spawn pong();
		  spawn ping 0;
		  cv
		end
	  val cvs = List.tabulate (nprocs, fn _ => pingpong())
	  in
	    List.app CVar.wait cvs
	  end

  end

structure Main = struct

    fun timeit n = let
	  val t0 = Time.now()
	  val () = ParallelPingPong.run n
	  val t = (Time.now() - t0)
	  in
	    Print.print (String.concat[
		Int.toString(n*ParallelPingPong.nprocs), " messages in ",
		Time.toString t, " seconds\n"
	      ])
	  end

  end

val _ = Main.timeit 100000

