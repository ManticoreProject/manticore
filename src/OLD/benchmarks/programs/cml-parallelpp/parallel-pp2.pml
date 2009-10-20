(* parallel-pp2.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * PML version of ping-pong benchmark for two processors.
 *)

structure ParallelPingPong (*: sig

    val run : int -> unit

  end*) = struct

    val nprocs = 8 (* number of communicating pairs *)

    val vps = VProcExtras.vprocs()
    val nVps = List.length vps

    fun spawnOn (f, id) = VProcExtras.spawnOn f (List.nth(vps, id))

    fun run n = let
	  fun pingpong id = let
		val pingVP = id mod nVps
		val pongVP = (id+1) mod nVps
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
		  spawnOn (pong, pongVP);
		  spawnOn (fn () => ping 0, pingVP);
		  cv
		end
	  val cvs = List.tabulate (nprocs, pingpong)
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
		Int.toString(n*ParallelPingPong.nprocs), " messages in ", Time.toString t,
		" seconds on ", Int.toString ParallelPingPong.nVps, " processors\n"
	      ])
	  end

  end

val _ = Main.timeit 100000

