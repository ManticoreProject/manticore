(* spawn.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Spawn : sig

    val repeat : int -> unit

  end = struct

    fun forkJoin f = CML.sync (CML.joinEvt (CML.spawn f))

    fun repeat n = if (n <= 0) then () else (forkJoin (fn () => ()); repeat(n-1))

  end

structure Main = struct

    fun timeit n = let
	  fun thd () = let
		val t0 = Time.now()
		val () = Spawn.repeat n
		val t = Time.-(Time.now(), t0)
		in
		  TextIO.print(concat[
		      Int.toString n, " threads spawned in ",
		      Time.toString t, " seconds\n"
		    ])
		end
	  in
	    RunCML.doit (thd, NONE)
	  end

    fun main _ = (timeit 10000000; OS.Process.success)

  end
