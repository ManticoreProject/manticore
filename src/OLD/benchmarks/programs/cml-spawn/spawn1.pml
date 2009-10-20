(* spawn1.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Spawn : sig

    val repeat : int -> unit

  end = struct

    fun forkJoin f = let
	  val cv = CVar.new()
	  in
	    spawn (f(); CVar.signal cv);
	    CVar.wait cv
	  end

    fun repeat n = if (n <= 0)
	  then ()
	  else (forkJoin (fn () => ()); repeat(n-1))

  end

structure Main = struct

    fun timeit n = let
	  val t0 = Time.now()
	  val () = Spawn.repeat n
	  val t = (Time.now() - t0)
	  in
	    Print.print(String.concat[
		Int.toString n, " threads spawned in ",
		Time.toString t, " seconds\n"
	      ])
	  end

  end

val _ = Main.timeit 10000000
