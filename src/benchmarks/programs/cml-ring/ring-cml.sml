(* ring-cml.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Carl Ritson's ring benchmark using CML's synchronous channels.
 *)

structure Ring : sig

    val numElements : int

    val doit : int -> unit

  end = struct

    val numElements = 256

    fun root ncycles (ch, nextCh) = let
	  fun lp (0, tok) = (CML.send(nextCh, 0); ignore(CML.recv ch))
	    | lp (n, tok) = (CML.send(nextCh, tok); lp(n-1, CML.recv ch + 1))
	  in
	    CML.spawn (fn () => lp(ncycles, 1))
	  end

    fun element (ch, nextCh) = let
	  fun lp 0 = CML.send(nextCh, 0)
	    | lp tok = (CML.send(nextCh, tok); lp(CML.recv ch))
	  in
	    CML.spawn (fn () => lp(CML.recv ch))
	  end

    fun doit ncycles = let
	  val rootCh = CML.channel()
	  fun mkElem (0, nextCh) = root ncycles (rootCh, nextCh)
	    | mkElem (i, nextCh) = let
		val ch = CML.channel()
		in
		  element (ch, nextCh);
		  mkElem (i-1, ch)
		end
	  in
	    CML.sync (CML.joinEvt (mkElem (numElements-1, rootCh)))
	  end

  end

structure Main =
  struct

    fun timeit ncycles = let
	  fun thd () = let
		val t0 = Time.now()
		val _ = Ring.doit ncycles
		val t = Time.-(Time.now(), t0)
		in
		  TextIO.print(concat[
		      Int.toString ncycles, "*", Int.toString Ring.numElements,
		      " iterations in ", Time.toString t,
		      " seconds\n"
		    ])
		end
	  in
	    RunCML.doit(thd, NONE)
	  end

    fun main _ = (timeit 50000; OS.Process.success)

  end (* local *)
