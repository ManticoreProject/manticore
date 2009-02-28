(* primes2.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Compute primes using the sieve of Eratosthenes.  This version runs on a
 * multiple processors.
 *)

structure Primes (*: sig

    val nthPrime : int -> int

  end*) = struct

    val vps = VProcExtras.vprocs()
    val nVps = List.length vps

    fun spawnOnNext f = let
	  val id = VProcExtras.id(VProcExtras.host()) + 1
	  val id = if (id >= nVps) then 0 else id
	  in
	    VProcExtras.spawnOn f (List.nth(vps, id))
	  end

    fun sieve () = let
	  val primes = PrimChan.new ()
	  fun counter start = let
		val ch = PrimChan.new()
		fun count i = (PrimChan.send(ch, i); count(i+1))
		in
		  spawnOnNext (fn () => (count start));
		  ch
		end
	  fun filter (p, inCh) = let
	    val outCh = PrimChan.new()
	    fun loop () = let val i = PrimChan.recv inCh
		  in
		    if ((i mod p) <> 0)
		      then PrimChan.send (outCh, i)
		      else ();
		    loop ()
		  end
	    in
	      spawn (loop());
	      outCh
	    end
	  fun head ch = let val p = PrimChan.recv ch
		in
		  PrimChan.send (primes, p);
		  head (filter (p, ch))
		end
	  in
	    spawnOnNext (fn () => (head (counter 2)));
	    primes
	  end;
    
    fun nthPrime n = let
	  val ch = sieve ()
	  fun loop i = if (i <= 0) then PrimChan.recv ch else (PrimChan.recv ch; loop(i-1))
	  in
	    loop (n-1)
	  end;

  end

structure Main = struct

    fun timeit n = let
	  val t0 = Time.now()
	  val p = Primes.nthPrime n
	  val t = (Time.now() - t0)
	  in
	    Print.print(String.concat[
		"Prime(", Int.toString n, ") = ",
		Int.toString p, "; ", Time.toString t,
		" seconds\n"
	      ])
	  end

  end

(* running with "-p 1" *)
(*val _ = Main.timeit 350*)	(* works *)
(*val _ = Main.timeit 351*)	(* works *)
(*val _ = Main.timeit 352*)		(* works *)
(*val _ = Main.timeit 353*)	(* fails *)
(*val _ = Main.timeit 356*)
(*val _ = Main.timeit 362*)
(*val _ = Main.timeit 375*)	(* fails *)
val _ = Main.timeit 2000
