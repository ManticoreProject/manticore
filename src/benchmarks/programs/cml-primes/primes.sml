(* prims.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Compute primes using the sieve of Eratosthenes.
 *)

structure Primes : sig

    val nthPrime : int -> int

  end = struct

    fun sieve () = let
	  val primes = CML.channel ()
	  fun counter start = let
		val ch = CML.channel()
		fun count i = (CML.send(ch, i); count(i+1))
		in
		  CML.spawn (fn () => count start);
		  ch
		end
	  fun filter (p, inCh) = let
	    val outCh = CML.channel()
	    fun loop () = let val i = CML.recv inCh
		  in
		    if ((i mod p) <> 0)
		      then CML.send (outCh, i)
		      else ();
		    loop ()
		  end
	    in
	      CML.spawn loop;
	      outCh
	    end
	  fun head ch = let val p = CML.recv ch
		in
		  CML.send (primes, p);
		  head (filter (p, ch))
		end
	  in
	    CML.spawn (fn () => head (counter 2));
	    primes
	  end;
    
    fun nthPrime n = let
	  val ch = sieve ()
	  fun loop 0 = CML.recv ch
	    | loop i = (CML.recv ch; loop(i-1))
	  in
	    loop (n-1)
	  end;

  end

structure Main = struct

    val dfltN = 5000

    fun testit n = let
	  fun thd () = let
		val p = Primes.nthPrime n
		in
		  TextIO.print(concat[
		      "Prime(", Int.toString n, ") = ",
		      Int.toString p, "\n"
		    ])
		end
	  in
	    RunCML.doit (thd, NONE)
	  end

    fun timeit n = let
	  fun thd () = let
		val t0 = Time.now()
		val p = Primes.nthPrime n
		val t = Time.-(Time.now(), t0)
		in
		  TextIO.print(concat[
		      "Prime(", Int.toString n, ") = ",
		      Int.toString p, "; ", Time.toString t,
		      " seconds\n"
		    ])
		end
	  in
	    RunCML.doit (thd, NONE)
	  end

    fun main (_, args) = let
	  val n = (case args
		 of arg::_ => Option.getOpt (Int.fromString arg, dfltN)
		  | _ => dfltN
		(* end case *))
	  in
	    timeit n;
	    OS.Process.success
	  end

  end
