(* primes1.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Compute primes using the sieve of Eratosthenes.  This version runs on a
 * single processor.
 *)

structure Primes (*: sig

    val nthPrime : int -> int

  end*) = struct

    fun sieve () = let
	  val primes = PrimChan.new ()
	  fun counter start = let
		val ch = PrimChan.new()
		fun count i = (PrimChan.send(ch, i); count(i+1))
		in
		  spawn (count start);
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
	    spawn (head (counter 2));
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

    val dfltN = 5000

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

    fun main (_, args) = let
	  val n = (case args
		 of arg::_ => Option.getOpt (Int.fromString arg, dfltN)
		  | _ => dfltN
		(* end case *))
	  in
	    timeit n
	  end

  end

(* running with "-p 1" *)
val _ = Main.main (CommandLine.name(), CommandLine.arguments())
