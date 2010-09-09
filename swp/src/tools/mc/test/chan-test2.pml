(* chan-test.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Basic test of CML channels.
 *)

val vps = VProcExtras.vprocs()

val n = 10	(* number of messages *)

fun pong ch = let
      fun lp expected = let
	    val msg = PrimChan.recv ch
	    in
	      if (expected <> msg)
		then Print.print("pong received " ^ Int.toString msg ^ ", but expected " ^ Int.toString expected ^ "\n")
		else Print.print("pong received " ^ Int.toString msg ^ "\n");
	      if (msg < n)
		then lp (msg + 1)
		else ()
	    end
      in
	lp 0;
	Print.print "pong done\n"
      end

fun ping ch = let
      fun lp i = if (i <= n)
	    then (
	      Print.print("ping sends " ^ Int.toString i ^ "\n");
	      PrimChan.send (ch, i);
	      lp (i+1))
	    else ()
      in
	lp 0;
	Print.print "ping done\n"
      end

val _ = (case VProcExtras.vprocs()
       of (_::vp2::_) => let (* spawn on different vprocs *)
	  val ch = PrimChan.new()
	  in
	    VProcExtras.spawnOn (fn () => (pong ch)) vp2;
	    ping ch
	  end
	| _ => let
	  val ch = PrimChan.new()
	  in
	    spawn (pong ch);
	    ping ch
	  end
      (* end case *))