(* chan-test.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Basic test of CML channels.
 *)

val n = 10	(* number of messages *)

fun pong ch = let
      fun lp () = let
	    val msg = PrimEvent.sync(PrimEvent.wrap(PrimChan.recvEvt ch, Int.toString))
	    in
	      Print.print("pong received " ^ msg ^ "\n");
	      lp ()
	    end
      in
	lp ()
      end

fun ping ch = let
      fun lp i = if (i <= n)
	    then (
	      Print.print("ping sends " ^ Int.toString i ^ "\n");
	      PrimEvent.sync(PrimChan.sendEvt (ch, i));
	      lp (i+1))
	    else ()
      in
	lp 0;
	Print.print "ping done\n"
      end

val _ = let
	val ch = PrimChan.new()
	in
	  spawn (pong ch);
	  ping ch
	end
