(* ring-cml.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Carl Ritson's ring benchmark using CML's synchronous channels.
 *)

functor RingFn (
    val name : string
    type 'a chan
    val new : unit -> 'a chan
    val send : 'a chan * 'a -> unit
    val recv : 'a chan -> 'a
  ) = struct

    local
      val numElements = 256
  
      fun root ncycles (ch, nextCh) = let
	    fun lp (0, tok) = (send(nextCh, 0); ignore(recv ch))
	      | lp (n, tok) = (send(nextCh, tok); lp(n-1, recv ch + 1))
	    in
	      CML.spawn (fn () => lp(ncycles, 1))
	    end
  
      fun element (ch, nextCh) = let
	    fun lp 0 = send(nextCh, 0)
	      | lp tok = (send(nextCh, tok); lp(recv ch))
	    in
	      CML.spawn (fn () => lp(recv ch))
	    end
  
      fun doit ncycles = let
	    val rootCh = new()
	    fun mkElem (0, nextCh) = root ncycles (rootCh, nextCh)
	      | mkElem (i, nextCh) = let
		  val ch = new()
		  in
		    element (ch, nextCh);
		    mkElem (i-1, ch)
		  end
	    in
	      CML.sync (CML.joinEvt (mkElem (numElements-1, rootCh)))
	    end

    in

    fun main (_, [ncycles]) = (case Int.fromString ncycles
	   of SOME n => (doit n; OS.Process.success)
	    | NONE => OS.Process.failure
	  (* end case *))
      | main _ = OS.Process.failure

    fun run ncycles = RunCML.doit(fn () => doit ncycles, NONE)
    fun export () = RunCML.exportFn (name, main, NONE)
    end

  end (* local *)

structure RingSync = RingFn(
    val name = "ring-cml"
    type 'a chan = 'a CML.chan
    val new = CML.channel
    val send = CML.send
    val recv = CML.recv)

structure RingAsync = RingFn(
    val name = "ring-cml-async"
    type 'a chan = 'a Mailbox.mbox
    val new = Mailbox.mailbox
    val send = Mailbox.send
    val recv = Mailbox.recv)
