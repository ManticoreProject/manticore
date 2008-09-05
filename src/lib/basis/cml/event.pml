(* event.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * An implementation of the CML event combinators on top of primitive events.
 * See the DAMP'08 paper for details.
 *)

structure Event : sig

    type 'a event

    val always : 'a -> 'a event
    val never : 'a event
    val guard : (unit -> 'a event) -> 'a event
    val withNack : (unit event -> 'a event) -> 'a event
    val choose : 'a event * 'a event -> 'a event
    val wrap : ('a event * ('a -> 'b)) -> 'b event
    val sync : 'a event -> 'a

  end = struct

    type svar = PrimEvent.signal_var
    type 'a thunk = unit -> 'a

  (* NOTE: we could specialize this representation to differentiate between
   * guarded and non-guarded events.
   *)
    datatype 'a event
      = E of (svar list * (svar list * 'a thunk) PrimEvent.pevent) thunk

    fun baseEvt ev = E(fn () => ([], PrimEvent.wrap(ev, fn x => ([], fn () => x))))

    fun always x = baseEvt(PrimEvent.always x)

    val never = E(fn () => ([], PrimEvent.never))

    fun recvEvt ch = baseEvt(PrimEvent.recvEvt ch)
    fun sendEvt (ch, msg) = baseEvt(PrimEvent.sendEvt(ch, msg))

    fun wrap (E thunk, f) = let
	  fun thunk' () = let
		val (cvs, ev) = thunk()
		in
		  (cvs, PrimEvent.wrap(ev, fn (cvs, g) => (cvs, f o g)))
		end
	  in
	    E thunk'
	  end

    fun guard f = let
	  fun thunk' () = let val E thunk = f() in thunk() end
	  in
	    E thunk'
	  end

    fun withNack f = let
	  fun thunk () = let
		val nack = PrimEvent.new()
		val E thunk' = f (baseEvt (PrimEvent.waitEvt nack))
		val (cvs, ev) = thunk' ()
		in
		  (nack::cvs, ev)
		end
	  in
	    E thunk
	  end

    fun choose (E thunk1, E thunk2) = let
	  fun thunk () = let
		val (cvs1, ev1) = thunk1()
		val (cvs2, ev2) = thunk2()
		in (
		  cvs1 @ cvs2,
		  PrimEvent.choose (
		    PrimEvent.wrap(ev1, fn (cvs, th) => (cvs @ cvs2, th)),
		    PrimEvent.wrap(ev2, fn (cvs, th) => (cvs @ cvs1, th)))
		) end
	  in
	    E thunk
	  end

    fun sync (E thunk) = let
	  val (_, ev) = thunk()
	  val (cvs, act) = PrimEvent.sync ev
	  in
	    List.app PrimEvent.set cvs;
	    act()
	  end

  end

