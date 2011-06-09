(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature ENGINE =
   sig
      type 'a t

      datatype 'a res =
	 Done of 'a
       | Raise of exn
       | TimeOut of 'a t

      val new: (unit -> 'a) -> 'a t
      val repeat: {thunk: unit -> 'a,
		   limit: Time.time,
		   tries: int} -> 'a option
      val run: 'a t * Time.time -> 'a res
      val timeLimit: Time.time * (unit -> 'a) -> 'a option
   end

structure Engine: ENGINE =
struct

open MLton

datatype 'a t = T of {return: 'a res Thread.t option ref,
		      thread: Thread.Runnable.t}
and 'a res =
   Done of 'a
 | Raise of exn
 | TimeOut of 'a t

val which = Itimer.Real
val signal = Itimer.signal which

fun done (return): unit =
   (return := NONE
    ; Itimer.set (which, {value = Time.zeroTime,
			  interval = Time.zeroTime})
    ; Signal.setHandler (signal, Signal.Handler.default))

fun new (f: unit -> 'a): 'a t =
   let
      val return = ref NONE
      val thread =
	 Thread.new
	 (fn () =>
	  let
	     val res = Done (f ()) handle e => Raise e
	     val ret = valOf (!return)
	     val _ = done return
	  in
	     Thread.switch (fn _ => Thread.prepare (ret, res))
	  end)
      val thread = Thread.prepare (thread, ())
   in
      T {return = return, thread = thread}
   end
     
fun run (T {return, thread}, time: Time.time): 'a res =
   Thread.switch
   (fn cur: 'a res Thread.t =>
    let
       val _ = return := SOME cur
       fun handler (me: Thread.Runnable.t): Thread.Runnable.t =
	  Thread.prepare
	  (Thread.prepend (cur, fn () => (done return
					  ; TimeOut (T {return = return,
							thread = me}))),
	   ())
       val _ = Signal.setHandler (signal, Signal.Handler.handler handler)
       val _ = Itimer.set (which, {value = time,
				   interval = Time.zeroTime})
    in
       thread
    end)

fun timeLimit (t: Time.time, f: unit -> 'a): 'a option =
   case run (new f, t) of
      Done a => SOME a
    | Raise e => raise e
    | TimeOut _ => NONE

fun repeat {thunk, limit, tries} =
   let
      fun loop (n: int) =
	 if n <= 0
	    then NONE
	 else (case timeLimit (limit, thunk) of
		  NONE => loop (n - 1)
		| SOME a => SOME a)
   in loop tries
   end

end
