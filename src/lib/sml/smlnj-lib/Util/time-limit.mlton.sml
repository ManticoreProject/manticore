(* time-limit.mlton.sml
 * 
 * Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *
 *)

structure TimeLimit:
sig
   exception TimeOut
   val timeLimit : Time.time -> ('a -> 'b) -> 'a -> 'b
end =
struct
   exception TimeOut

   fun timeLimit t f x =
      case Engine.run (Engine.new (fn () => f x), t) of
	 Engine.Done res => res
       | Engine.Raise exn => raise exn
       | Engine.TimeOut _ => raise TimeOut
end
