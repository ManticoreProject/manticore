(* memo.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Memoization cells. I (ams) needed these working with the basis,
 * and thought others might find them useful.
 *)

structure Memo :> sig

  type 'a memo

  val new : unit -> 'a memo
  val get : (unit -> 'a) -> 'a memo -> 'a

  val computedYet : 'a memo -> bool

end = struct

  type 'a memo = 'a option ref

(* new : unit -> 'a memo 
 * This is intended to be typed at the call site, as in
 *   val n : int memo = Memo.new ()
 *)
  fun new () = ref NONE

(* get : (unit -> 'a) -> 'a memo -> 'a *)
(* side-effect: the item is computed if it hasn't yet been *)
  fun get (compute : unit -> 'a) (m : 'a memo) : 'a =
   (case !m
      of NONE => let
           val x = compute ()
           val _ = (m := SOME x)
           in
             x
           end
       | SOME x => x)

(* isComputed : 'a memo -> bool *)
  fun computedYet (m : 'a memo) : bool = not (isSome (!m))

end
