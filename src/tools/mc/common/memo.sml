(* memo.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Memoization cells. I (ams) needed these working with the (dynamically-
 * loaded) basis in AST rewrites, and thought others might find them useful.
 *)

structure Memo :> sig

  type 'a memo

  val new : (unit -> 'a) -> 'a memo
  val get : 'a memo -> 'a

  val computedYet : 'a memo -> bool

end = struct

  datatype 'a memo_info
    = ToDo of unit -> 'a
    | Did  of 'a

  type 'a memo = 'a memo_info ref

(* new : (unit -> 'a) -> 'a memo *)
  fun new susp = ref (ToDo susp)

(* get : 'a memo -> 'a *)
(* side-effect: the item is computed if it hasn't yet been *)
  fun get (m : 'a memo) : 'a =
   (case !m
      of ToDo susp => let
           val x = susp ()
           val _ = (m := Did x)
           in
             x
           end
       | Did x => x)

(* isComputed : 'a memo -> bool *)
  fun computedYet (m : 'a memo) : bool =
   (case !m
      of ToDo _ => false
       | Did _  => true)

end
