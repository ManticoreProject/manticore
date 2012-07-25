(* memo.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Memoization cells. I (ams) needed these working with the (dynamically-
 * loaded) basis in AST rewrites, and thought others might find them useful.
 *)

structure Memo : sig

  type 'a thunk = unit -> 'a
  val new : 'a thunk -> 'a thunk

end = struct

  type 'a thunk = unit -> 'a

  fun new (f : 'a thunk) : 'a thunk = let
    val cell = ref NONE
    val remember = fn x => (cell := SOME x; x)
    fun get () = (case !cell
      of SOME x => x
       | NONE => remember (f ())
      (* end case *))
    in
      get
    end

end
