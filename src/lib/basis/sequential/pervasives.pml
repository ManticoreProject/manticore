(* pervasives.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

datatype option = datatype Option.option

val print = Print.print

val rev = List.rev
val app = List.app
val map = List.map
val foldl = List.foldl
val foldr = List.foldr
val nth = List.nth

fun fst (x, _) = x
fun snd (_, y) = y

val isSome = Option.isSome

(* input argument provided by passing -i <inputArgument> to the program. the value
 * defaults to 0 if no input was provided. *)
local
  _primcode (
      extern int M_GetInputArgument ();
  define @get-input-argument (_ : unit / _ : exh) : ml_int =
      let arg : int = ccall M_GetInputArgument ()
      return (alloc(arg))
    ;
  )
  val getInputArgument : unit -> int = _prim (@get-input-argument)
in
val inputArgument : int = getInputArgument ()
end (* local *)
