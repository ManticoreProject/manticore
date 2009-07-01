(* pervasives.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

(* FIXME: add support for rebinding datatypes
datatype 'a option = datatype 'a Option.option
*)
type 'a option = 'a Option.option

val print = Print.print

val rev = List.rev
val app = List.app
val map = List.map
val foldl = List.foldl
val foldr = List.foldr
val nth = List.nth
