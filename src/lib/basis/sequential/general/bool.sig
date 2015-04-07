(* bool.sig
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

signature BOOL =
  sig

    datatype bool = true | false

    val not : bool -> bool

    val scan : (char, 'a) StringCvt.reader -> (bool, 'a) StringCvt.reader
    val fromString : string -> bool option

    val toString : bool -> string

  end
