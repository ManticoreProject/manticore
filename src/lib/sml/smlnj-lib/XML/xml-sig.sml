(* xml-sig.sml
 *
 * COPYRIGHT (c) 2010 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature XML =
  sig

    type document
    type element
    type comment
    type text

  (* attributes *)

    datatype 'a attr_value
      = NO_ATTR			(* attribute not defined *)
      | WRONG_TYPE		(* attribute defined, but at a different type *)
      | VALUE of 'a		(* attribute value *)
    val getAttr : element * string -> string option
    val getIntAttr : element * string -> int attr_value
    val getRealAttr : element * string -> real attr_value

  (* element editing *)
    val addAttr : element * string * string -> unit
    val removeAttr : element * string -> unit

  end
