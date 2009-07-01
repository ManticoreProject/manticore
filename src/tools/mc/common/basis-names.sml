(* basis-names.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *
 * Names of identifiers and operators bound in the MinML basis.
 *)

structure BasisNames =
  struct

    nonfix mod div

  (* predefined type names *)
    val unit =		Atom.atom "unit"
    val bool =		Atom.atom "bool"
    val exn =		Atom.atom "exn"
    val int =		Atom.atom "int"
    val long =		Atom.atom "long"
    val integer =	Atom.atom "integer"
    val float =		Atom.atom "float"
    val double =	Atom.atom "double"
    val char =		Atom.atom "char"
    val rune =		Atom.atom "rune"
    val string =	Atom.atom "string"
    val list =		Atom.atom "list"
    val option =	Atom.atom "option"
    val parray =        Atom.atom "parray"
    val trap =          Atom.atom "trap"
    val chan =		Atom.atom "chan"
    val ivar =		Atom.atom "ivar"
    val mvar =		Atom.atom "mvar"
    val event =		Atom.atom "event"
    val thread_id =     Atom.atom "tid"

  (* operators *)
    val eq =		Atom.atom "="
    val neq =		Atom.atom "<>"
    val gte =		Atom.atom ">="
    val gt =		Atom.atom ">"
    val lte =		Atom.atom "<="
    val lt =		Atom.atom "<"
    val append =	Atom.atom "@"
    val concat =	Atom.atom "^"
    val psub =          Atom.atom "!"
    val plus =		Atom.atom "+"
    val minus =		Atom.atom "-"
    val times =		Atom.atom "*"
    val div =		Atom.atom "div"
    val mod =		Atom.atom "mod"
    val fdiv =		Atom.atom "/"
    val uMinus =	Atom.atom "~"

  (* pre-defined data constructors *)
    val boolTrue =	Atom.atom "true"
    val boolFalse =	Atom.atom "false"
    val listCons =	Atom.atom "::"
    val listCons' =	Atom.atom "CONS"
    val listNil =	Atom.atom "nil"
    val listNil' =	Atom.atom "NIL"
    val optionNONE =	Atom.atom "NONE"
    val optionSOME =	Atom.atom "SOME"
    val trapVal =       Atom.atom "Val"
    val trapExn =       Atom.atom "Exn"

  (* pre-defined exception constructors *)
    val exnBind =	Atom.atom "Bind"
    val exnDiv =	Atom.atom "Div"
    val exnFail =	Atom.atom "Fail"
    val exnMatch =	Atom.atom "Match"

  end
