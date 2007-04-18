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
    val bool =		Atom.atom "bool"
    val int =		Atom.atom "int"
    val list =		Atom.atom "list"
    val string =	Atom.atom "string"
    val unit =		Atom.atom "unit"

  (* operators *)
    val eq =		Atom.atom "="
    val lte =		Atom.atom "<="
    val lt =		Atom.atom "<"
    val append =	Atom.atom "@"
    val plus =		Atom.atom "+"
    val minus =		Atom.atom "-"
    val times =		Atom.atom "*"
    val div =		Atom.atom "div"
    val mod =		Atom.atom "mod"
    val uMinus =	Atom.atom "~"

  (* pre-defined data constructors *)
    val boolTrue =	Atom.atom "true"
    val boolFalse =	Atom.atom "false"
    val listCons =	Atom.atom "::"
    val listNil =	Atom.atom "nil"

  (* predefined variables *)
    val args =		Atom.atom "args"
    val print =		Atom.atom "print"
    val fail =		Atom.atom "fail"
    val itos =		Atom.atom "itos"
    val size =		Atom.atom "size"
    val sub =		Atom.atom "sub"
    val substring =	Atom.atom "substring"
    val concat =	Atom.atom "concat"

  end
