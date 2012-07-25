(* ty-var.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure TyVar : sig

  (* create a new type variable *)
    val new : Atom.atom -> AST.tyvar

  (* create a new type variable with a specified type class *)
    val newClass : Atom.atom * Types.ty_class -> AST.tyvar

  (* return true if two type variables are the same (i.e., have the same stamp) *)
    val same : AST.tyvar * AST.tyvar -> bool

  (* compare two type variables *)
    val compare : AST.tyvar * AST.tyvar -> order

  (* sets of type variables *)
    structure Set : ORD_SET where type Key.ord_key = AST.tyvar

  (* finite maps on type variables *)
    structure Map : ORD_MAP where type Key.ord_key = AST.tyvar

  end = struct

    datatype tyvar = datatype AST.tyvar

  (* create a new type variable *)
    fun new name = TVar{stamp = Stamp.new(), name = name, class = NONE}

  (* create a new type variable with a specified type class *)
    fun newClass (name, class) = TVar{stamp = Stamp.new(), name = name, class = SOME class}

  (* return true if two type variables are the same (i.e., have the same stamp) *)
    fun same (TVar{stamp=a, ...}, TVar{stamp=b, ...}) = Stamp.same(a, b)
(*    fun same (TVar{name=a, ...}, TVar{name=b, ...}) = Atom.same(a, b)*)

    fun compare (TVar{stamp = a, ...}, TVar{stamp = b, ...}) = Stamp.compare(a, b)
(*    fun compare (TVar{name = a, ...}, TVar{name = b, ...}) = Atom.compare(a, b)*)

    structure Set = RedBlackSetFn (
      struct
	type ord_key = tyvar
	val compare = compare
      end)

    structure Map = RedBlackMapFn (
      struct
	type ord_key = tyvar
	val compare = compare 
      end)

  end
