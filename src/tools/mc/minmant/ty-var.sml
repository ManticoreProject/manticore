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

  (* return true if two type variables are the same (i.e., have the same stamp) *)
    val same : AST.tyvar * AST.tyvar -> bool

  (* finite maps on type variables *)
    structure Map : ORD_MAP where type Key.ord_key = AST.tyvar

  end = struct

    datatype tyvar = datatype AST.tyvar

  (* create a new type variable *)
    fun new name = TVar{stamp = Stamp.new(), name = name}

  (* return true if two type variables are the same (i.e., have the same stamp) *)
    fun same (TVar{stamp=a, ...}, TVar{stamp=b, ...}) = Stamp.same(a, b)

    structure Map = RedBlackMapFn (
      struct
	type ord_key = tyvar
	fun compare (TVar{stamp = a, ...}, TVar{stamp = b, ...}) = Stamp.compare(a, b)
      end)

  end
