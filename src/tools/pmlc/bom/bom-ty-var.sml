(* bom-ty-var.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOMTyVar : sig

    type t

    val same : t * t -> bool

    val toString : t -> string

  end = struct

    datatype t = datatype BOMRep.ty_var

    fun same (TV a, TV b) = Stamp.same(a, b)

    fun toString (TV id) = "'tv" ^ Stamp.toString id

  end

