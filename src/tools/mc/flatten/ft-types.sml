(* ft-types.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Types for the flattening transformation.
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

(* The technical point here is that the notes/amsft paper
 * contains no account of type constructors (apart from perhaps
 * nullaries, in its non-specific reference to ground types).
 * So one needs to decide what to do with types like int option.
 *)

structure FTTypes = struct

  structure T = Types
  structure R = RepresentationTypes
  structure U = TypeUtil

  datatype ty
    = T  of T.ty
    | IR of T.ty * R.ty

  fun same (T t1, T t2) = U.same (t1, t2)
    | same (IR (i1, r1), IR (i2, r2)) = 
        U.same (i1, i2) andalso R.same (r1, r2)
    | same _ = false

  fun toString (T t) = U.toString t
    | toString (IR (i, r)) = "<" ^ U.toString i ^ " / " ^ R.toString r ^ ">"

end
