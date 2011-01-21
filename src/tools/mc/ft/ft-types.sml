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

  structure ITy = InterfaceTypes
  structure RTy = RepresentationTypes

  datatype ty
    = I  of ITy.ty
    | IR of ITy.ty * RTy.ty

  fun same (I t1, I t2) = ITy.same (t1, t2)
    | same (IR (t1, r1), IR (t2, r2)) = 
        ITy.same (t1, t2) andalso RTy.same (r1, r2)
    | same _ = false

  fun toString (I t) = ITy.toString t
    | toString (IR (t, r)) = "<" ^ ITy.toString t ^ " / " ^ RTy.toString r ^ ">"

end
