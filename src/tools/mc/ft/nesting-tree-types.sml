(* nesting-tree-types.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Types for the flattening transformation.
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)
 
structure NestingTreeTypes = struct

  datatype ty
    = Lf
    | Nd of ty

  fun same (Lf, Lf) = true
    | same (Nd t, Nd u) = same (t, u)
    | same _ = false

  fun toString Lf = "Lf"
    | toString (Nd t) = "Nd(" ^ toString t ^ ")"

end
