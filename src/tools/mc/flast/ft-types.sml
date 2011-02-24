(* ft-types.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Types for the flattening transformation.
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure FTTypes = struct

  structure T = Types    (* AST types *)
  structure U = TypeUtil (* AST type utils *)
  structure R = FTReprTypes
  structure N = NestingTreeTypes

  datatype ty_scheme = datatype R.ty_scheme

  datatype ty
    = IR of T.ty * R.ty

end
