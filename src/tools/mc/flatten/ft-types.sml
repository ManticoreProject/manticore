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

  datatype ty
    = IR of T.ty * R.ty

  fun same (t1 : ty, t2 : ty) : bool = let
    fun ty (IR (t1, r1), IR (t2, r2)) = 
      U.same (t1, t2) andalso R.same (r1, r2)
    in
      ty (t1, t2)
    end

  val toString : ty -> string = (fn (IR (i, r)) =>
    String.concat ["<", U.toString i, " / ", R.toString r, ">"])

  fun interfaceTy (IR (i, _)) = i

  fun reprTy (IR (_, r)) = r

end
