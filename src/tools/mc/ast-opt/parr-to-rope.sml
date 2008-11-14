(* parr-to-rope.sml
 *
 * COPYRIGHT (c) 2008 Manticore Group
 * All rights reserved.
 *
 * Translation of parallel arrays to ropes.
 *)

structure ParrToRope = struct

(* We want: PArrayExp (es, ty) ==> ApplyExp (Ropes.fromList, es, ty rope) *)

  fun xform (es, ty) = ApplyExp (Ropes.fromList, ASTUtil.mkList es, ty rope)

end
