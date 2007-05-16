(* type-class.sml
 *
 * Utility functions for dealing with type classes.
 *)

structure TypeClass =
struct

structure B = Basis
structure Ty = Types

val IntClass = [B.intTy, B.longTy, B.integerTy]
val FloatClass = [B.floatTy, B.doubleTy]
val NumClass = IntClass @ FloatClass
val OrderClass = NumClass @ [B.charTy, B.runeTy, B.stringTy]
val BasicClass = B.unitTy :: OrderClass

fun new cl = Ty.ClassTy (Ty.Class (ref (Ty.CLASS cl)))
		 
fun isClass (Ty.ConTy (_, tyc), c) =
    List.exists (fn (Ty.ConTy (_, tyc')) => TyCon.same (tyc, tyc')) c
  | isClass _ = false
		
fun isEqualityType t =
    let
	fun arg (Ty.DCon {argTy, ...}) = argTy
    in
	isClass (t, BasicClass) orelse
	(case t of
	     Ty.TupleTy tys => List.all isEqualityType tys
	   | Ty.ConTy (_, Ty.DataTyc {cons, ...}) =>
	     List.all isEqualityType (List.mapPartial arg (!cons))
	   | _ => false
	(* end case *))
    end

end
