(* translate-types.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure TranslateTypes =
  struct

    structure Ty = Types;
    structure BTy = BOMTy

    fun tr ty = (case TypeUtil.prune ty
	   of Ty.ErrorTy => raise Fail "unexpected ErrorTy"
	    | Ty.MetaTy _ => BTy.T_Any (* can this happen? *)
	    | Ty.ClassTy _ => raise Fail "unresolved overload"
	    | Ty.VarTy _ => BTy.T_Any
	    | Ty.ConTy(tyArgs, tyc) => raise Fail "ConTy unimplemented"
	    | Ty.FunTy(ty1, ty2) => BTy.T_Fun([tr ty1], [BTy.exhTy], [tr ty2])
	    | Ty.TupleTy [] => BTy.unitTy
	    | Ty.TupleTy tys => BTy.T_Tuple(false, List.map tr tys)
	  (* end case *))

    fun trScheme (Ty.TyScheme(_, ty)) = tr ty

  end
