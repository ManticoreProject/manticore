functor ElaborateBOMEnv (S: ELABORATE_BOMENV_STRUCTS): ELABORATE_BOMENV = struct

  open S

  structure AstBOM = Ast.AstBOM


  (* FIXME: get the order of this delcaration right *)
  datatype t
    = T of {HLE: HLOpEnv.t, TE: TypeEnv.t, VE: ValEnv.t}

  (* structure Type = struct *)
  (*   datatype t  *)
  (*     = TypeVar of Tyvar.t *)
  (*     | MLType of Env.Type.t *)
  (*     |   *)
  (* end *)

  structure Scheme = struct
    datatype t = T of {typarams: CoreBOM.TyParam.t list, ty: CoreBOM.BOMType.t}

    fun getCoreBOM.TyParams (T {typarams=typarams', ...}) = typarams'
    fun getTy (T {ty=ty',...}) = ty'
    fun new (typarams: CoreBOM.TyParam.t list, ty: CoreBOM.BomType.t)
    fun generalizes (scheme: t, ty: CoreBOM.BomType.t): bool =
  end



end
