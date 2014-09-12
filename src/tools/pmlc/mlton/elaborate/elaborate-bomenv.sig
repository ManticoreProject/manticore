signature ELABORATE_BOMENV_STRUCTS =
  sig
    structure Ast: AST
    structure CoreBOM: CORE_BOM
    sharing CoreBOM.AstBOM = Ast.AstBOM
  end

signature ELABORATE_BOMENV =
  sig
    include ELABORATE_BOMENV_STRUCTS

    structure AstBOM: AST_BOM
    type t

    (* datatype IdStatus = *)
    (*   Val                       (* value *) *)
    (* | Exn                       (* exception *) *)
    (* | Con                       (* constructor *) *)

    structure TyAlias: sig
      type t = {
        params: CoreBOM.TyParam.t list,
        ty: CoreBOM.BomType.t
      }

      val applyToArgs: t * CoreBOM.BomType.t list -> CoreBOM.BomType.t option
      (* val fromConsDef: CoreBOM.DataConsDef.t -> TyAlias.t *)
      val arity: t -> int
      val error: t
    end

    structure TypeDefn: sig
      (* TODO: make this opaque? *)
      datatype t
        = Alias of TyAlias.t
        | Con of CoreBOM.TyCon.t

      val applyToArgs: t * CoreBOM.BomType.t list -> CoreBOM.BomType.t option
      val arity: t -> int
      val isCon: t -> t option
      val error: t
    end


    structure TyParamEnv: sig
      type t
      type env

      val lookup: env * AstBOM.TyParam.t -> CoreBOM.TyParam.t option
      val extend: env * AstBOM.TyParam.t -> env
      val getParams: env  -> CoreBOM.TyParam.t list


      val lookupThis: t * AstBOM.TyParam.t -> CoreBOM.TyParam.t option
      val extendThis: t * AstBOM.TyParam.t -> t

      val empty: t
    end

    structure TyEnv : sig
      type env
      type t

      val extend: env * CoreBOM.TyId.t * TypeDefn.t -> env
      val lookup: env * CoreBOM.TyId.t -> TypeDefn.t option
      val printKeys: env -> unit

      (* ??? can't get this to compile *)
      (* val extendThis: t * AstBOM.BomType.t * TypeDefn.t -> t *)
      (* val lookupThis: t * AstBOM.BomType.t -> TypeDefn.t option *)

      val empty: t
    end

    structure ValEnv: sig
      type env
      type t

      val extend: env * CoreBOM.ValId.t * TyAlias.t -> env
      val lookup: env * CoreBOM.ValId.t -> TyAlias.t option
    end

  val empty: t
  val emptyNamed: CoreBOM.ModuleId.t -> t
  val setName: t * CoreBOM.ModuleId.t -> t
  val setName': t * AstBOM.BomId.t -> t

  sharing type TyEnv.env = TyParamEnv.env = ValEnv.env = t
  (* sharing type ValEnv.env = TyEnv.t = HLOpEnv.t = t *)
  end
