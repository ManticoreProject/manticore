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
      val equal: t * t -> bool
      val equals: t list * t list -> bool
      val equals': t list * t list -> t list option
    end

    (* A TypeDefn is either an alias or a constructor plus a unique
    identifier *)
    structure TypeDefn: sig
      type t

      (* These increment the internal uid counter as a side effect *)
      val newCon: CoreBOM.TyCon.t -> t
      val newAlias: TyAlias.t -> t

      val getCon: t -> CoreBOM.TyCon.t option
      val isCon: t -> t option
      (* val getAlias: t -> TyAlias.t option *)

      val applyToArgs: t * CoreBOM.BomType.t list -> CoreBOM.BomType.t option
      val arity: t -> int

      val compare: t * t -> order

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
      val lookupCon: env * CoreBOM.TyId.t -> CoreBOM.TyCon.t option
      val printKeys: env -> unit

      (* ??? can't get this to compile *)
      (* val extendThis: t * AstBOM.BomType.t * TypeDefn.t -> t *)
      (* val lookupThis: t * AstBOM.BomType.t -> TypeDefn.t option *)

      val empty: t
    end

    structure ValEnv: sig
      type env
      type t

      val extend: env * CoreBOM.ValId.t * CoreBOM.Val.t -> env
      val lookup: env * CoreBOM.ValId.t -> CoreBOM.Val.t option
    end

  val empty: t
  val emptyNamed: CoreBOM.ModuleId.t -> t
  val setName: t * CoreBOM.ModuleId.t -> t
  val setName': t * AstBOM.BomId.t -> t

  (* val setValEnv: t * ValEnv.t -> t *)
  (* val getValEnv: t -> ValEnv.t *)

  sharing type TyEnv.env = TyParamEnv.env = ValEnv.env = t
  (* sharing type ValEnv.env = TyEnv.t = HLOpEnv.t = t *)
  end
