signature ELABORATE_BOMENV_STRUCTS =
  sig
    structure Ast: AST
    structure CoreBOM: CORE_BOM
    structure Env: ELABORATE_ENV
    sharing CoreBOM.Ast = Ast
  end

signature ELABORATE_BOMENV =
  sig
    include ELABORATE_BOMENV_STRUCTS

    (* structure BOM: AST_BOM *)

    type t

    (* datatype IdStatus = *)
    (*   Val                       (* value *) *)
    (* | Exn                       (* exception *) *)
    (* | Con                       (* constructor *) *)

    structure TyAlias: sig
      type t = {
        params: CoreBOM.TyParam.t list,
        ty: CoreBOM.BOMType.t
      }

      val applyToArgs: t * CoreBOM.BOMType.t list -> CoreBOM.BOMType.t option
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

      val applyToArgs: t * CoreBOM.BOMType.t list -> CoreBOM.BOMType.t option
      val arity: t -> int

      val compare: t * t -> order

      val error: t
    end


    structure TyParamEnv: sig
      type t
      type env

      val lookup: env * Ast.BOM.TyParam.t -> CoreBOM.TyParam.t option
      val extend: env * Ast.BOM.TyParam.t -> env
      val getParams: env  -> CoreBOM.TyParam.t list


      val lookupThis: t * Ast.BOM.TyParam.t -> CoreBOM.TyParam.t option
      val extendThis: t * Ast.BOM.TyParam.t -> t

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
      (* val extendThis: t * Ast.BOM.BOMType.t * TypeDefn.t -> t *)
      (* val lookupThis: t * Ast.BOM.BOMType.t -> TypeDefn.t option *)

      val empty: t
    end

    structure ValEnv: sig
      type env
      type t

      val extend: env * CoreBOM.ValId.t * CoreBOM.Val.t -> env
      val lookup: env * CoreBOM.ValId.t -> CoreBOM.Val.t option
    end

    structure PrimTyEnv : sig
      type t

      (* Don't need any functions for extending this because it's fixed *)
      val lookupML: Env.Type.t -> CoreBOM.BOMType.t option
      val lookupBOM: CoreBOM.BOMType.t -> Env.Type.t option
    end

    structure MLTyEnv: sig
      type t
      type key = Env.TypeEnv.Tycon.t
      type value = CoreBOM.BOMType.t vector -> CoreBOM.BOMType.t option

      val extendThis: t * key * value -> t
      val lookupThis: t * key -> value option

      val translateType: t * Env.TypeEnv.Type.t -> CoreBOM.BOMType.t
      val translateType': t -> Env.TypeEnv.Type.t -> CoreBOM.BOMType.t

      val empty: t
    end

    structure Context: sig
      type t

      val newInt: t * IntInf.int -> CoreBOM.Literal.t
      val newFloat: t * real -> CoreBOM.Literal.t

      val setTy: t * CoreBOM.RawTy.t -> t
      (* val setFloatTy: t * CoreBOM.RawTy.t -> t *)

      val empty: t
    end

    val empty: t
    (* val emptyNamed: CoreBOM.ModuleId.t -> t *)
    val setName: t * CoreBOM.ModuleId.t -> t
    val setName': t * Ast.BOM.BOMId.t -> t

    (* val primTyEnv: PrimTyEnv.t *)

    sharing type TyEnv.env = TyParamEnv.env = ValEnv.env = t
  end
