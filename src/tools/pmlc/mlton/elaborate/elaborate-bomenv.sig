signature ELABORATE_BOMENV_STRUCTS =
  sig
    structure Ast: AST
    structure CoreBOM: CORE_BOM
    sharing CoreBOM.AstBOM = Ast.AstBOM
    (* structure ElaborateEnv: ELABORATE_ENV *)
    (* (* from atoms/tyvar.sig *) *)
    (* structure Tyvar: TYVAR *)
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


    structure TyParamEnv: sig
      type t
      type env

      val lookup: env * AstBOM.TyParam.t -> CoreBOM.TyParam.t option
      val extend: env * AstBOM.TyParam.t -> env


      val lookupThis: t * AstBOM.TyParam.t -> CoreBOM.TyParam.t option
      val extendThis: t * AstBOM.TyParam.t -> t

      val empty: t
    end

    (* structure TyEnv: sig *)

    (*   type t *)

    (*   val lookup: t * AstBOM.BomId.t -> CoreBOM.BomType.t option *)
    (*   val extend: t * AstBOM.BomId.t * CoreBOM.BomType.t -> t *)
    (*   val empty: t *)
    (* end *)


    (* (* TODO: refactor these to be a part of the structures above *) *)
    (* val getTyParamEnv: t -> TyParamEnv.t *)
    (* val getTyEnv: t -> TyEnv.t *)

    (* val setTyParamEnv: t * TyParamEnv.t -> t *)
    (* val setTyEnv: t * TyEnv.t -> t *)

    (* val extendTyParamEnv: t * AstBOM.TyParam.t -> t *)
    (* val extendTyEnv: t * AstBOM.BomId.t * CoreBOM.BomType.t -> t *)

    (* val lookupTyParam: t * AstBOM.TyParam.t -> CoreBOM.TyParam.t option *)

    (* val empty: t *)


    (* structure Scheme: sig *)
    (*   type t *)

    (*   val getTyParams: t -> TyParam.t list *)
    (*   val getTy: t -> CoreBOM.BomType.t *)
    (*   val generalizes: t * CoreBOM.BomType.t -> bool *)

    (*   val empty: TyParam.t list * CoreBOM.BomType.t -> t *)
    (* end *)

    (* structure TyFun: sig *)
    (*   type t *)
    (* end *)

    (* structure TyStr: sig *)
    (*   type t *)

    (*   val tyFun : t -> TyFun.t *)
    (*   val valEnv : t -> ValEnv.t *)
    (*   val new : TyFun.t * ValEnv.t -> t *)
    (* end *)

    (* (* structure HLOPEnv: sig *) *)
    (*   type env *)
    (*   type t *)

    (*   val extend: t * CoreBOM.HLOpId.t * Scheme.t -> unit *)
    (*   val lookup: t * CoreBOM.HLOpId.t -> Scheme.t option *)
    (* end *)


    structure TypeDefn: sig
      (* type t *)

      (* TODO: make this opaque? *)
      datatype t
        = TyAlias of {
          params: CoreBOM.TyParam.t list,
          ty: CoreBOM.BomType.t
        }
        | TyCon of CoreBOM.TyCon.t

    end

    structure TypeEnv : sig
      type env
      type t

      val extend: env * AstBOM.BomType.t -> env
      val lookup: env * AstBOM.BomType.t -> TypeDefn.t option

      val extendThis: t * AstBOM.BomType.t -> env
      val lookupThis: t * AstBOM.BomType.t -> TypeDefn.t option

      val empty: t
    end

    (* structure ValEnv : sig *)
    (*   type env *)
    (*   type t *)
    (*   val extend: env * CoreBOM.ValId.t * Scheme.t * IdStatus -> unit *)
    (*   val lookup: env * CoreBOM.Valid -> (Scheme.t * IdStatus) option *)

    (*   val extendThis: t * CoreBOM.ValId.t * Scheme.t * IdStatus -> unit *)
    (*   val lookupThis: t * CoreBOM.ValId.t -> (Scheme.t * IdStatus) option *)
    (* end *)


  sharing type TypeEnv.env = TyParamEnv.env = t
  (* sharing type ValEnv.env = TypeEnv.t = HLOpEnv.t = t *)
  end
