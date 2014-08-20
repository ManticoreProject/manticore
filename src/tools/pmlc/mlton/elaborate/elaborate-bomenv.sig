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

    (* type t *)

    (* datatype IdStatus = *)
    (*   Val                       (* value *) *)
    (* | Exn                       (* exception *) *)
    (* | Con                       (* constructor *) *)
    (* | Ext                       (* extern *) *)


    structure TyParamEnv: sig
      type t

      val lookup: t * AstBOM.TyParam.t -> CoreBOM.TyParam.t option
      val extend: t * AstBOM.TyParam.t -> t
      val empty: t
    end

    structure TyEnv: sig

      type t

      val lookup: t * AstBOM.BomId.t -> CoreBOM.BomType.t option
      val extend: t * AstBOM.BomId.t * CoreBOM.BomType.t -> t
      val empty: t
    end


    val getTyParamEnv: t -> TyParamEnv.t
    val getTyEnv: t -> TyEnv.t

    val setTyParamEnv: t * TyParamEnv.t -> t
    val setTyEnv: t * TyEnv.t -> t

    val empty: t


    (* structure Type: sig *)
    (*   type t *)
    (*   (* TODO: figure out how much of this needs to be re-implemented *) *)
    (* end *)

    (* structure Scheme: sig *)
    (*   type t *)

    (*   val getTyvars: t -> Tyvar.t list *)
    (*   val getTy: t -> Type.t *)
    (*   val new: Tyvar.t list * Type.t -> t *)
    (*   val generalizes: t * Type.t -> bool *)
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

    (* structure HLOPEnv: sig *)
    (*   type env *)
    (*   type t *)

    (*   val extend: t * CoreBOM.HLOpId.t * Scheme.t -> unit *)
    (*   val lookup: t * CoreBOM.HLOpId.t -> Scheme.t option *)
    (* end *)

    (* structure TypeEnv : sig *)
    (*   type env *)
    (*   type t *)

    (*   val extend: env * CoreBOM.TyCon.t * TyStr.t -> unit *)
    (*   val lookup: env * CoreBOM.TyCon.t -> TyStr.t option *)

    (*   val extendThis: t * CoreBOM.TyCon.t * TyStr.t -> unit *)
    (*   val lookupThis: t * CoreBOM.TyCon.t -> TyStr.t option *)

    (* end *)

  (*   structure ValEnv : sig *)
  (*     type env *)
  (*     type t *)
  (*     val extend: env * CoreBOM.ValId.t * Scheme.t * IdStatus -> unit *)
  (*     val lookup: env * CoreBOM.Valid -> (Scheme.t * IdStatus) option *)

  (*     val extendThis: t * CoreBOM.ValId.t * Scheme.t * IdStatus -> unit *)
  (*     val lookupThis: t * CoreBOM.ValId.t -> (Scheme.t * Idstatus) option *)
  (*   end *)


  (* sharing type ValEnv.env = TypeEnv.t = HLOpEnv.t = t *)
  end
