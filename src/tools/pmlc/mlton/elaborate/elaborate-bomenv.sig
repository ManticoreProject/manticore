signature ELABORATE_BOMENV_STRUCTS =
  sig
    structure Ast: AST
    structure CoreBOM: CORE_BOM
    structure ElaborateEnv: ELABORATE_ENV
    (* from atoms/tyvar.sig *)
    structure Tyvar: TYVAR
  end

signature ELABORATE_BOMENV =
  sig
    include ELABORATE_BOMENV_STRUCTS

    structure AstBOM: AST_BOM

    type t

    datatype IdStatus =
      Val                       (* value *)
    | Exn                       (* exception *)
    | Con                       (* constructor *)
    | Ext                       (* extern *)


    (* structure Type: sig *)
    (*   type t *)
    (*   (* TODO: figure out how much of this needs to be re-implemented *) *)
    (* end *)

    structure Scheme: sig
      type t

      val getTyvars: t -> Tyvar.t list
      val getTy: t -> Type.t
      val new: Tyvar.t list * Type.t -> t
      val generalizes: t * Type.t -> bool
    end

    structure TyFun: sig
      type t
    end

    structure TyStr: sig
      type t

      val tyFun : t -> TyFun.t
      val valEnv : t -> ValEnv.t
      val def : TyFun.t * ValEnv.t -> t
    end

    structure HLOPEnv: sig
      type env
      type t

      val extend: t * CoreBOM.HLOpId.t * Scheme.t -> unit
      val lookup: t * CoreBOM.HLOpId.t -> Scheme.t option
    end

    structure TypeEnv : sig
      type env
      type t

      val extend: env * CoreBOM.TyCon.t * TyStr.t -> unit
      val lookup: env * CoreBOM.TyCon.t -> TyStr.t option

      val extendThis: t * CoreBOM.TyCon.t * TyStr.t -> unit
      val lookupThis: t * CoreBOM.TyCon.t -> TyStr.t option

    end

    structure ValEnv : sig
      type env
      type t
      val extend: env * CoreBOM.ValId.t * Scheme.t * IdStatus -> unit
      val lookup: env * CoreBOM.Valid -> (Scheme.t * IdStatus) option

      val extendThis: t * CoreBOM.ValId.t * Scheme.t * IdStatus -> unit
      val lookupThis: t * CoreBOM.ValId.t -> (Scheme.t * Idstatus) option
    end


  sharing type ValEnv.env = TypeEnv.t = HLOpEnv.t = t
  end
