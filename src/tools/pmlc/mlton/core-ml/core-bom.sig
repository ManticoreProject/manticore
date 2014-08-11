signature CORE_BOM_STRUCTS =
  sig
    structure AstBOM: AST_BOM
  end

signature CORE_BOM =
  sig
    include CORE_BOM_STRUCTS

    (* FIXME: factor out individual signatures *)
    include AST_BOM

    structure HLOp : sig
      (* collapse HLOp(Q)Id together here *)
    end

    structure TyCon : sig
      type t

      val fromBomType: BomType.t -> t
      val fromLongTyId: LongTyId.t -> t
      val fromDataConsDef: DataConsDef.t -> t
      val fromDataTypeDef: DataTypeDef.t -> t
    end

    structure ValId : sig
      type t

      val fromLongValueId: LongValueId.t -> t
      val fromBomId: BomId.t -> t
    end

    structure Decs : sig
      type t
    end

  end
