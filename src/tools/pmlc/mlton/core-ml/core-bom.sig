signature CORE_BOM_STRUCTS =
  sig
    structure Ast: AST
  end

signature CORE_BOM =
  sig
    include CORE_BOM_STRUCTS

    (* For now, we copy over the structures we had from ast-bom, but
    leave their signatures blank. They can be filled in as needed,
    reducing cruft *)

    structure BomId: sig
    end

    structure HLOpId: sig
    end

    structure TyParam: sig
      include TYVAR
      val fromAst: AstBOM.TyParam.t
    end

    structure PrimOp: sig
    end

    structure LongTyId: sig
    end

    structure LongConId: sig
    end

    structure LongValueId: sig
    end

    structure HLOpQId: sig
    end

    structure SymbolicId: sig
    end

    structure Attrs: sig
    end

    structure RawTy: sig
    end

    structure TyParams: sig
    end

    structure BomValueId: sig
    end

    structure BomType: sig
      type t

      val fromAst: AstBOM.BomType.t -> t
      val arity: t -> int

    end

    structure TyArgs: sig
    end

    structure DataConsDef: sig
    end

    structure DataTypeDef: sig
    end

    structure CArgTy: sig
    end

    structure CReturnTy: sig
    end

    structure Field: sig
    end

    structure VarPat: sig
    end

    structure FunDef: sig
    end

    structure Literal: sig
    end

    structure CaseRule: sig
    end

    structure TyCaseRule: sig
    end

    structure SimpleExp: sig
    end

    structure Exp: sig
    end

    structure RHS: sig
    end

    structure Definition: sig
    end

    structure HLOp : sig
      (* collapse HLOp(Q)Id together here *)
    end

    (* structure TyVar : sig *)
    (*   type t *)
    (* end *)

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

    (* structure Type: *)

  end
