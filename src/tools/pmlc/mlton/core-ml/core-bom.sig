signature CORE_BOM_STRUCTS =
  sig
    (* structure Region: REGION (* sharing type Region.t = Region.Wrap.region *) *)
    structure Ast: AST (* sharing Ast.AstBOM.Region = Region *)

    (* sharing Region = Ast.AstBOM.Region *)
  end

signature CORE_BOM =
  sig
    include CORE_BOM_STRUCTS

    structure AstBOM: AST_BOM

    (* For now, we copy over the structures we had from ast-bom, but
    leave their signatures blank. They can be filled in as needed,
    reducing cruft *)

    structure BomId: sig
      type t

      val fromAst: AstBOM.BomId.t -> t
    end

    structure HLOpId: sig
    end

    structure TyParam: sig
      include TYVAR

      val fromAst: AstBOM.TyParam.t -> t
    end

    structure PrimOp: sig
    end

    structure LongTyId: sig
      type t
    end

    structure LongConId: sig
    end

    structure LongValueId: sig
      type t
    end

    structure HLOpQId: sig
    end

    structure SymbolicId: sig
    end

    structure Attrs: sig
    end

    structure RawTy: sig
      type t

      val fromAst: AstBOM.RawTy.t -> t
    end

    (* structure TyParams: sig *)
    (* end *)

    structure BomValueId: sig
    end

    structure BomType: sig
      type t

      val fromAst: AstBOM.BomType.t -> t
      val arity: t -> int
    end

    structure TyArgs: sig
      type t

      val getTypes: t -> BomType.t list
    end

    structure DataConsDef: sig
      type t
    end

    structure DataTypeDef: sig
      type t
    end

    structure CArgTy: sig
    end

    structure CReturnTy: sig
    end

    structure Field: sig
      type t

      val fromAst: AstBOM.Field.t -> t
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
      (* type t *)

      (* val fromBomType: BomType.t -> t *)
      (* val fromLongTyId: LongTyId.t -> t *)
      (* val fromDataConsDef: DataConsDef.t -> t *)
      (* val fromDataTypeDef: DataTypeDef.t -> t *)
    end

    structure ValId : sig
      (* type t *)

      (* val fromLongValueId: LongValueId.t -> t *)
      (* val fromBomId: BomId.t -> t *)
    end

    structure Decs : sig
      (* type t *)
    end

    (* structure Type: *)

  end
