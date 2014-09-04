signature CORE_BOM_STRUCTS =
  sig
    (* structure Region: REGION (* sharing type Region.t = Region.Wrap.region *) *)
    structure Ast: AST

    (* sharing Region = Ast.AstBOM.Region *)
  end

signature CORE_BOM =
  sig
    include CORE_BOM_STRUCTS

    structure AstBOM: AST_BOM sharing AstBOM = Ast.AstBOM

    (* For now, we copy over the structures we had from ast-bom, but
    leave their signatures blank. They can be filled in as needed,
    reducing cruft *)

    structure HLOpId: sig
    end

    structure TyParam: sig
      type t

      val fromAst: AstBOM.TyParam.t -> t
      val flattenFromAst: AstBOM.TyParams.t option -> AstBOM.TyParam.t list
      val flattenFromAst': AstBOM.TyParams.t option -> t list
      val hash: t -> int
      val name: t -> string
      val compare: t * t -> order

      include WRAPPED
        sharing type obj = t
        (* sharing type node' = node *)
    end

    structure PrimOp: sig
    end

    structure LongConId: sig
    end


    structure HLOpQId: sig
    end

    structure SymbolicId: sig
    end

    structure BomId: sig
      type t

      val fromAst: AstBOM.BomId.t -> t
      (* val fromStrid: AstBOM.Strid.t * Region.t -> t *)
      val toString: t -> string
      val bogus: t

      include WRAPPED
        sharing type obj = t
    end

    structure LongTyId: sig
      (* structure AstLongId: LONGID *)

      type t

      val fromAst: AstBOM.LongTyId.t -> t
      val toString: t -> string
      val hasQualifier: t -> bool
      val truncate: t -> BomId.t
    end


    structure LongValueId: sig
      type t

      val fromAst: AstBOM.LongValueId.t -> t
      val toString: t -> string
      val hasQualifier: t -> bool
      val truncate: t -> BomId.t
    end


    structure ModuleId: sig
      type t

      val compare: t * t -> order
      val fromLongTyId: AstBOM.LongTyId.t -> t
      val fromLongTyId': AstBOM.LongTyId.t -> t * BomId.t
      val fromBomId: AstBOM.BomId.t -> t
      val toString: t -> string
      val bogus: t
    end


    structure TyId: sig
      datatype t
        = BomTy of BomId.t
        | QBomTy of ModuleId.t * BomId.t
        (* | MLTy *)

      val fromAstBomId: AstBOM.BomId.t -> t
      val fromLongTyId: AstBOM.LongTyId.t -> t

      (* Add the given qualifier only if it doesn't yet have one *)
      val maybeQualify: t * ModuleId.t -> t

      val toString: t -> string
      val compare: t * t -> order
    end

    structure ValId : sig
      datatype t
        = BomVal of BomId.t
        | QBomVal of ModuleId.t * BomId.t

      val fromAstBomId: AstBOM.BomId.t -> t
      val fromLongValueId: AstBOM.LongValueId.t -> t

      (* Add the given qualifier only if it doesn't yet have one *)
      val maybeQualify: t * ModuleId.t -> t

      val toString: t -> string
      val compare: t * t -> order
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

    structure Field: sig
      type t
      type ty

      datatype node
        = Immutable of IntInf.int * ty
        | Mutable of IntInf.int * ty

      val keepRegion: ('a -> node) * ('a * Region.t) -> t

      include WRAPPED
        sharing type obj = t
        sharing type node' = node
      (* val fromAst: AstBOM.Field.t -> t *)
    end

    structure DataConsDef: sig
      type t
      type ty

      datatype node
        = ConsDef of BomId.t * ty option

      include WRAPPED
        sharing type node' = node
        sharing type obj = t
    end

    structure TyCon: sig
      type t
      datatype node
        = TyC of {
            id: TyId.t,
            definition: DataConsDef.t list ref,
            params: TyParam.t list
        }

      include WRAPPED
        sharing type node' = node
        sharing type obj = t
    end

    structure BomType: sig
      type t

      datatype node
        = Param of TyParam.t
        | TyCon of {
            con: TyCon.t,
            args: t list
          }
        | Record of Field.t list
        | Tuple of t list
        | Fun of {
            dom: t list,
            cont: t list,
            rng: t list
          }
        | Any
        | VProc
        | Cont of t list
        | Addr of t
        | Raw of RawTy.t
        | Error

      (* val fromAst: AstBOM.BomType.t -> t *)
      val arity: t -> int
      val errorFromAst: AstBOM.BomType.t -> t
      val keepRegion: ('a -> node) * ('a * Region.t) -> t

      val applyArg: t * TyParam.t * t -> t

      include WRAPPED
        sharing type node' = node
        sharing type obj = t
    end

    structure TyArgs: sig
      type t

      val getTypes: t -> BomType.t list
      val flattenFromAst: AstBOM.TyArgs.t option -> AstBOM.BomType.t list
    end

    structure DataTypeDef: sig
      type t
    end

    structure CArgTy: sig
    end

    structure CReturnTy: sig
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

    structure HLOp: sig
      (* collapse HLOp(Q)Id together here *)
    end

    structure Decs : sig
      (* type t *)
    end

    (* structure Type: *)
    sharing type DataConsDef.ty = Field.ty = BomType.t

  end
