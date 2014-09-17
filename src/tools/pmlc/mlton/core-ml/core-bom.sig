signature CORE_BOM_STRUCTS =
  sig
    (* structure Region: REGION (* sharing type Region.t = Region.Wrap.region *) *)
    structure Ast: AST

    (* sharing Region = Ast.AstBOM.Region *)
  end

signature DEPENDENCY_WRAPPER_STRUCTS =
  sig
    type node'
    (* type t *)
  end

(* need to get around cyclic dependency *)
signature DEPENDENCY_WRAPPER =
  sig
    include DEPENDENCY_WRAPPER_STRUCTS

    type t

    val wrap: node' -> t
    val node: t -> node'
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
      (* val flattenFromAst: AstBOM.TyParams.t option -> AstBOM.TyParam.t list *)
      (* val flattenFromAst': AstBOM.TyParams.t option -> t list *)
      val hash: t -> int
      val name: t -> string
      val compare: t * t -> order
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

    structure LongConId: sig
      type t

      val fromAst: AstBOM.LongConId.t -> t
      val toString: t -> string
      val hasQualifier: t -> bool
      val truncate: t -> BomId.t
    end



    structure ModuleId: sig
      type t

      val compare: t * t -> order
      val fromLongTyId: AstBOM.LongTyId.t -> t
      val fromLongTyId': AstBOM.LongTyId.t -> t * BomId.t
      val fromLongValueId: AstBOM.LongValueId.t -> t
      val fromLongValueId': AstBOM.LongValueId.t -> t * BomId.t
      val fromLongConId: AstBOM.LongConId.t -> t
      val fromLongConId': AstBOM.LongConId.t -> t * BomId.t
      val fromBomId: AstBOM.BomId.t -> t
      val toString: t -> string
      val toBomId: t -> BomId.t
      val bogus: t
    end


    structure TyId: sig
      datatype t
        = BomTy of BomId.t
        | QBomTy of ModuleId.t * BomId.t
      (* TODO: MLTy *)
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
      val fromLongConId: AstBOM.LongConId.t -> t

      (* Add the given qualifier only if it doesn't yet have one *)
      val maybeQualify: t * ModuleId.t -> t

      val toString: t -> string
      val compare: t * t -> order
    end



    structure Attrs: sig
    end

    structure RawTy: sig
      datatype t
        = Int8
        | Uint8
        | Int16
        | Uint16
        | Int32
        | Uint32
        | Int64
        | Uint64
        | Float32
        | Float64

      val fromAst: AstBOM.RawTy.t -> t
    end

    structure BomValueId: sig
    end

    datatype field_t
      = Immutable of IntInf.int * type_t
      | Mutable of IntInf.int * type_t
    and type_t
      = Param of TyParam.t
      | TyCon of {
          con: tycon_t,
          args: type_t list
        }
      | Con of {
          dom: type_t,
          rng: type_t
        }
      | Record of field_t list
      | Tuple of type_t list
      | Fun of {
          dom: type_t list,
          cont: type_t list,
          rng: type_t list
        }
      | Any
      | VProc
      | Cont of type_t list
      | Addr of type_t
      | Raw of RawTy.t
      | Error
    and dataconsdef_t
      = ConsDef of BomId.t * type_t option
    and tycon_t
      = TyC of {
          id: TyId.t,
          definition: dataconsdef_t list ref,
          params: TyParam.t list
      }

    structure Field: sig
      (* type ty *)

      datatype t = datatype field_t
      (* datatype node *)
      (*   = Immutable of IntInf.int * ty *)
      (*   | Mutable of IntInf.int * ty *)

      (* include DEPENDENCY_WRAPPER *)
      (*   sharing type node' = node *)

      val index: t -> IntInf.int
      val bogus: t
    end

    structure DataConsDef: sig
      (* type ty *)

      datatype t = datatype dataconsdef_t

      (* datatype node *)
      (*   = ConsDef of BomId.t * ty option  *)

      (* include DEPENDENCY_WRAPPER *)
      (*   sharing type node' = node *)

      val arity: t -> int
      val error: t
    end


    structure TyCon: sig
      (* TODO: this should have a uid *)
      type ty
      (* datatype t *)
      (*   = TyC of { *)
      (*       id: TyId.t, *)
      (*       definition: DataConsDef.t list ref, *)
      (*       params: TyParam.t list *)
      (*   } *)

      datatype t = datatype tycon_t

      val toBomTy: t -> ty
      val arity: t -> int
      val applyToArgs: t * ty list -> ty option
    end

    structure BomType: sig
      datatype t = datatype type_t
      (* datatype t *)
      (*   = Param of TyParam.t *)
      (*   | TyCon of { *)
      (*       con: TyCon.t, *)
      (*       args: t list *)
      (*     } *)
      (*   | Con of { *)
      (*       dom: t, *)
      (*       rng: t *)
      (*     } *)
      (*   | Record of Field.t list *)
      (*   | Tuple of t list *)
      (*   | Fun of { *)
      (*       dom: t list, *)
      (*       cont: t list, *)
      (*       rng: t list *)
      (*     } *)
      (*   | Any *)
      (*   | VProc *)
      (*   | Cont of t list *)
      (*   | Addr of t *)
      (*   | Raw of RawTy.t *)
      (*   (* | NoReturn *) *)
      (*   | Error *)

      val arity: t -> int
      val applyArg: t * TyParam.t * t -> t
      val uniqueTyParams: t -> TyParam.t list
      val equal: t * t -> bool
      val equals: t list * t list -> bool
      val equal': t * t -> t option
      val equals': t list * t list -> t list option
      val isCon: t -> t option
      val unit: t

    end


    (* structure TyArgs: sig *)
    (*   type t *)

    (*   val getTypes: t -> BomType.t list *)
      (* val flattenFromAst: AstBOM.TyArgs.t option -> AstBOM.BomType.t list *)
    (* end  *)

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


    structure PrimTy: sig
      include PRIM_TY
      type arg = BomType.t
      type result = BomType.t Prim.prim

      val nullaryCon: AstBOM.PrimOp.t -> result option
      val unaryCon: AstBOM.PrimOp.t -> (arg -> result) option
      val binaryCon: AstBOM.PrimOp.t -> (arg * arg -> result) option
      val ternaryCon: AstBOM.PrimOp.t -> (arg * arg * arg -> result) option

      (* (* SOME (return type) if the application is good (correct number *)
      (* of args of the correct type), otherwise, NONE *) *)
      (* val applyOp: AstBOM.PrimOp.t * BomType.t list -> BomType.t option *)
    end


    sharing type TyCon.ty = BomType.t
  end
