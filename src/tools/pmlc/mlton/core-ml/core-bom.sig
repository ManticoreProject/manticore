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

    structure Attr: sig
      type t

      val flattenFromAst: AstBOM.Attrs.t option -> t list
    end

    structure TyParam: sig
      type t

      val fromAst: AstBOM.TyParam.t -> t
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
      val toString: t -> string
      val bogus: t
    end

    structure LongTyId: sig
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

      val fromAstBomId: AstBOM.BomId.t -> t
      val fromLongTyId: AstBOM.LongTyId.t -> t

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

      val error: t
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
      | NoReturn
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
      datatype t = datatype field_t

      val index: t -> IntInf.int
      val bogus: t
    end

    structure DataConsDef: sig
      datatype t = datatype dataconsdef_t

      val arity: t -> int
      val error: t
    end

    structure BomType: sig
      datatype t = datatype type_t

      val arity: t -> int
      val applyArg: t * TyParam.t * t -> t
      val applyArgs: t * (TyParam.t * t) list -> t
      val applyArgs': t * TyParam.t list * t list -> t option
      val uniqueTyParams: t -> TyParam.t list
      val equal: t * t -> bool
      val equals: t list * t list -> bool
      val equal': t * t -> t option
      val equals': t list * t list -> t list option
      val isCon: t -> t option
      val unit: t

    end

    structure TyCon: sig
      (* TODO: this should have a uid *)
      datatype t = datatype tycon_t

      val toBomTy: t -> BomType.t
      val arity: t -> int
      val applyToArgs: t * BomType.t list -> BomType.t option
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

    structure Val: sig
      type t

      val typeOf: t -> BomType.t
      val idOf: t -> ValId.t
      val stampOf: t -> Stamp.stamp

      val compare: t * t -> order
      val same: t * t -> bool

      val hasId: t * ValId.t -> bool

      val new: ValId.t * BomType.t * TyParam.t list -> t

      val applyToArgs: t * BomType.t list -> t option

      val error: t
    end

    structure Exp: sig
      type t

      datatype node
        (* let takes over FunExp and ConExp, plus Do, treating Do exp
        exp' as let _ = exp exp', plus TypeCast *)
        = Let of Val.t list * t * t
        | If of t * t * t
        | Case                  (* TODO *)
        | TyCase                (* TODO *)
        | Apply of Val.t * t list * t list
        | Throw of Val.t * t list
        | Return of t list
        | PrimOp of Val.t * t list
        | Alloc of Val.t * t list
        | RecAccess of IntInf.int * t * t
        | Promote of t
        | HostVproc
        | VpLoad of IntInf.int * t
        | VpAddr of IntInf.int * t
        | VpStore of IntInf.int * t * t
        | Val of Val.t
        (* | Lit of Literal.t *)
        (* | MLString of BomType.Tuple [BomType.Raw RawTy.Int64,  *)

        val new: node * BomType.t -> t
        val typeOf: t -> BomType.t
        val node: t -> node
    end

    structure HLOp: sig
      (* collapse HLOp(Q)Id together here *)
    end

    structure PrimOp: sig
      include PRIM_TY
      type arg = Val.t
      type result = BomType.t Prim.prim

      val nullaryCon: AstBOM.PrimOp.t -> result option
      val unaryCon: AstBOM.PrimOp.t -> (BomType.t -> result) option
      val binaryCon: AstBOM.PrimOp.t -> (BomType.t * BomType.t -> result) option
      val ternaryCon: AstBOM.PrimOp.t -> (
        BomType.t * BomType.t * BomType.t -> result) option

      (* (* SOME (return type) if the application is good (correct number *)
      (* of args of the correct type), otherwise, NONE *) *)
      (* val applyOp: AstBOM.PrimOp.t * BomType.t list -> BomType.t option *)
    end

    structure Definition: sig
      datatype t
        = Fun of Attr.t list * ValId.t * Exp.t
        | HLOp of Attr.t list * ValId.t * Exp.t
        | Import of BomType.t
              (* TODO: datatypes *)
    end

    (* structure Decs : sig *)
    (*   datatype t = T of Definition.t list *)

    (*   val empty: t *)
    (* end *)


  end
