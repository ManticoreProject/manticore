(* This module corresponds to CoreML -- it is a typed counterpart to
AstBOM, like CoreML is a typed counterpart to Ast. *)

signature CORE_BOM_STRUCTS =
  sig
    structure Ast: AST
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

      val fromAst: AstBOM.Attrs.t -> t list
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
      | NoReturn                (* FIXME: delete this *)
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

	    (* equality that considers Any to be equal to anything *)
      val equal: t * t -> bool
      val equals: t list * t list -> bool
      val equal': t * t -> t option
      val equals': t list * t list -> t list option

	    (* equality that holds iff two types are identical *)
	    val strictEqual: t * t -> bool
	    val strictEqual': t * t -> t option

      val isCon: t -> t option
      val isFun: t -> t option
      val isCont: t -> t option

      val unit: t
	    val wrapTuple: t list -> t

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
      datatype t
        = Raw of RawTy.t
        | VoidPointer

      val fromAst: AstBOM.CArgTy.t -> t
    end

    structure CReturnTy: sig
      datatype t
        = CArg of CArgTy.t
        | Void

      val fromAst: AstBOM.CReturnTy.t -> t
    end

    structure VarPat: sig
    end

    structure Literal: sig
      type t
      datatype node
        = Int of IntInf.int
        | Float of real
        | String of string
        | NullVP

      val new: node * BomType.t -> t
      val typeOf: t -> BomType.t
      val valOf: t -> node
    end

    structure Val: sig
      type t

      val typeOf: t -> BomType.t
      val idOf: t -> ValId.t
      val stampOf: t -> Stamp.stamp

      val compare: t * t -> order
      val same: t * t -> bool

      (* val hasId: t * ValId.t -> bool *)

      val new: ValId.t * BomType.t * TyParam.t list -> t
      (* val new: BomType.t * TyParam.t list -> t *)

      val applyToArgs: t * BomType.t list -> t option

      val error: t
    end

    structure FunDef: sig
      type exp

      datatype t
        = Def of Attr.t list * Val.t * Val.t list * Val.t list * BomType.t list
            * exp
    end

    structure SimpleExp: sig
      type t

      datatype node
        = PrimOp of t Prim.prim
        | HostVproc
        | VpLoad of IntInf.int * t
        | VpAddr of IntInf.int * t
        | VpStore of IntInf.int * t * t
        | AllocId of Val.t * t list
        | RecAccess of IntInf.int * t * t option
        | Promote of t
        | TypeCast of BomType.t * t
        | Lit of Literal.t
        (* | MLString *)
        | Val of Val.t

        val new: node * BomType.t -> t
        val typeOf: t -> BomType.t
        val node: t -> node
		    val dest: t -> node * BomType.t

		    (* val newWithType: (t -> node) * t -> t *)

		    val error: t
    end

    structure CaseRule: sig
      type exp

      datatype t
        = LongRule of Val.t * Val.t list * exp
        | LiteralRule of Literal.t * exp
        | DefaultRule of Val.t * exp

      val returnTy: t -> BomType.t list
    end

    structure TyCaseRule: sig
      type exp

      datatype t
        = TyRule of BomType.t * exp
        | Default of exp

      val returnTy: t -> BomType.t list
    end



    structure PrimOp: sig
      include PRIM_TY

      type arg = SimpleExp.t

      (* primitive operators *)
      type t = arg Prim.prim

      (* primitive conditionals *)
      type cond = arg Prim.cond

      val returnTy: t -> BomType.t

      (* SOME if the application is good (correct number of args of
       the correct type to a real primop), otherwise, NONE *)
      val applyOp: AstBOM.PrimOp.t * arg list -> t option
      val applyCond: AstBOM.PrimOp.t * arg list -> cond option
    end

    structure Exp: sig
      type t

      datatype node
        = Let of Val.t list * rhs * t
        | FunExp of FunDef.t list * t
        | ContExp of Val.t * Val.t list * t * t
        | If of PrimOp.cond * t * t
        | Do of SimpleExp.t * t
        | Case of SimpleExp.t * CaseRule.t list
        | Typecase of TyParam.t * TyCaseRule.t list
        | Apply of Val.t * SimpleExp.t list * SimpleExp.t list
        | Throw of Val.t * SimpleExp.t list
        | Return of SimpleExp.t list
      and rhs
        = Composite of t
        | Simple of SimpleExp.t

      val new: node * BomType.t list -> t
      val typeOf: t -> BomType.t list
      val node: t -> node
		  val dest: t -> node * BomType.t list

		  val error: t
    end

    structure HLOp: sig
      (* collapse HLOp(Q)Id together here *)
    end

    structure Definition: sig
      datatype t
        = Fun of FunDef.t list
        | HLOp of Attr.t list * ValId.t * Exp.t
        | Import of BomType.t
        | Extern of CReturnTy.t * Val.t * CArgTy.t list * Attr.t list
              (* TODO: datatypes *)
    end

    (* structure Decs : sig *)
    (*   datatype t = T of Definition.t list *)

    (*   val empty: t *)
    (* end *)
    (* sharing type Exp.primOp = PrimOp.t *)
    sharing type CaseRule.exp = TyCaseRule.exp = FunDef.exp = Exp.t
end
