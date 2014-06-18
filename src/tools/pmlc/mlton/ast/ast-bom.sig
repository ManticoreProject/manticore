(* ast-bom.sig
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


signature AST_BOM_STRUCTS =
  sig
    include AST_ATOMS
   end

signature AST_BOM =
  sig
    include AST_BOM_STRUCTS

    structure BomId : AST_ID
    structure HLOpId : AST_ID
    structure TyParam : AST_ID
    structure TyArg : AST_ID
    (* tentative *)
    structure Param : AST_ID
    structure FunParam : AST_ID
	(* ?? *)
	structure LongTyId : LONGID
	structure LongConId : LONGID
	structure LongValueId : LONGID

	(* structure LongId : LONGID *)
    (* structure PrimOp : AST_ID *)
    (* structure DataConsDef : AST_ID *)


    structure Attrs : sig
    type t
    datatype node
      = Attributes of string list
	include WRAPPED
    sharing type node' = node
    sharing type obj = t
    end


    (* structure LongId : sig *)
    (* type t *)
    (* datatype node *)
    (*   = Id of BomId.t * TyArg.t list option *)
    (*   | QualifiedId of TyArg.t list option *)
    (* include WRAPPED *)
    (*   sharing type node' = node *)
    (*   sharing type obj = t *)
    (* end *)

    structure Type : sig
    type t
    type field
    datatype node
      = Param of TyParam.t
      | LongId of LongTyId.t * TyArg.t list option
      | Offset of field * field list option
      | List of t list
      | Fun of t list * t list
      | Any
      | VProc
      | Cont of TyArg.t list option
      | Addr of t
    include WRAPPED
    sharing type node' = node
      sharing type obj = t
    end

    structure DataConsDef : sig
      type t
      datatype node
        = ConsDef of BomId.t * Type.t option
      include WRAPPED
      sharing type node' = node
      sharing type obj = t
    end

    structure DataTypeDef : sig
        type t
        datatype node
          = ConsDefs of BomId.t * TyParam.t list option *
                       DataConsDef.t list
          | SimpleDef of BomId.t * TyParam.t list option * LongTyId.t
        include WRAPPED
        sharing type node' = node
        sharing type obj = t
    end

    structure RawTy : sig
    type t
    datatype node
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
    include WRAPPED
      sharing type node' = node
      sharing type obj = t
    end

    structure CArgTy : sig
    type t
    datatype node
      = Raw of RawTy.t
      | VoidPointer
    include WRAPPED
      sharing type node' = node
      sharing type obj = t
    end

    structure CReturnTy.t : sig
    type t
    datatype node
      = CArg of CArgTy.t
      | Void
    include WRAPPED
      sharing type node' = node
      sharing type obj = t
    end

    structure Field : sig
      type t
      datatype node
        = Immutable of int * Type.t
        | Mutable of int * Type.t
      include WRAPPED
        sharing type node' = node
        sharing type obj = t
    end


    structure FunDef : sig
      type t
      type exp
      datatype node
        = Def of Attrs.t option * BomId.t * TyParam.t list option
            * Param.t list option * Param.t list option * Type.t * exp
      include WRAPPED
        sharing type node' = node
        sharing type obj = t
    end

    structure Literal : sig
    type t
    datatype node
      = PosInt of int
      | Float of real
      | String of string
      | NullVP
    include WRAPPED
      sharing type node' = node
      sharing type obj = t
    end


    structure VarPat : sig
    type t
    datatype node
      = Wild
      | Var of BomId.t * Type.t option
    include WRAPPED
      sharing type node' = node
      sharing type obj = t
    end

    structure CaseRule : sig
    type t
    type exp
    datatype node
      = LongRule of LongConId.t * VarPat.t list * exp
      | LiteralRule Literal.t * exp
      | DefaultRule of VarPat.t * exp       (* collapsing CaseDefault *)
    include WRAPPED
      sharing type node' = node
      sharing type obj = t
    end

    structure TyCaseRule : sig
    type t
    type exp
    datatype node
      = TyRule of Type.t * exp
      | Default of exp
    include WRAPPED
      sharing type node' = node
      sharing type obj = t
    end

    structure SimpleExp : sig
      type t
      datatype node
        = PrimOp of 'var Prim.prim * t list (* pulled from pmlc/prim/prim.sml *)
        | AllocId of LongValueId.t * t list
        | AllocType of Type.t * t list
        | AtIndex of int * t * t option
        | TypeCast of Type.t * t
        | HostVproc
        | VpLoad of int * t
        | VpAddr of int * t
        | VpStore of int * t * t
        | Id of LongId.t
        | Lit of Literal.t
        | MLString of string    (* TODO: replace from mlton/ast/ast-const.{fun, sig} *)
    include WRAPPED
      sharing type node' = node
      sharing type obj =
    end


    structure Exp : sig
    type t
    type rhs
    datatype node
      = Let of VarPat.t list * rhs * t
      | Do of SimpleExp.t * t
      | Fun of FunDef.t list * t
      | Cont of BomId.t * Param.t list option * t * t
      | If of SimpleExp.t * t * t
      | Case of SimpleExp.t * CaseRule.t list
      | Typecase of TyParam.t * TyCaseRule.t list
      | Apply of LongId.t * SimpleExp.t list option * SimpleExp.t list option
      | Throw of LongId.t * SimpleExp.t list option
      | Return of SimpleExp.t list option
    include WRAPPED
      sharing type node' = node
      sharing type obj = t
    end

    structure RHS : sig
      type t
      type exp
      datatype node
        = Composite of exp
        | Simple of SimpleExp.t
      include WRAPPED
        sharing type node' = node
        sharing type obj = t
    end


    structure Definition : sig
      type t
      datatype node
        = Extern of CReturnTy.t * BomId.t * CArgTy.t list * Attrs.t
        | Datatype of DatatypeDef.t * DataTypeDef.t list option
        | Type of BomId.t * TyParam.t list option * Type.t
        | DefineShortId of Attrs.t option * HLOpId.t * TyParam.t list option *
                           FunParam.t list * Type.t * Exp.t option
        | DefineLongId of HLOpId.t * TyParam.t list option * LongValueId.t
        | Fun of FunDef.t * FunDef.t list option
		| InstanceType of LongTyId.t * TyArg.t list
		| Instance of LongValueId.t * TyArg.t list
      include WRAPPED
        sharing type node' = node
        sharing type obj = t
    end


  sharing type RHS.exp = Exp.t
  sharing type CaseRule.exp = Exp.t
  sharing type TyCaseRule.exp = Exp.t
  sharing type Exp.rhs = RHS.t
  sharing type FunDef.exp = Exp.t
  sharing type Type.field = Field.t
  end
