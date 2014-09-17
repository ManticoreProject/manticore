(* ast-bom.sig
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


signature AST_BOM_STRUCTS =
  sig
    include AST_ATOMS
    (* include REGION  *)
   end

signature AST_BOM =
  sig
  include AST_BOM_STRUCTS

  structure Region: REGION

  structure BomId : AST_ID
  structure HLOpId : AST_ID
  structure TyParam : AST_ID
  structure PrimOp : AST_ID

  structure LongTyId : LONGID sharing LongTyId.Id = BomId
  structure LongConId : LONGID sharing LongConId.Id = BomId
  structure LongValueId : LONGID sharing LongValueId.Id = BomId
  structure HLOpQId : LONGID sharing HLOpQId.Id = HLOpId

  structure SymbolicId : AST_ID (* FIXME: Not sure what this should be *)

  sharing Symbol = BomId.Symbol = HLOpId.Symbol = TyParam.Symbol
    = LongTyId.Symbol = LongConId.Symbol = LongValueId.Symbol
    = HLOpQId.Symbol = SymbolicId.Symbol = PrimOp.Symbol
  (* sharing LongTyId.Id = LongTyId.Strid = BomId *)
  (* sharing LongTyId.Strid = LongConId.Strid = LongValueId.Strid = Strid *)
  sharing LongTyId.Strid = LongConId.Strid = LongValueId.Strid = BomId

  structure Attrs : sig
    type t
    datatype node
      = T of string list

    val layout : t -> Layout.t

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

    val layout : t -> Layout.t

    include WRAPPED
      sharing type node' = node
      sharing type obj = t
    end

  (* structure TyParams : sig *)
  (* type t *)
  (* datatype node *)
  (*   = T of TyParam.t list *)

  (* val layout : t -> Layout.t *)

  (* include WRAPPED *)
  (*   sharing type node' = node *)
  (*   sharing type obj = t *)
  (* end *)

  (* structure PrimOp : sig *)
  (*   type t *)
  (*   datatype node *)
  (*     = T of CharVector.vector *)

  (*   val layout : t -> Layout.t *)

  (*   include WRAPPED *)
  (*     sharing type node' = node *)
  (*     sharing type obj = t *)
  (* end *)


  structure BomValueId : sig
    type t
    datatype node
      = LongId of LongTyId.t
      | HLOpQId of HLOpQId.t

    val layout : t -> Layout.t

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

    structure BomType : sig
    type t
    type field
    (* type tyArgs *)

    datatype node
      = Param of TyParam.t
      | LongId of LongTyId.t * t list
      | Record of field list
      | Tuple of t list
      | Fun of t list * t list * t list
      | Any
      | VProc
      | Cont of t list
      | Addr of t
      | Raw of RawTy.t

    val layout : t -> Layout.t

    include WRAPPED
      sharing type node' = node
      sharing type obj = t
    end


    (* structure TyArgs : sig *)
    (* type t *)
    (* datatype node *)
    (*   = ArgTypes of BomType.t list *)

    (* val layout : t -> Layout.t *)

    (* include WRAPPED *)
    (*   sharing type node' = node *)
    (*   sharing type obj = t *)
    (* end *)


    structure DataConsDef : sig
    type t
    datatype node
      = ConsDef of BomId.t * BomType.t option

    val layout : t -> Layout.t

    include WRAPPED
      sharing type node' = node
      sharing type obj = t
    end

    structure DataTypeDef : sig
      type t
      datatype node
        = ConsDefs of BomId.t * TyParam.t list * DataConsDef.t list
        (* | SimpleDef of BomId.t * TyParams.t option * LongTyId.t *)

      val layout : t -> Layout.t

      include WRAPPED
        sharing type node' = node
        sharing type obj = t
    end

  structure CArgTy : sig
    type t
    datatype node
      = Raw of RawTy.t
      | VoidPointer

    val layout : t -> Layout.t

    include WRAPPED
      sharing type node' = node
      sharing type obj = t
  end

  structure CReturnTy : sig
    type t
    datatype node
      = CArg of CArgTy.t
      | Void

    val layout : t -> Layout.t

    include WRAPPED
      sharing type node' = node
      sharing type obj = t
    end

    structure Field : sig
      type t
      datatype node
        = Immutable of IntInf.int * BomType.t
        | Mutable of IntInf.int * BomType.t

      val layout : t -> Layout.t

      include WRAPPED
        sharing type node' = node
        sharing type obj = t
  end

  structure VarPat : sig
    type t
    datatype node
      = Wild of BomType.t option
      | Var of BomId.t * BomType.t option

    val layout : t -> Layout.t

    include WRAPPED
      sharing type node' = node
      sharing type obj = t
    end

  structure FunDef : sig
    type t
    type exp
    datatype node
      = Def of Attrs.t option * BomId.t * TyParam.t list
        * VarPat.t list * VarPat.t list * BomType.t list * exp

    val layout: t -> Layout.t

    include WRAPPED
      sharing type node' = node
      sharing type obj = t
  end

  structure Literal : sig
    type t
    datatype node
      = PosInt of IntInf.int
      | Float of real
      | String of string
      | NullVP

    val layout : t -> Layout.t

    include WRAPPED
      sharing type node' = node
      sharing type obj = t
  end




  structure CaseRule : sig
    type t
    type exp
    datatype node
      = LongRule of LongConId.t * VarPat.t list * exp
      | LiteralRule of Literal.t * exp
      | DefaultRule of VarPat.t * exp       (* collapsing CaseDefault *)

    val layout : t -> Layout.t
    val isDefault : t -> bool

    include WRAPPED
      sharing type node' = node
      sharing type obj = t
  end

  structure TyCaseRule : sig
    type t
    type exp
    datatype node
      = TyRule of BomType.t * exp
      | Default of exp

    val layout : t -> Layout.t

    val isDefault : t -> bool

    include WRAPPED
      sharing type node' = node
      sharing type obj = t
  end

  structure SimpleExp : sig
      type t
      datatype node
        = PrimOp of PrimOp.t * t list
        | AllocId of LongValueId.t * t list
        | AllocType of BomType.t list * t list
        | AtIndex of IntInf.int * t * t option
        | TypeCast of BomType.t * t
        | Promote of t
        | HostVproc
        | VpLoad of IntInf.int * t
        | VpAddr of IntInf.int * t
        | VpStore of IntInf.int * t * t
        | Id of LongValueId.t
        | Lit of Literal.t
        | MLString of IntInf.int vector

    val layout : t -> Layout.t

    include WRAPPED
      sharing type node' = node
      sharing type obj = t
  end


  structure Exp : sig
    type t
    type rhs
    datatype node
      = Let of VarPat.t list * rhs * t
      | Do of SimpleExp.t * t
      | FunExp of FunDef.t list * t
      | ContExp of BomId.t * VarPat.t list * t * t
      | If of SimpleExp.t * t * t
      | Case of SimpleExp.t * CaseRule.t list
      | Typecase of TyParam.t * TyCaseRule.t list
      | Apply of LongValueId.t * SimpleExp.t list * SimpleExp.t list
      | Throw of BomId.t * SimpleExp.t list
      | Return of SimpleExp.t list

    val layout : t -> Layout.t

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

      val layout : t -> Layout.t
      include WRAPPED
        sharing type node' = node
        sharing type obj = t
    end


    structure Definition : sig
      type t
      datatype node
        = Extern of CReturnTy.t * BomId.t * CArgTy.t list * Attrs.t
        | Datatype of DataTypeDef.t list
        | DatatypeAlias of BomId.t * LongTyId.t
        | TypeDefn of BomId.t * TyParam.t list * BomType.t
        | DefineShortId of Attrs.t option * HLOpId.t *
            TyParam.t list * VarPat.t list * VarPat.t list *
            BomType.t list * Exp.t option
        | DefineLongId of HLOpId.t * TyParam.t list * LongValueId.t
        | Fun of FunDef.t list
        | InstanceType of LongTyId.t * BomType.t list
        | Instance of LongValueId.t * BomType.t list

      val layout : t -> Layout.t
      include WRAPPED
        sharing type node' = node
        sharing type obj = t
    end


  sharing type RHS.exp = Exp.t
  sharing type CaseRule.exp = Exp.t
  sharing type TyCaseRule.exp = Exp.t
  sharing type Exp.rhs = RHS.t
  sharing type FunDef.exp = Exp.t
  sharing type BomType.field = Field.t
  (* sharing type BomType.tyArgs = TyArgs.t *)

  end
