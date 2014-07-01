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
  (* structure Param : AST_ID *)
  (* structure FunParam : AST_ID *)
  structure LongTyId : LONGID sharing LongTyId.Id = BomId
  structure LongConId : LONGID sharing LongConId.Id = BomId
  structure LongValueId : LONGID sharing LongValueId.Id = BomId
  structure PrimTycons : PRIM_TYCONS

  sharing Symbol = BomId.Symbol = HLOpId.Symbol = TyParam.Symbol
    = LongTyId.Symbol = LongConId.Symbol = LongValueId.Symbol

  structure Attrs : sig
    type t
    datatype node
      = T of string list

    val layout : t -> Layout.t

	  include WRAPPED
      sharing type node' = node
      sharing type obj = t
  end

	structure TyParams : sig
	type t
	datatype node
	  = T of TyParam.t list

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
	  type tyArgs

    datatype node
      = Param of TyParam.t
      | LongId of LongTyId.t * tyArgs option
      | Record of field list
      | Tuple of t list
      | Fun of t list * t list option * t list
      | Any
      | VProc
      | Cont of tyArgs option
      | Addr of t

    val layout : t -> Layout.t

    include WRAPPED
	    sharing type node' = node
	    sharing type obj = t
    end


	  structure TyArgs : sig
	  type t
	  datatype node
	    = ArgTypes of BomType.t list

    val layout : t -> Layout.t

    include WRAPPED
	    sharing type node' = node
	    sharing type obj = t
	  end


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
        = ConsDefs of BomId.t * TyParams.t option *
          DataConsDef.t list
        | SimpleDef of BomId.t * TyParams.t option * LongTyId.t

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
      = Def of Attrs.t option * BomId.t * TyParams.t option
        * VarPat.t list option * VarPat.t list option * BomType.t list option * exp
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
      = LongRule of LongConId.t * VarPat.t list option * exp (* FIXME in .fun layout *)
      | LiteralRule of Literal.t * exp
      | DefaultRule of VarPat.t * exp       (* collapsing CaseDefault *)
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

    include WRAPPED
      sharing type node' = node
      sharing type obj = t
  end

  structure SimpleExp : sig
      type t
      datatype node
        = PrimOp of PrimTycons.tycon * t list (* pulled from ./prim-tycons.sig *)
        | AllocId of LongValueId.t * t list
        | AllocType of TyArgs.t * t list (* FIXME in .fun *)
        | AtIndex of IntInf.int * t * t option
        | TypeCast of BomType.t * t
        | HostVproc
        | VpLoad of IntInf.int * t
        | VpAddr of IntInf.int * t
        | VpStore of IntInf.int * t * t
        | Id of LongValueId.t
        | Lit of Literal.t
        | MLString of IntInf.int vector    (* FIXME in .fun *)

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
      | ContExp of BomId.t * VarPat.t list option * t * t
      | If of SimpleExp.t * t * t
      | Case of SimpleExp.t * CaseRule.t list
      | Typecase of TyParam.t * TyCaseRule.t list
      | Apply of LongValueId.t * SimpleExp.t list option * SimpleExp.t list option
      | Throw of BomId.t * TyArgs.t option * SimpleExp.t list option (* FIXME *)
      | Return of SimpleExp.t list option

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
        | Datatype of DataTypeDef.t * DataTypeDef.t list option
        | TypeDefn of BomId.t * TyParams.t option * BomType.t
        | DefineShortId of Attrs.t option * HLOpId.t *
            TyParams.t option * VarPat.t list option * VarPat.t list option *
            BomType.t list option * Exp.t option
        | DefineLongId of HLOpId.t * TyParams.t option * LongValueId.t
        | Fun of FunDef.t list
		    | InstanceType of LongTyId.t * TyArgs.t
		    | Instance of LongValueId.t * TyArgs.t

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
  sharing type BomType.tyArgs = TyArgs.t
  (* why is this erroring? *)
  (* sharing type LongTyId.Strid = LongConId.Strid = LongValueId.Strid *)
  end
