(* ast-bom.sig
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchica go.edu)
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

    structure BOMId : AST_ID
    structure HLOpId : AST_ID
    structure TyParam : AST_ID
    structure PrimOp : AST_ID

    (* why have we retained LongId? *)
    structure LongId : LONGID sharing LongId.Id = BOMId
    structure HLOpQId : LONGID sharing HLOpQId.Id = HLOpId

    sharing Symbol = BOMId.Symbol = HLOpId.Symbol = TyParam.Symbol
      = LongId.Symbol = HLOpQId.Symbol = PrimOp.Symbol
    sharing LongId.Strid = HLOpQId.Strid = BOMId

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
	datatype node = datatype RawTypes.raw_ty

	val layout : t -> Layout.t

	include WRAPPED
	  where type node' = node
	  where type obj = t
      end

    structure ValueId : sig
	type t
	datatype node
	  = LongId of LongId.t
	  | HLOpQId of HLOpQId.t

	val layout : t -> Layout.t

	include WRAPPED
	  sharing type node' = node
	  sharing type obj = t
      end

    structure BOMType : sig
	  type t
	  type field
	  (* type tyArgs *)

	  datatype node
	    = Param of TyParam.t		(* type variables *)
	    | TyCon of LongId.t * t list	(* type constructors *)
	    | Record of field list		(* records *)
	    | Tuple of (bool * t) list		(* tuples (special case of records) *)
	    | Fun of t list * t list * t list	(* functions: params/conts -> result *)
	    | Cont of t list			(* continuation *)
	    | Array of t          		(* array *)
	    | Vector of t         		(* vector (immutable array) *)
	    | Addr of t				(* address (used for atomic memory operations) *)
	    | BigNum
	    | Exn				(* exception, from ML code *)
	    | Any				(* any type represented as one machine word *)
	    | VProc				(* virtual processor handle *)
	    | Raw of RawTy.t			(* raw machine types *)

	  val layout : t -> Layout.t

	  include WRAPPED
	    sharing type node' = node
	    sharing type obj = t
      end

    structure DataConsDef : sig
	type t
	datatype node
	  = ConsDef of BOMId.t * BOMType.t option

	val layout : t -> Layout.t

	include WRAPPED
	  sharing type node' = node
	  sharing type obj = t
      end

      structure DataTypeDef : sig
	  type t
	  datatype node
	    = ConsDefs of BOMId.t * TyParam.t list * DataConsDef.t list
	    (* | SimpleDef of BOMId.t * TyParams.t option * LongId.t *)

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
	    = Immutable of IntInf.int * BOMType.t
	    | Mutable of IntInf.int * BOMType.t

	  val layout : t -> Layout.t

	  include WRAPPED
	    sharing type node' = node
	    sharing type obj = t
      end

    structure VarPat : sig
	type t
	datatype node
	  = Wild of BOMType.t option
	  | Var of BOMId.t * BOMType.t option

	val layout : t -> Layout.t

	include WRAPPED
	  sharing type node' = node
	  sharing type obj = t
      end

    structure FunDef : sig
	type t
	type exp
	datatype node
	  = Def of Attrs.t option * BOMId.t * TyParam.t list
	    * VarPat.t list * VarPat.t list * BOMType.t list * exp

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
	  = PatRule of LongId.t * VarPat.t list * exp
	  | LiteralRule of Literal.t * exp

	val layout : t -> Layout.t
	(* val isDefault : t -> bool *)

	include WRAPPED
	  sharing type node' = node
	  sharing type obj = t
      end

    structure TyCaseRule : sig
	type t
	type exp
	datatype node
	  = TyRule of BOMType.t * exp
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
	  | AllocId of LongId.t * t list
	  | AllocType of BOMType.t * t list
	  | Select of IntInf.int * t
	  | Assign of IntInf.int * t * t
	  | AddrOf of IntInf.int * t
	  | TypeCast of BOMType.t * t
	  | Promote of t
	  | HostVproc
	  | VpLoad of IntInf.int * t
	  | VpAddr of IntInf.int * t
	  | VpStore of IntInf.int * t * t
	  | Id of LongId.t
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
	  | ContExp of BOMId.t * VarPat.t list * t * t
	  | If of SimpleExp.t * t * t
	  | Case of SimpleExp.t * CaseRule.t list
	  | Typecase of TyParam.t * TyCaseRule.t list
	  | Apply of LongId.t * SimpleExp.t list * SimpleExp.t list
	  | Throw of BOMId.t * SimpleExp.t list
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
	    = Datatype of DataTypeDef.t list
	    | TypeDefn of BOMId.t * TyParam.t list * BOMType.t
	    | DefineHLOp of Attrs.t option * HLOpId.t *
		TyParam.t list * VarPat.t list * VarPat.t list *
		BOMType.t list * Exp.t
	    | Fun of FunDef.t list
	    | Extern of CReturnTy.t * BOMId.t * CArgTy.t list * Attrs.t option

	  val layout : t -> Layout.t
	  include WRAPPED
	    sharing type node' = node
	    sharing type obj = t
	end

    (* Structures for the import mechanism *)
    structure PrimConDef : sig
	type t
	datatype node
	  = T of Con.t * Type.t option * LongId.t

	include WRAPPED
	  sharing type node' = node
	  sharing type obj = t
      end

    structure ImportCon : sig
	type t
	datatype node
	  = T of Longcon.t * Type.t option * BOMId.t option

	include WRAPPED
	  sharing type node' = node
	  sharing type obj = t
      end

    structure Import : sig
	type t
	datatype node
	  = Datatype of Type.t * BOMId.t option * ImportCon.t list
	  | Exn of Longcon.t * Type.t option * BOMId.t option
	  | Val of Longvid.t * Type.t * BOMId.t option

	include WRAPPED
	  sharing type node' = node
	  sharing type obj = t
      end

    sharing type RHS.exp = Exp.t
    sharing type CaseRule.exp = Exp.t
    sharing type TyCaseRule.exp = Exp.t
    sharing type Exp.rhs = RHS.t
    sharing type FunDef.exp = Exp.t
    sharing type BOMType.field = Field.t
    (* sharing type BOMType.tyArgs = TyArgs.t *)

  end
