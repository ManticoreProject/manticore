(* ast-bom.sig
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

(* TODO: really probably all the datatypes need to be pulled out of
the signature, so we avoid having mutually-dependent structures.  *)

signature AST_BOM_STRUCTS =
  sig
    include AST_ATOMS
   end

signature AST_BOM =
  sig
    include AST_BOM_STRUCTS

	(* is this sbth to move away from? *)
	structure BomId : AST_ID
	structure HLOpId : AST_ID
	structure TyParam : AST_ID 	(* this is what i understand this to be *)


	structure Attrs : (* TODO *)

	structure Definition : sig
		type t
		datatype node
		  = Extern of CReturnTy.t * BomId.t *
					  (CArgTy.t * CArgTy.t list option) * Attrs.t
		  | Datatype of DatatypeDef.t * DataTypeDef.t list option
		  | Type of BomId.t * TyParams.t option * Type.t
		  | DefineShortId of Attrs.t option * HLOpId.t * TyParams.t option *
							 FunParams.t * ReturnTy.t option * Exp.t option
		  | DefineLongId of HLOpId.t * TyParams.t option * LongId.t
		  | Fun of FunDef.t * FunDef.t list option
		include WRAPPED
		sharing type node' = node
		sharing type obj = t
	end

	structure CReturnTy.t : sig
				  ??
	end

	structure CArgTy : sig
				  ??
	end


	structure DatatypeDef : sig
		type t
		datatype node
		  = ConsDef of BomId.t * TyParams.t option *
					   DataConsDef.t list (* dropping option *)
		  | SimpleDef of BomId.t * TyParams.t option * LongId.t
		include WRAPPED
		sharing type node' = node
		sharing type obj = t
	end


	(* added *)
	(* structure TyParam : sig *)
	(* 	type t *)
	(* 	datatype node *)
	(* 	  = t *)
	(* 	include WRAPPED *)
	(* 	sharing type node' = node *)
	(* 	sharing type obj = t *)
	(* end *)

    structure Type : sig
	type t
	datatype node
	  = Param of TyParam.t
	  | LongId of LongId.t
	  | Record of Field.t * Field.t list option
	  | List of Type.t * Type.t list option
	  | Fun of Types.t * Types.t
	  | Any
	  | VProc
	  | Cont of TyArgs.t option 	(* ?? *)
	  | Addr of t
	include WRAPPED
	  sharing type node' = node
	  sharing type obj = t
	end

	structure Types : sig
		type t
		datatype node
		  = Singleton of Type.t * MaybeMore of Type.t list option
		include WRAPPED
		  sharing type node' = node
		  sharing type obj = t
	end

	structure TyArgs : sig
		type t
		datatype node
		  = Args of Type.t * MaybeArgs of Type.t list option
		include WRAPPED
		  sharing type node' = node
		  sharing type obj = obj
	end

	(* moved here to keep type-things together *)
	(* we have the same thing here for Types, TyArgs, TyParams --> combine  *)

	structure TyParams : sig
	end		(* TODO *)

	(* skipping for now *)
	(* structure Field : sig *)
	(* 	type t *)
	(* 	datatype node *)
	(* 	  = TypeConstraint of int * Type.t (* ?? not sure *) *)
	(* 	  | Index of int * Type.t *)
	(* 	include WRAPPED *)
	(* 	sharing type node' = node *)
	(* 	sharing type obj = obj *)
	(* 	end *)

	structure FunDef : sig
	  type t
	  datatype node
		= Attributes of Attrs.t option * Id of BomId.t
			* FunInstanceType of TyParams.t option (* ?? *)
			* ExplitParams of Params.t option
			* ImplicitParams of Params.t option
			* ReturnTy of Type.t
			* Body of Exp.t
	  include WRAPPED  			(* can we factor this out? *)
		sharing type node' = node
		sharing type obj = t
	end

	structure RHS : sig
	  type t
	  datatype node
		= Composite of Exp.t
		| Simple of SimpleExp.t
	  include WRAPPED
		sharing type node' = node
		sharing type obj = t
	end

	structure SimpleExp : sig
	  type t
	  datatype node
		= Primitive of t list
		| AllocId of LongId.t * t list
		| AtIndex of PosInt.t * t * t option
		| ?? of Type.t * t 		(* not sure what (Type) SimpleExp denotes *)
		| HostVproc
		| VpLoad of PosInt.t * t
		| VpAddr of PosInt.t * t
		| VpStore of PosInt.t * t * t
		| Id of LongId.t
		| ??
		(* not sure how to handle Literal or MLString *)
	end

    structure CaseRule : sig
	type t
	type exp
	datatype node
	  = ??
	include WRAPPED
	  sharing type node' = node
	  sharing type obj = t
      end


    structure VarPat : sig
	type t
	datatype node
	  = Wild
	  | Var of ?? * Type.t option
	include WRAPPED
	  sharing type node' = node
	  sharing type obj = t
      end


    structure Exp : sig
	type t
	datatype node
	  = Let of VarPat.t list * t * t
	  | Do of t * t
	  | Fun of Lambda.t list * t
	  | Cont of Lambta.t * t
	  | If of t * t * t
	  | Case of t * Cases.t
	  | Typecase of ??
	  | Apply of ??
	  | Throw of ??
	  | Return of ??
	include WRAPPED
	  sharing type node' = node
	  sharing type obj = t
      end

  end
