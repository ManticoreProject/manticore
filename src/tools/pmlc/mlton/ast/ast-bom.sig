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

	structure BomId : AST_ID
	structure HLOpId : AST_ID
	structure TyParam : AST_ID 	(* this is what i understand this to be *)


	structure Attrs : sig (* TODO *) end

	structure DataConsDef : sig (* TODO *) end

	structure DatatypeDef : sig
		type t
		datatype node
		  = ConsDef of BomId.t * TyParam.t list option *
					   DataConsDef.t list (* dropping option *)
		  | SimpleDef of BomId.t * TyParam.t list option * LongId.t
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


	structure TyParam : sig 	(* TODO *) end

	structure TyArg : sig 		(* TODO *) end

	structure LongId : sig
	type t
	datatype node
	  = BomId.t * TyArg.t list option
		| ?? * TyArg.t list option
	include WRAPPED
	  sharing type node' = node
	  sharing type obj = t
	end

	(* skipping for now *)
	structure Field : sig end
	(* 	type t *)
	(* 	datatype node *)
	(* 	  = TypeConstraint of int * Type.t (* ?? not sure *) *)
	(* 	  | Index of int * Type.t *)
	(* 	include WRAPPED *)
	(* 	sharing type node' = node *)
	(* 	sharing type obj = obj *)
	(* 	end *)

    structure Type : sig
	type t
	datatype node
	  = Param of TyParam.t
	  | LongId of LongId.t
	  | Record of Field.t * Field.t list option
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

	structure Params : sig 		(* TODO *) end

	structure FunDef : sig
	  type t
	  datatype node
		= Def of Attrs.t option * BomId.t * TyParam.t list option
			* Param.t list option * Param.t list option * Type.t * Exp.t
	  include WRAPPED  			(* can we factor this out? *)
		sharing type node' = node
		sharing type obj = t
	end

	structure Literal : sig
	type t
	datatype node
	  = PosInt of int
	  | Float of real
	  | String of string
	  | nullVP
	include WRAPPED
	  sharing type node' = node
	  sharing type obj = t
	end

	structure SimpleExp : sig
	  type t
	  datatype node
		= Primitive of t list
		| AllocId of LongId.t * t list
		| AtIndex of int * t * t option
		| ?? of Type.t * t 		(* not sure what (Type) SimpleExp denotes *)
		| HostVproc
		| VpLoad of int * t
		| VpAddr of int * t
		| VpStore of int * t * t
		| Id of LongId.t
		| Lit of Literal.t
		| String of MLString 	(* ?? *)
	include WRAPPED
	  sharing type node' = node
	  sharing type obj =
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
	(* type exp *)
	datatype node
	  = LongRule of LongId.t * VarPat.t list * Exp.t
	  | LiteralRule Literal.t * Exp.t
	  | DefaultRule of VarPat.t * Exp.t 		(* collapsing CaseDefault *)
	include WRAPPED
	  sharing type node' = node
	  sharing type obj = t
    end

	structure TyCaseRule : sig
	type t
	datatype node
	  = TyRule of Type.t * Exp.t
	  | Default of Exp.t
	include WRAPPED
	  sharing type node' = node
	  sharing type obj = t
	end

    structure Exp : sig
	type t
	datatype node
	  = Let of VarPat.t list * RHS.t * t
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

	structure RHS : sig 		(* not good *)
	  type t
	  datatype node
		= Composite of Exp.t
		| Simple of SimpleExp.t
	  include WRAPPED
		sharing type node' = node
		sharing type obj = t
	end


  end
