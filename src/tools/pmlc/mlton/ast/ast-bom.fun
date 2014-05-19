(* ast-bom.fun
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor AstBOM (S: AST_BOM_STRUCTS) : AST_BOM =
  struct
  open S

  structure Wrap = Region.wrap

  (* datatypes *)
    datatype type_node =
      = ??
    and varpat_node
      = Wild
      | Var of BomId.t * ty option
    withtype ty = type_node Wrap.t
         and varpat = varpat_node Wrap.t
	 and cases = cases_node Wrap.t
	 and exp = exp_node Wrap.t

  (* layout *)
    local
      structure L = Layout
    in
    fun layoutType ty = (case Type.node ty (* Wrap -> Type *)
						  of Type.Param(tyParam) => TyParam.layout tyParam
						   | Type.LongId(longId) => LongId.layout longId
						   | Type.Record(field, fields) => let
							   val fields' = Option.getOpt (fields, [])
							   val asLayouts = (map layoutField field::fields')
						   in
							   asLayouts
						   end
						   |  Type.List(ty, tys) => let
							   val tys' = Option.getOpt (tys, [])
							   val asLayouts = map layoutType ty::tys'
						   in
							   asLayouts
						   end
						   | Type.Fun(tys, tys) =>
							 L.seq [(layoutTypes tys), L.str " -> ", layoutTypes(tys)]
						   | Type.Any => L.str "any"
						   | Type.VProc => L.str "vproc"
						   | Type.Cont(tyArgs) => let
							   val asStrings = if (Option.isSome tyArgs) then
												   (layout tyArgs)
											   else
												   ""
						   in
							   L.seq [L.str("cont "), asStrings]
						   end
						   | Type.Addr(t) => layoutType t
						(* end case *))

    fun layoutVarPat v = (case Wrap.node v
	   of Wild => L.str "_"
	    | Var(x, optTy) => L.seq [BomId.layout x, " : ",
								  layoutType opTy]
	  (* end case *))
    fun layoutCases _ = ??
    fun layoutExp _ = ??
    end (* local *)

	(* Structures *)

	(* Atoms *)
	structure BomId = AstId (structure Symbol = Symbol)
	structure HLOpId = AstId (structure Symbol = Symbol)
	structure TyParam = AstId (structure Symbol = Symbol)
	structure TyArg = AstId (structure Symbol = Symbol)
	structure Param = AstId (structure Symbol = Symbol)
	structure FunParam = AstId (structure Symbol = Symbol)

	structure Attrs = struct
	open Wrap
	datatype node
	  = Attributes of string list
	type t = node Wrap.t
	type node' = node
	type obj = t
	end

	structure LongId = struct
	open Wrap
	(* type t *)
	datatype node
	  = Id of BomId.t * TyArg.t list option
	  | QualifiedId of TyArg.t list option
	type t = node Wrap.t
	type node' = node
	type obj = t
	end



    structure Type = struct
	open Wrap
	type field 					(* TODO: match at end? *)
	datatype node = type_node
	type t = node Wrap.t
    type node' = node
    type obj = t
    val layout = layoutType
    end


	structure DataConsDef = struct
	open Wrap
	datatype node = dataconsdef_node (* data_cons_def_node ? *)
	type t = node Wrap.t
	type node' = node
	type obj = t
	end

	structure DataTypeDef = struct
	open Wrap
	datatype node
	  = ConsDef of BomId.t * TyParam.t list option * DataConsDef.t list
	  | SimpleDef of BomId.t * TyParam.t list option * LongId.t
	type t = node Wrap.t
	type node' = node
	type obj = t
	end

	structure RawTy = struct
	open Wrap
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
	type t = node Wrap.t
	type node' = node
	type obj = t
	end

	structure CArgTy = struct
	datatype node
	  = Raw of RawTy.t
	  | VoidPointer
	type t = node Wrap.t
	type node' = node
	type obj = t
	end

	structure CReturnTy = struct
	datatype node
	  = CArg of CArgTy.t
	  | Void
	type t = node Wrap.t
	type node' = node
	type obj = t
	end

	structure Field = struct
	datatype node
		= Immutable of int * Type.t
		| Mutable of int * Type.t
	type t = node Wrap.t
	type node' = node
	type obj = t
	end

	structure FunDef = struct
	open Wrap
	(* type exp *)
	(* datatype node *)
	(*   = Def of Attrs.t option * BomId.t * TyParam.t list option *)
	(* 		   * Param.t list option * Param.t list option * *)
	(* 		   Type.t * exp *)
	datatype node = datatype fundef_node
	type t = node Wrap.t
	type node' = node
	type obj = t
	end

	structure Literal = struct
	open Wrap
	datatype node
	  = PosInt of int
	  | Float of real
	  | String of string
	  | NullVP
	type t = node Wrap.t
	type node' = node
	type obj = t
	end


    structure VarPat = struct
	open Wrap
	datatype node
	  = Wild
	  | Var of BomId.t * Type.t option
	type t = node Wrap.t
    type node' = node
    type obj = t
    (* val layout = layoutVarPat *)
    end

    structure CaseRule = struct
	open Wrap
	datatype node = datatype casserule_node
	type t = node Wrap.t
	type node' = node
	type obj = t
    end

	structure TyCaseRule = struct
	open Wrap
	datatype node = datatype tycaserule_node
	type t = node Wrap.t
	type node' = node
	type obj = t
	end


	structure SimpleExp = struct
	open Wrap
	datatype node
	  = PrimOp of 'var Prim.prim * t list
	  | AllocId of LongId.t * t list
	  | AllocType of Type.t * t list
	  | AtIndex of int * t * t option
	  | TypeCast of Type.t * t
	  | HostVproc
	  | VpLoad of int * t
	  | VpAddr of int * t
	  | VpStore of int * t * t
	  | Id of LongId.t
	  | Lit of Literal.t
	  | MLString of string
	type t = node Wrap.t
	type node' = node
	type obj = t
	end

    (* structure Cases = struct *)
	(* open Wrap *)
	(* type t = cases *)
	(* datatype node = datatype cases_node *)
    (* type node' = node *)
    (* type obj = t *)
    (* val layout = layoutCases *)
    (* end *)

    structure Exp = struct
	open Wrap
	datatype node = datatype exp_node
	type t = node Wrap.t
    type node' = node
    type obj = t
    val layout = layoutExp
    end

	structure RHS = struct
	datatype node = rhs_node
	type t = node Wrap.t
	type node' = node
	type obj = t
	end

	structure Definition = struct
	open Wrap.t
	datatype node
	  = Extern of CReturnTy.t * BomId.t * CArgTy.t list * Attrs.t
	  | Datatype of DatatypeDef.t * DataTypeDef.t list option
	  | Type of BomId.t * TyParam.t list option * Type.t
	  | DefineShortId of Attrs.t option * HLOpId.t *
						 TyParam.t list option *FunParam.t list *
						 ReturnTy.t option * Exp.t option
	  | DefineLongId of HLOpId.t * TyParam.t list option * LongId.t
	  | Fun of FunDef.t * FunDef.t list option
	type t = node Wrap.t
	type node' = node
	type obj = t
	end


  end
