(* ast-bom.fun
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor AstBOM (S: AST_BOM_STRUCTS) : AST_BOM =
  struct
    open S

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

    structure Type = struct
	open Wrap
	type t = ty
	datatype node = datatype type_node
        type node' = node
        type obj = t
        val layout = layoutType
      end

    structure VarPat = struct
	open Wrap
	type t = varpat
	datatype node = datatype varpat_node
        type node' = node
        type obj = t
        val layout = layoutVarPat
      end

    structure Cases = struct
	open Wrap
	type t = cases
	datatype node = datatype cases_node
        type node' = node
        type obj = t
        val layout = layoutCases
      end

    structure Exp = struct
	open Wrap
	type t = exp
	datatype node = datatype exp_node
        type node' = node
        type obj = t
        val layout = layoutExp
      end

  end
