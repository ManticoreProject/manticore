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
      | Var of ?? * ty option
    withtype ty = type_node Wrap.t
         and varpat = varpat_node Wrap.t
	 and cases = cases_node Wrap.t
	 and exp = exp_node Wrap.t

  (* layout *)
    local
      structure L = Layout
    in
    fun layoutType _ = ??
    fun layoutVarPat v = (case Wrap.node v
	   of Wild => L.str "_"
	    | Var(x, optTy) => ??
	  (* end case *))
    fun layoutCases _ = ??
    fun layoutExp _ = ??
    end (* local *)

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
