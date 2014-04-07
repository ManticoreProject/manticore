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

    structure Type : sig
	type t
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

    structure Cases : sig
	type t
	type exp
	datatype node
	  = ??
	include WRAPPED
	  sharing type node' = node
	  sharing type obj = t
      end

    structure Exp : sig
	type t = Cases.exp
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
