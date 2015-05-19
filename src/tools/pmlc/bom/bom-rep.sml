(* bom-rep.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOMRep =
  struct

  (* Raw machine types *)
    datatype raw_ty = datatype RawTypes.raw_ty

  (* type variables are used for HLOps that have not been instantiated yet; regular BOM
   * code is monomorphic.
   *)
    datatype ty_var = TV of Stamp.stamp

    datatype ty
      = T_Param of ty_var			(* type parameter (only inside HLOps!) *)
      | T_Raw of raw_ty				(* machine types *)
      | T_Con of tyc * ty list			(* type constructor *)
      | T_Record of (bool * ty) list		(* record type; bool marks mutable fields *)
      | T_Packed of (int * bool * ty) list	(* packed record *)
      | T_Fun of ty list * ty list * ty list	(* function type (args/conts) -> results *)
      | T_Cont of ty list			(* continuation *)
(*
      | T_Array of ty
      | T_Vector of ty
      | T_Addr of ty
      | T_Bignum
      | T_VProc
*)
      | T_CFun of CFunctions.c_proto		(* C functions *)
      | T_Any					(* any type; an escape hatch that we may not need *)

    and tyc
      = Tyc of {
	  name : string,		(* the type constructor's name *)
	  stamp : Stamp.stamp,		(* a unique stamp *)
	  arity : int,			(* number of type arguments; will be 0 for datatypes *)
	  cons : data_con list ref,	(* list of constructors; this will be empty for abstract
					 * type constructors.
					 *)
	  props : PropList.holder	(* property holder *)
	}

    and data_con = DCon of {	      (* a data-constructor function *)
	  name : string,		(* the name of the constructor *)
	  stamp : Stamp.stamp,		(* a unique stamp *)
	  argTy : ty list,		(* type(s) of argument(s) to this constructor *)
	  myTyc : tyc,			(* the datatype that this constructor belongs to *)
	  props : PropList.holder
	}

  (* vproc offset *)
(* FIXME: we should switch to symbolic names for vproc access *)
    type vp_field = IntInf.int

    datatype exp = E_Pt of (ProgPt.ppt * term)

    and term
      = E_Let of (var list * exp * exp)
      | E_Stmt of (var list * rhs * exp)
      | E_Fun of (lambda list * exp)
      | E_Cont of (lambda * exp)
      | E_If of (cond * exp * exp)
      | E_Case of (var * (pat * exp) list * exp option)
      | E_Typecase of (ty_var * (ty * exp) list * exp option)  (* only inside HLOp definitions *)
      | E_Apply of (var * var list * var list)
      | E_Throw of (var * var list)
      | E_Ret of var list
      | E_HLOp of (hlop * var list * var list)	(* application of high-level operator *)

    and rhs
      = E_Prim of prim				(* primitive operator *)
      | E_Alloc of (ty * var list)		(* allocation in local heap *)
      | E_DCon of (data_con * var list)		(* data constructor; the argument list is empty for *)
						(* nullary constructors *)
      | E_Select of (int * var)			(* select i'th field (zero-based) *)
      | E_Update of (int * var * var)		(* update i'th field (zero-based) *)
      | E_AddrOf of (int * var)			(* return address of i'th field (zero-based) *)
      | E_Cast of (ty * var)			(* deprecated *)
      | E_Promote of var			(* promotion of object to global heap *)
      | E_CCall of (var * var list)		(* foreign-function calls *)
      | E_HostVProc				(* gets the hosting VProc *)
      | E_VPLoad of (vp_field * var)		(* load a value from the given byte *)
						(* offset in the vproc structure *)
      | E_VPStore of (vp_field * var * var)	(* store a value at the given byte *)
						(* offset in the vproc structure *)
      | E_VPAddr of (vp_field * var)		(* address of given byte offset *)
						(* in the vproc structure *)
      | E_Const of const

    and lambda = FB of {	  	    (* function/continuation abstraction *)
	  f : var,				(* function name *)
	  params : var list,			(* parameters *)
	  exh : var list,			(* exception continuation *)
	  body : exp				(* function body *)
	}

    and pat			  	    (* simple, one-level, patterns *)
      = P_DCon of data_con * var list		(* data constructor; the argument *)
						(* list is empty for *)
						(* nullary constructors *)
      | P_Const of const

    and var_kind			    (* the kinds of variable bindings *)
      = VK_None					(* placeholder *)
      | VK_Let of exp				(* bound on lhs of E_Let *)
      | VK_RHS of rhs				(* bound on lhs of E_Stmt *)
      | VK_Param				(* parameter to lambda *)
      | VK_Fun of lambda			(* bound to lambda in E_Fun *)
      | VK_Cont of lambda			(* bound to lambda in E_Cont *)
      | VK_CFun of c_fun			(* denotes a C function *)

    and hlop = HLOp of {
	name : string,
	id : Stamp.stamp,
	sign : hlop_sig,
	returns : bool,				(* true, if the operation returns *)
        pure : bool,                    	(* is the HLOp guaranteed to be pure? *)
        constr : bool                   	(* does the HLOp create an object worthy of *)
						(* tracking (e.g., CML channel)? *)
      }

    withtype var = (var_kind, ty) VarRep.var_rep
         and cond = var Prim.cond
         and prim = var Prim.prim
	 and const = (Literal.literal * ty)
	 and c_fun = var CFunctions.c_fun
         and hlop_sig = {		(* High-level operation signature *)
	      params : ty list,		(* parameter signature *)
	      exh : ty list,
	      results : ty list		(* list of results *)
	    }

  (* the representation of BOM program *)
    datatype program = PROGRAM of {
	exnTyc : tyc,				(* the exception datatype *)
	dataTycs : tyc list,			(* other datatypes *)
	hlops : hlop list,			(* the HLOps *)
	externs : var CFunctions.c_fun list,	(* C functions that are referenced by the program *)
	body : lambda				(* the program body *)
      }

  end
