(* bom.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOM =
  struct

    datatype exp = E_Pt of (ProgPt.ppt * term)

    and term
      = E_Let of (var list * exp * exp)
      | E_Stmt of (var list * rhs * exp)
      | E_Fun of (lambda list * exp)
      | E_Cont of (lambda * exp)
      | E_If of (var * exp * exp)
      | E_Case of (var * (pat * exp) list * exp option)
      | E_Apply of (var * var list)
      | E_Throw of (var * var list)
      | E_Ret of var list

    and rhs
      = E_Const of const
      | E_Cast of (ty * var)
      | E_Select of (int * var)
      | E_Alloc of (ty * var list)
      | E_Wrap of var
      | E_Unwrap of var
      | E_Prim of var primop
      | E_DCon of (data_con * var list)	(* data constructor *)
      | E_HLOp of (hlop * var list)	(* application of high-level operator *)
      | E_CCall of (var * var list)	(* foreign-function calls *)
(* QUESTION: should the following operations be builtin or supported as hlops? *)
    (* non-atomic queue operations *)
      | E_QItemAlloc of var list	(* allocate a queue item *)
      | E_QEnqueue of (var * var)	(* insert an item [nonatomic] *)
      | E_QDequeue of var		(* remove an item [nonatomic] *)
      | E_QEmpty of var			(* return true if queue is empty [nonatomic] *)
    (* concurrent queue operations *)
      | E_AtomicQEnqueue of (var * var)	(* insert an item [atomic] *)
      | E_AtomicQDequeue of var		(* remove an item [atomic] *)
    (* scheduler operations *)
      | E_Dequeue of var
      | E_Enqueue of (var * var * var)
    (* VProc operations *)
      | E_HostVProc			(* gets the hosting VProc *)
      | E_VPLoad of (offset * var)	(* load a value from the given byte offset *)
					(* in the vproc structure *)
      | E_VPStore of (offset * var * var)	(* store a value at the given byte offset *)
					(* in the vproc structure *)

    and lambda = FB of {	      (* function/continuation abstraction *)
	  f : var,			(* function name *)
	  params : var list,		(* parameters *)
	  exh : var,			(* exception continuation *)
	  body : exp			(* function body *)
	}

    and pat
      = P_DCon of data_con * var
      | P_Const of const

    and const
      = E_IConst of IntInf.int * ty
      | E_SConst of string
      | E_FConst of FloatLit.float * ty
      | E_BConst of bool

    and var_kind
      = VK_None
      | VK_Let of exp
      | VK_RHS of rhs
      | VK_Param
      | VK_Fun of lambda
      | VK_Cont of lambda

    withtype var = (var_kind, ty) VarRep.var_rep
         and prim = var Prim.prim
	 and lambda = (var * var list * exp)

    datatype module = MODULE of {
	name : Atom.atom,
	externs : var CFunctions.c_fun list,
	body : lambda
      }

(* FIXME: need constructor functions *)
    fun mkExp t = E_Pt(ProgPt.new(), e)
    fun mkRet args = mkExp(E_Ret args))

  end
