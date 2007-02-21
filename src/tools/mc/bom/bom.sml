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
      | E_Fun of (lambda list * exp)	(* not in BOL-Unit rep. *)
      | E_Cont of (lambda * exp)
      | E_If of (var * exp * exp)
      | E_Case of (var * (pat * exp) list * exp option)
      | E_Apply of (var * var list)
      | E_Throw of (var * var list)
      | E_Ret of var list

    and rhs
      = E_Const of const
      | E_Cast of (var * ty)
      | E_Select of (int * var)
      | E_Update of (int * var * var)
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
      | E_Enqueue of (var * var)	(* insert an item [nonatomic] *)
      | E_Dequeue of var		(* remove an item [nonatomic] *)
      | E_EmptyQ of var			(* return true if queue is empty [nonatomic] *)
    (* concurrent queue operations *)
      | E_AtomicEnqueue of (var * var)	(* insert an item [atomic] *)
      | E_AtomicDequeue of var		(* remove an item [atomic] *)
    (* VProc operations *)
      | E_HostVProc			(* returns the host VProc *)
      | E_GetRdyQ of var		(* returns the ready queue of the VProc *)
    (* Thread operations *)
      | E_GetTId of var			(* returns the current Thread ID of the VProc *)
      | E_SetTId of var	* var		(* sets the current Thread ID of the VProc *)

    and pat
      = P_DCon of data_con * var
      | P_Const of const

    and const
      = E_IConst of IntInf.int * ty
      | E_SConst of string
      | E_FConst of FloatLit.float * ty
      | E_BConst of bool

    withtype var = (var_kind, ty) VarRep.var_rep
         and prim = var Prim.prim
	 and lambda = (var * var list * exp)

  end
