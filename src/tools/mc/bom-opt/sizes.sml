(* sizes.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Sizes : sig

    val sizeOfApply : (BOM.var * BOM.var list * BOM.var list) -> int

  (* is the expression smaller than the given size? *)
    val smallerThan : BOM.exp * int -> bool

  end = struct

    structure B = BOM

    fun sizeOfRHS (B.E_Const _) = 0
      | sizeOfRHS (B.E_Cast _) = 0
      | sizeOfRHS (B.E_Select _) = 1
      | sizeOfRHS (B.E_Update _) = 1
      | sizeOfRHS (B.E_AddrOf _) = 1
      | sizeOfRHS (B.E_Alloc(_, args)) = 1 + List.length args
      | sizeOfRHS (B.E_Promote _) = 2
      | sizeOfRHS (B.E_Prim _) = 1
      | sizeOfRHS (B.E_DCon(_, args)) = 2 + List.length args
      | sizeOfRHS (B.E_CCall(_, args)) = 2 + List.length args
      | sizeOfRHS B.E_HostVProc = 1
      | sizeOfRHS (B.E_VPLoad _) = 1
      | sizeOfRHS (B.E_VPStore _) = 1

    fun sizeOfApply (f, xs, ys) = let
	  val n = List.length xs + List.length ys
	  in
	    case B.Var.kindOf f
	     of B.VK_Fun _ => n + 1
	      | _ => n + 3
	    (* end case *)
	  end

    fun foldSmaller f k [] = k
      | foldSmaller f k (x::r) = if k < 0 then k else foldSmaller f (f (x, k)) r

  (* Is the expression smaller than the given size?  We return diff >= 0 if
   * sizeOf(e) + diff = k; otherwise we return a negative number.  This function
   * exits early if k gets below 0.
   *)
    fun smaller (B.E_Pt(_, t), k) = if (k < 0) then k
	  else (case t
	     of B.E_Let(_, e1, e2) => smaller(e2, smaller(e1, k))
	      | B.E_Stmt(_, rhs, e) => smaller(e, k - sizeOfRHS rhs)
	      | B.E_Fun(fbs, e) => 
		    foldSmaller smallerFB (smaller(e, k)) fbs
	      | B.E_Cont(fb, e) => smaller(e, smallerFB(fb, k))
	      | B.E_If(_, e1, e2) => smaller(e2, smaller(e1, k-1))
	      | B.E_Case(_, cases, dflt) => let
		  fun sizeOfPat (B.P_DCon(dc, xs)) = 1 + List.length xs
		    | sizeOfPat (B.P_Const _) = 1
		  fun sizeOfCase ((pat, e), k) = smaller (e, k - sizeOfPat pat)
		  val k = (case dflt of SOME e => smaller(e, k) | _ => k)
		  in
		    foldSmaller sizeOfCase k cases
		  end
	      | B.E_Apply(f, xs, ys) => k - sizeOfApply (f, xs, ys)
	      | B.E_Throw(c, xs) => let
		  val n = List.length xs
		  in
		    case B.Var.kindOf c
		     of B.VK_Cont _ => k - (n + 1)
		      | _ => k - (n + 2)
		    (* end case *)
		  end
	      | B.E_Ret xs => k - List.length xs
	      | B.E_HLOp(hop, xs, ys) => (* FIXME *) k - 10
	    (* end case *))

    and smallerFB (B.FB{params, exh, body, ...}, k) =
(* FIXME: the number of free variables would be a better metric here *)
	  smaller (body, k - (1 + List.length params + List.length exh))

  (* is the expression smaller than the given size? *)
    fun smallerThan (e, k) = (smaller(e, k) >= 0)

  end
