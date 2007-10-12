(* sizes.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Sizes : sig

    val sizeOfExp : BOM.exp -> int

    val sizeOfApply : (BOM.var * BOM.var list * BOM.var list) -> int

  end = struct

    structure B = BOM

    fun sizeOfRHS (B.E_Const _) = 0
      | sizeOfRHS (B.E_Cast _) = 0
      | sizeOfRHS (B.E_Select _) = 1
      | sizeOfRHS (B.E_Update _) = 1
      | sizeOfRHS (B.E_AddrOf _) = 1
      | sizeOfRHS (B.E_Alloc(_, args)) = 1 + List.length args
      | sizeOfRHS (B.E_GAlloc(_, args)) = 10 + List.length args
      | sizeOfRHS (B.E_Promote _) = 3
      | sizeOfRHS (B.E_Prim _) = 1
      | sizeOfRHS (B.E_DCon(_, args)) = 2 + List.length args
      | sizeOfRHS (B.E_CCall(_, args)) = 2 + List.length args
      | sizeOfRHS B.E_HostVProc = 1
      | sizeOfRHS (B.E_VPLoad _) = 1
      | sizeOfRHS (B.E_VPStore _) = 1

    fun sizeOfFB (B.FB{params, exh, body, ...}) =
(* FIXME: the number of free variables would be a better metric here *)
	  List.length params + List.length exh + sizeOfExp body + 1

    and sizeOfExp (B.E_Pt(_, t)) = (case t
	   of B.E_Let(_, e1, e2) => sizeOfExp e1 + sizeOfExp e2
	    | B.E_Stmt(_, rhs, e) => sizeOfRHS rhs + sizeOfExp e
	    | B.E_Fun(fbs, e) =>
		List.foldl (fn (fb, sz) => sz + sizeOfFB fb) (sizeOfExp e) fbs
	    | B.E_Cont(fb, e) => sizeOfFB fb + sizeOfExp e
	    | B.E_If(_, e1, e2) => sizeOfExp e1 + sizeOfExp e2
	    | B.E_Case(_, cases, dflt) => let
		fun sizeOfPat (B.P_DCon(dc, xs)) = 1 + List.length xs
		  | sizeOfPat (B.P_Const _) = 1
		fun sizeOfCase ((pat, e), sz) = sizeOfPat pat + sizeOfExp e + sz
		in
		  List.foldl sizeOfCase (case dflt of SOME e => sizeOfExp e | _ => 0) cases
		end
	    | B.E_Apply(f, xs, ys) => sizeOfApply (f, xs, ys)
	    | B.E_Throw(k, xs) => let
		val n = List.length xs
		in
		  case B.Var.kindOf k
		   of B.VK_Cont _ => n + 1
		    | _ => n + 2
		  (* end case *)
		end
	    | B.E_Ret xs => List.length xs
	    | B.E_HLOp(hop, xs, ys) => (* FIXME *) 10
	  (* end case *))

    and sizeOfApply (f, xs, ys) = let
	  val n = List.length xs + List.length ys
	  in
	    case B.Var.kindOf f
	     of B.VK_Fun _ => n + 1
	      | _ => n + 3
	    (* end case *)
	  end

  end
