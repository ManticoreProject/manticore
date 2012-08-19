(* sizes.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This code was adapted from the original in bom-opt/sizes.sml.
 *)

structure Sizes : sig

    val sizeOfApply : (CPS.var * CPS.var list * CPS.var list) -> int
    val sizeOfThrow : (CPS.var * CPS.var list) -> int

  (* is the expression smaller than the given size? *)
    val smallerThan : CPS.exp * int -> bool

  end = struct

    structure C = CPS

    fun sizeOfRHS (C.Var _) = 0
      | sizeOfRHS (C.Const _) = 0
      | sizeOfRHS (C.Cast _) = 0
      | sizeOfRHS (C.Select _) = 1
      | sizeOfRHS (C.Update _) = 1
      | sizeOfRHS (C.AddrOf _) = 1
      | sizeOfRHS (C.Alloc(_, args)) = 1 + List.length args
      | sizeOfRHS (C.Promote _) = 2
      | sizeOfRHS (C.Prim _) = 1
      | sizeOfRHS (C.CCall(_, args)) = 2 + List.length args
      | sizeOfRHS C.HostVProc = 1
      | sizeOfRHS (C.VPLoad _) = 1
      | sizeOfRHS (C.VPStore _) = 1
      | sizeOfRHS (C.VPAddr _) = 1

    fun sizeOfApply (f, xs, ys) = let
	  val n = List.length xs + List.length ys
	  in
	    case C.Var.kindOf f
	     of C.VK_Fun _ => n + 1
	      | _ => n + 3
	    (* end case *)
	  end

    fun sizeOfThrow (k, xs) = let
	  val n = List.length xs
	  in
	    case C.Var.kindOf k
	     of C.VK_Cont _ => n + 1
	      | _ => n + 3
	    (* end case *)
	  end

    fun foldSmaller f k [] = k
      | foldSmaller f k (x::r) = if k < 0 then k else foldSmaller f (f (x, k)) r

  (* Is the expression smaller than the given size?  We return diff >= 0 if
   * sizeOf(e) + diff = k; otherwise we return a negative number.  This function
   * exits early if k gets below 0.
   *)
    fun smaller (C.Exp(_, t), k) = if (k < 0) then k
	  else (case t
	     of C.Let(_, e1, e2) => smaller(e2, k - sizeOfRHS e1)
	      | C.Fun(fbs, e) => 
		    foldSmaller smallerFB (smaller(e, k)) fbs
	      | C.Cont(fb, e) => smaller(e, smallerFB(fb, k))
	      | C.If(_, e1, e2) => smaller(e2, smaller(e1, k-1))
	      | C.Switch(_, cases, dflt) => let
		  fun sizeOfPat _ = 1
		  fun sizeOfCase ((pat, e), k) = smaller (e, k - sizeOfPat pat)
		  val k = (case dflt of SOME e => smaller(e, k) | _ => k)
		  in
		    foldSmaller sizeOfCase k cases
		  end
	      | C.Apply(f, xs, ys) => k - sizeOfApply (f, xs, ys)
	      | C.Throw(c, xs) => let
		  val n = List.length xs
		  in
		    case C.Var.kindOf c
		     of C.VK_Cont _ => k - (n + 1)
		      | _ => k - (n + 2)
		    (* end case *)
		  end
	    (* end case *))

    and smallerFB (C.FB{params, rets, body, ...}, k) =
(* FIXME: the number of free variables would be a better metric here *)
	  smaller (body, k - (1 + List.length params + List.length rets))

  (* is the expression smaller than the given size? *)
    fun smallerThan (e, k) = (smaller(e, k) >= 0)

  end
