(* rope-of-tuples.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * 
 * Translate a rope of tuples to a tuple of ropes.
 *)

(* IDEA: Why not generate parallel code? *)

structure RopeOfTuples : sig

    val transform : AST.exp -> AST.exp

end = struct

  structure A = AST
  structure T = Types

(* mapi : ('a * int -> 'b) -> 'a list -> 'b list *)
  fun mapi f xs = let
    fun m ([], _, acc) = List.rev acc
      | m (x::xs, i, acc) = m (xs, i+1, f(x,i)::acc)
    in
      m (xs, 0, [])
    end

  local

    val getTyc = BasisEnv.getTyConFromBasis

    val getDCon = BasisEnv.getDConFromBasis

    val memoListTyc : T.tycon Memo.memo = Memo.new (fn _ =>
      getTyc ["List", "list"])

    val memoRopeTyc : T.tycon Memo.memo = Memo.new (fn _ =>
      getTyc ["Rope", "rope"])

    val memoSeqTyc : T.tycon Memo.memo = Memo.new (fn _ =>
      getTyc ["ListSeq", "seq"])

    val memoListNil : A.dcon Memo.memo = Memo.new (fn _ =>
      getDCon ["List", "nil"])

    val memoListCONS : A.dcon Memo.memo = Memo.new (fn _ =>
      getDCon ["List", "CONS"])

    val memoRopeLEAF : A.dcon Memo.memo = Memo.new (fn _ =>
      getDCon ["Rope", "LEAF"])

    val memoRopeCAT : A.dcon Memo.memo = Memo.new (fn _ =>
      getDCon ["Rope", "CAT"])

  in

  (* listTyc : unit -> T.tycon *)
    fun listTyc () = Memo.get memoListTyc

  (* seqTyc : unit -> T.tycon *)
    fun seqTyc () = Memo.get memoSeqTyc

  (* ropeTyc : unit -> T.tycon *)
    fun ropeTyc () = Memo.get memoRopeTyc

  (* listNil : unit -> A.dcon *)
    fun listNil () = Memo.get memoListNil

  (* listCONS : unit -> A.dcon *)
    fun listCONS () = Memo.get memoListCONS

  (* ropeLEAF : unit -> A.dcon *)
    fun ropeLEAF () = Memo.get memoRopeLEAF

  (* ropeCAT : unit -> A.dcon *)
    fun ropeCAT () = Memo.get memoRopeCAT

  end

(* listTy : A.ty -> A.ty *)
  fun listTy (t : A.ty) : A.ty = A.ConTy ([t], listTyc ())

(* seqTy : A.ty -> A.ty *)
  fun seqTy (t : A.ty) : A.ty = A.ConTy ([t], seqTyc ())

(* ropeTy : A.ty -> A.ty *)
  fun ropeTy (t : A.ty) : A.ty = A.ConTy ([t], ropeTyc ())

(* typesInRopeTupleType : A.exp -> A.type list *)
(* For a rope whose elements are of type (t1 * t2 * ... * tn), *)
(* return the types in that  tuple: [t1, t2, ..., tn]. *)
(* As a side-effect, this function raises Fail if the arg "rope" is *)
(* neither a rope nor a rope of tuples. *)
  fun typesInRopeTupleType rope = let
    val ropeTyCon = BasisEnv.getTyConFromBasis ["Rope", "rope"]
    val rTy = TypeOf.exp rope
    val typesInTuple = 
     (case rTy
        of T.ConTy (ts, c) =>
            (if TyCon.same (c, ropeTyCon) then
              (case ts
		 of [A.TupleTy ts] => ts
		  | _ => raise Fail "expected a rope of a tuple")
	     else
               raise Fail "expected a rope")
	 | _ => raise Fail "expected a tycon")
    in
      typesInTuple
    end

(* mkSeqUnzipN : A.ty list -> A.lambda *)
(* Generate the function unzipN on sequences, for some n. *)
(* N is the arity of the tuples in the sequence. *)
(* ex: unzip3 [(1,true,"x")] --> ([1], [true], ["x"]) *)
  fun mkSeqUnzipN ts = let
    val seqToList = BasisEnv.getVarFromBasis ["ListSeq", "toList"]
    val seqFromList = BasisEnv.getVarFromBasis ["ListSeq", "fromList"]
    val listRev = BasisEnv.getVarFromBasis ["List", "rev"] 
    val n = List.length ts
    val fname = "unzipSeq" ^ Int.toString n
    val tupTy = A.TupleTy ts
    val unzipArgTy = seqTy tupTy (* sequence of tuples... *)
    val unzipResTy = A.TupleTy (List.map seqTy ts) (* ...tuple of sequences *)
    val unzipNV = Var.new (fname, A.FunTy (unzipArgTy, unzipResTy))
    val unzipArgV = Var.new ("s", unzipArgTy)
    val lpArgTy = A.TupleTy (listTy tupTy :: List.map listTy ts)
    val lpResTy = A.TupleTy (List.map listTy ts)
    val lpArgV = Var.new ("arg", lpArgTy)
    val lpV = Var.new ("lp", A.FunTy (lpArgTy, lpResTy))
    val accVs = mapi (fn (t,i) => Var.new ("acc"^Int.toString i, listTy t)) ts
    val nilBranch = A.PatMatch 
     (A.ConstPat (A.DConst (listNil (), [tupTy])),
      A.TupleExp (List.map (fn v => A.VarExp (v, [])) accVs))
    val tupV = Var.new ("tup", tupTy)
    val tupsV = Var.new ("tups", listTy tupTy)
    fun mkCons (h, t) = let
      val ty = TypeOf.exp (A.VarExp (h, []))
      in
        A.ApplyExp (A.ConstExp (A.DConst (listCONS (), [ty])),
		    A.TupleExp [A.VarExp (h, []), A.VarExp (t, [])],
		    listTy ty)
      end
    val consBranchPat = A.ConPat (listCONS (), 
				  [tupTy], 
				  A.TuplePat [A.VarPat tupV, A.VarPat tupsV])
    val consBranchBody = let
      val vs = mapi (fn (t, i) => Var.new ("t" ^ Int.toString i, t)) ts
      val lpArg = A.TupleExp (A.VarExp (tupsV, []) ::
			      ListPair.map mkCons (vs, accVs))
      in
        A.LetExp (A.ValBind (A.TuplePat (List.map A.VarPat vs),
			     A.VarExp (tupV, [])),
		  A.ApplyExp (A.VarExp (lpV, []),
			      lpArg,
			      lpResTy))
      end 
    val consBranch = A.PatMatch (consBranchPat, consBranchBody)
    val lpBody = A.CaseExp (A.VarExp (lpArgV, []),
			    [nilBranch, consBranch],
			    lpResTy)
    val lpLam = A.FB (lpV, lpArgV, lpBody)
    val lV = Var.new ("l", listTy tupTy)
    fun mkNil t = A.ConstExp (A.DConst (listNil (), [t]))
    val applySeqToList = A.ApplyExp (A.VarExp (seqToList, [tupTy]),
				     A.VarExp (unzipArgV, []),
				     listTy (A.TupleTy ts))
    val applyRev = A.ApplyExp (A.VarExp (listRev, [tupTy]), 
			       A.VarExp (lV, []), 
			       listTy tupTy)
    val applyLp = A.ApplyExp (A.VarExp (lpV, []),
			      A.TupleExp (applyRev :: List.map mkNil ts),
			      lpResTy)
    val unzipBody =
      A.LetExp (A.FunBind [lpLam],
      A.LetExp (A.ValBind (A.VarPat lV, applySeqToList),
      A.ApplyExp (A.VarExp (seqFromList, [tupTy]),
		  applyLp,
		  unzipResTy)))			      
    in
      A.FB (unzipNV, unzipArgV, unzipBody)
    end

(* mkRopeUnzipN : A.var -> A.ty list -> A.lambda *)
(* Generate the function unzipN on ropes, for some n. *)
(* N is the arity of the tuples in the rope. *)
  fun mkRopeUnzipN unzipSeqV ts = let
    fun mkVE v = A.VarExp (v, [])
    val n = List.length ts
    val fname = "unzipRope" ^ Int.toString n
    val tupTy = A.TupleTy ts
    val unzipArgTy = ropeTy tupTy (* rope of tuples... *)
    val unzipResTy = A.TupleTy (List.map ropeTy ts) (* ...tuple of ropes *)
    val unzipV = Var.new (fname, A.FunTy (unzipArgTy, unzipResTy))
    val unzipArgV = Var.new ("r", unzipArgTy)
    val lpV = Var.new ("lp", A.FunTy (unzipArgTy, unzipResTy))
    val lpArgV = Var.new ("r", unzipArgTy)
    val lenVL = Var.new ("len", Basis.intTy)
    val dataV = Var.new ("data", seqTy tupTy)
    val leafPat = A.ConPat (ropeLEAF (), 
			    [tupTy],
			    A.TuplePat [A.VarPat lenVL, A.VarPat dataV])
    fun mkLEAF e = let
      val ty = TypeOf.exp e
      in
        A.ApplyExp (A.ConstExp (A.DConst (ropeLEAF (), [ty])), 
		    A.TupleExp [A.VarExp (lenVL, []), e],
		    ropeTy ty)
      end
    val leafBody = let
      val vs = mapi (fn (t,i) => Var.new ("xs" ^ Int.toString i, t)) ts
      val tupPat = A.TuplePat (List.map A.VarPat vs)
      val applyUnzipSeq = A.ApplyExp (A.VarExp (unzipSeqV, []),
				      A.VarExp (dataV, []),
				      tupTy)
      val body = A.TupleExp (List.map mkLEAF (List.map (fn v => A.VarExp (v, [])) vs))
      in
        A.LetExp (A.ValBind (tupPat, applyUnzipSeq), body)		  
      end
    val leafBranch = A.PatMatch (leafPat, leafBody)
    val dV = Var.new ("d", Basis.intTy)
    val lenVC = Var.new ("len", Basis.intTy)
    val rLV = Var.new ("rL", ropeTy tupTy)
    val rRV = Var.new ("rR", ropeTy tupTy)
    val catPat = A.ConPat (ropeCAT (), 
			   [tupTy],
			   A.TuplePat (List.map A.VarPat [dV, lenVC, rLV, rRV]))
    fun mkCAT (e1, e2) = let
      val ty = TypeOf.exp e1 (* should be the same as e2 *)
      in
        A.ApplyExp (A.ConstExp (A.DConst (ropeCAT (), [ty])),
		    A.TupleExp [mkVE dV, mkVE lenVC, e1, e2],
		    ropeTy ty)
      end
    val catBody = let
      val vsL = mapi (fn (t,i) => Var.new ("xsL_"^Int.toString i, t)) ts
      val vsR = mapi (fn (t,i) => Var.new ("xsR_"^Int.toString i, t)) ts
      val patL = A.TuplePat (List.map A.VarPat vsL)
      val patR = A.TuplePat (List.map A.VarPat vsR)
      val bindL = A.ValBind (patL, A.ApplyExp (mkVE lpV, mkVE rLV, unzipResTy))
      val bindR = A.ValBind (patR, A.ApplyExp (mkVE lpV, mkVE rRV, unzipResTy))
      val body = A.TupleExp 
       (ListPair.map (fn (vL, vR) => mkCAT (mkVE vL, mkVE vR)) (vsL, vsR))
      in
        A.LetExp (bindL, A.LetExp (bindR, body))
                
      end
    val catBranch = A.PatMatch (catPat, catBody)
    val lpBody = A.CaseExp (A.VarExp (lpArgV, []),
			    [leafBranch, catBranch],
			    unzipResTy)
    val lpLam = A.FB (lpV, lpArgV, lpBody)
    val unzipBody = A.LetExp (A.FunBind [lpLam],
			       A.ApplyExp (A.VarExp (lpV, []),
					   A.VarExp (unzipArgV, []),
					   unzipResTy))
    in
      A.FB (unzipV, unzipArgV, unzipBody)
    end

(* transform : A.exp -> A,exp *)
(* Generate the necessary rope unzipping code, *)
(* then apply it the given rope of tuples. *)  
  fun transform (rope : A.exp) : A.exp = let
    val ts = typesInRopeTupleType rope
    val n = List.length ts
    val (seqUnzipN as A.FB (unzipSeq, _, _)) = mkSeqUnzipN ts
    val (ropeUnzipN as A.FB (unzipRope, _, _)) = mkRopeUnzipN unzipSeq ts
    val funs = A.FunBind [seqUnzipN, ropeUnzipN]
    val resTy = A.TupleTy (List.map ropeTy ts)
    val apply = A.ApplyExp (A.VarExp (unzipSeq, []), rope, resTy)
    in
      A.LetExp (funs, apply)
    end

end
