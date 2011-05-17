(* synth-tab.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure SynthTab = struct

(* tabs of pairs *)

  structure A = AST
  structure T = Types
  structure B = Basis
  structure D = DelayedBasis

  structure AU = ASTUtil
  structure FU = FlattenUtil

  structure DD  = D.DataCon
  structure DV  = D.Var
  structure DTy = D.Ty

  val vpat = A.VarPat
  fun pairPat (x, y) = A.TuplePat [vpat x, vpat y]
  fun varsPat xs = A.TuplePat (List.map vpat xs)

  fun vexp x = A.VarExp (x, [])
  fun dcon c = A.ConstExp (A.DConst (c, []))

  infix 3 **
  fun t1 ** t2 = T.TupleTy [t1, t2]

  infixr 2 -->
  fun t1 --> t2 = T.FunTy (t1, t2)

  infix 4 @@  (* application: var to exps *)
  fun f @@ args = AU.mkApplyExp (vexp f, args)

  infix 4 @@< (* application: var to vars *)
  fun f @@< xs = AU.mkApplyExp (vexp f, List.map vexp xs)

  infix 4 @@- (* application: dcon to vars *)
  fun c @@- xs = AU.mkApplyExp (dcon c, List.map vexp xs)

  infix 1 <-
  fun x <- e = A.ValBind (vpat x, e)

  infix 1 <:
  fun x <: t = Var.new (x, t)

  fun ptup es = case TranslatePtup.tr (fn e => e) es
    of SOME e => e
     | NONE => raise Fail "ptup translation failed"

  fun seqTy t =
    if FU.isInt t then DTy.int_seq ()
    else if FU.isDouble t then DTy.dbl_seq ()
    else DTy.arr_seq t

  fun seq t = let
    val ty = seqTy t
    in
      if FU.isInt t 
        then (DV.iseqUpd (), DV.iseqCreate (), ty)
      else if FU.isDouble t 
        then (DV.dseqUpd (), DV.dseqCreate (), ty)
      else
        (DV.arrSeqSub (), raise Fail "todo", ty)
    end

  fun ropeTy t =
    if FU.isInt t then DTy.int_rope ()
    else if FU.isDouble t then DTy.dbl_rope ()
    else DTy.rope t

  fun rope t = let
    val ty = ropeTy t
    in 
      if FU.isInt t 
        then (DD.intLEAF (), DV.irLength (), DV.irCwB (), DV.irEmpty (), ty)
      else if FU.isDouble t 
        then (DD.dblLEAF (), DV.drLength (), DV.drCwB (), DV.drEmpty (), ty)
      else
        (DD.ropeLEAF (), DV.ropeLength (), DV.ropeCwB (), DV.ropeEmpty (), ty)
    end

  fun farray t =
    if FU.isInt t then
      (DD.intFArray (), DTy.int_farray ())
    else if FU.isDouble t then
      (DD.dblFArray (), DTy.dbl_farray ())
    else
      (DD.farray (), DTy.farray t)

  fun tupBind (xs, tupExp, exp) = let
    val tup = "tup" <: TypeOf.exp tupExp
    in
      AU.mkLetExp ([tup <- tupExp],
        AU.mkCaseExp (vexp tup, [A.PatMatch (varsPat xs, exp)]))
    end

(*
fun tabSPair (lo, hi, f) = let
  val n = hi - lo + 1
  val s1 = S1.unsafeCreate n
  val s2 = S2.unsafeCreate n
  fun tabF i =
    if (i >= n) 
      then (s1, s2)
    else let
      val (a, b) = f (lo+i)
      in
        S1.update (s1, i, a);
	S2.update (s2, i, b);
        tabF (i+1)
      end
  in
    tabF 0
  end      
*)
  fun mkSPair (outTy1, outTy2) = let
    val (update1, create1, seqTy1) = seq outTy1
    val (update2, create2, seqTy2) = seq outTy2
    val domTy = T.TupleTy [B.intTy, B.intTy, B.intTy --> outTy1 ** outTy2]
    val rngTy = seqTy1 ** seqTy2
    val tabSPair = "tabSPair" <: domTy --> rngTy
    val lo = "lo" <: B.intTy
    val hi = "hi" <: B.intTy
    val f = "f" <: B.intTy --> outTy1 ** outTy2
    val n = "n" <: B.intTy
    val s1 = "s1" <: seqTy1
    val s2 = "s2" <: seqTy2
    val tabF = "tabF" <: B.intTy --> seqTy1 ** seqTy2
    val i = "i" <: B.intTy
    val a = "a" <: outTy1
    val b = "b" <: outTy2
    (* make tabF first *)
    val test = AU.intGTE (vexp i, vexp n)
    val thenBranch = AU.mkTupleExp [vexp s1, vexp s2]
    val elseBranch = tupBind ([a, b], f @@ [AU.plus (vexp lo) (vexp i)],
      AU.mkSeqExp ([update1 @@< [s1, i, a],
		    update2 @@< [s2, i, b]],
		   tabF @@ [AU.plusOne (vexp i)]))
    val tabFBody = AU.mkIfExp (test, thenBranch, elseBranch)
    val tabFLam = AU.mkFunWithParams (tabF, [i], tabFBody)
    (* now make tabSPair *)
    val binds = [n <- AU.plusOne (AU.minus (vexp hi) (vexp lo)),
		 s1 <- create1 @@< [n],
		 s2 <- create2 @@< [n],
		 A.FunBind [tabFLam]]
    val tabSBody = AU.mkLetExp (binds, tabF @@ [AU.mkInt 0])
    val tabSLam = AU.mkFunWithParams (tabSPair, [lo, hi, f], tabSBody)
    in
      tabSLam
    end

(*
fun rcat (rsL, rsR) = (case rsL
  of (r1L, r2L) => (case rsR
    of (r1R, r2R) => 
      (R1.cwb (r1L, r1R), R2.cwb (r2L, r2R))))
*)
  fun mkRCat (ropeTy1, ropeTy2, cwb1, cwb2) = let
    val domTy = (ropeTy1 ** ropeTy2) ** (ropeTy1 ** ropeTy2)
    val rngTy = ropeTy1 ** ropeTy2
    val rcat = "rcat" <: domTy --> rngTy
    val rsL = "rsL" <: ropeTy1 ** ropeTy2
    val rsR = "rsR" <: ropeTy1 ** ropeTy2
    val r1L = "r1L" <: ropeTy1
    val r2L = "r2L" <: ropeTy2
    val r1R = "r1R" <: ropeTy1
    val r2R = "r2R" <: ropeTy2
    val result = AU.mkTupleExp [cwb1 @@< [r1L, r1R], cwb2 @@< [r2L, r2R]]
    val body = AU.mkCaseExp (vexp rsL, 
      [A.PatMatch (varsPat [r1L, r2L], AU.mkCaseExp (vexp rsR,
        [A.PatMatch (varsPat [r1R, r2R], result)]))])
    val lam = AU.mkFunWithParams (rcat, [rsL, rsR], body)
    in
      lam
    end
(*
fun rleaves (n, s1, s2) = (R1.LEAF (n, s1), R2.LEAF (n, s2))
*)
  fun mkRLeaves (seqTy1, seqTy2, ropeTy1, ropeTy2, r1Lf, r2Lf) = let
    val domTy = T.TupleTy [B.intTy, seqTy1, seqTy2]
    val rngTy = ropeTy1 ** ropeTy2
    val rleaves = "rleaves" <: domTy --> rngTy
    val n = "n" <: B.intTy
    val s1 = "s1" <: seqTy1
    val s2 = "s2" <: seqTy2
    val body = AU.mkTupleExp [r1Lf @@- [n, s1], r2Lf @@- [n, s2]]
    val lam = AU.mkFunWithParams (rleaves, [n, s1, s2], body)
    in
      lam
    end

(*
fun tabFromToP (lo, hi, f) = let
  fun rcat ... (* see above *)
  fun rleaves ... (* see above *) 
  in
    if (lo > hi) then (R1.empty, R2.empty)
    else let
      val nElts = hi-lo+1
      in
        if (R1.maxLeafSize > nElts) then let
          val (s1, s2) = seqPair (lo, hi, f)
          in
            rleaves (nElts, s1, s2)
	  end
        else let
          val m = (hi + lo) div 2
	  val m' = m+1
          val (rsL, rsR) = (| tabFromToP (lo, m, f),
		  	      tabFromToP (m', hi, f) |)
          in
            rcat (rsL, rsR)
          end
      end
  end
 *)
  fun mkTabFromToP (outTy1, outTy2) = let
    val seqTy1 = seqTy outTy1
    val seqTy2 = seqTy outTy2
    val (lf1, len1, cwb1, empty1, ropeTy1) = rope outTy1
    val (lf2, len2, cwb2, empty2, ropeTy2) = rope outTy2
    val seqPairLam as A.FB (seqPair, _, _) = mkSPair (outTy1, outTy2)
    val rcatLam as A.FB (rcat, _, _) = 
      mkRCat (ropeTy1, ropeTy2, cwb1, cwb2)
    val rleavesLam as A.FB (rleaves, _, _) = 
      mkRLeaves (seqTy1, seqTy2, ropeTy1, ropeTy2, lf1, lf2)
    val domTy = T.TupleTy [B.intTy, B.intTy, B.intTy --> outTy1 ** outTy2]
    val rngTy = ropeTy1 ** ropeTy2
    val tabFromToP = "tabFromToP" <: domTy --> rngTy
    val lo = "lo" <: B.intTy
    val hi = "hi" <: B.intTy
    val f  = "f" <: B.intTy --> outTy1 ** outTy2
    val nElts = "nElts" <: B.intTy
    val s1 = "s1" <: seqTy1
    val s2 = "s2" <: seqTy2
    val m = "m" <: B.intTy
    val rsL = "rsL" <: ropeTy1 ** ropeTy2 
    val rsR = "rsR" <: ropeTy1 ** ropeTy2
    val if1Test = AU.intGT (vexp lo, vexp hi)
    val if1Then = AU.mkTupleExp [vexp empty1, vexp empty2]
    val if2Test = AU.intGT (vexp (DV.maxLeafSize()), vexp nElts)
    val if2Then = tupBind ([s1, s2], 
			   seqPair @@< [lo, hi, f], 
			   rleaves @@< [nElts, s1, s2])
    val if2Else = let
      val m' = "m'" <: B.intTy
      val bind1 = m <- AU.intDiv (AU.plus (vexp hi) (vexp lo), AU.mkInt 2)
      val bind2 = m' <- AU.plusOne (vexp m)
      val pt = ptup [tabFromToP @@< [lo, m, f], tabFromToP @@< [m', hi, f]]
      in
        AU.mkLetExp ([bind1, bind2], 
          tupBind ([rsL, rsR], pt, 
            rcat @@< [rsL, rsR]))
      end
    val bindNElts = nElts <- AU.plusOne (AU.minus (vexp hi) (vexp lo))
    val if1Else = AU.mkLetExp ([nElts <- AU.plusOne (AU.minus (vexp hi) (vexp lo))], 
      AU.mkIfExp (if2Test, if2Then, if2Else))
    val body = AU.mkLetExp ([A.FunBind [rcatLam], A.FunBind [rleavesLam]],
      AU.mkIfExp (if1Test, if1Then, if1Else))
    val ropePairLam = AU.mkFunWithParams (tabFromToP, [lo, hi, f], body)
    in
      {seqPair = seqPairLam,
       tabFromToP = ropePairLam}
    end

(* 
  fun tabP (n, f) = tabFromToP (0, n-1, f)
 *)
  fun mkTabP (outTy1, outTy2) = let
    val ropeTy1 = ropeTy outTy1
    val ropeTy2 = ropeTy outTy2
    val domTy = B.intTy ** (B.intTy --> outTy1 ** outTy2)
    val rngTy = ropeTy1 ** ropeTy2
    val tabP = "tabP" <: domTy --> rngTy
    val n = "n" <: B.intTy
    val f = "f" <: B.intTy --> outTy1 ** outTy2
    val {seqPair, tabFromToP} = mkTabFromToP (outTy1, outTy2)
    val A.FB (tFTP, _, _) = tabFromToP
    val body = tFTP @@ [AU.mkInt 0, AU.minusOne (vexp n), vexp f]
    val lam = AU.mkFunWithParams (tabP, [n, f], body)
    in
      {seqPair = seqPair,
       tabFromToP = tabFromToP,
       tabP = lam}
    end

(*
  fun tabFromToStepP (from, to_, step, f) = let
    fun f' i = f (from + step * i)
    in
      if (step < 0)
        then (if (to_ > from) 
	        then (R1.empty, R2.empty)
              else
                tabFromToP (0, (from-to_) div (~step), f'))
      else if (step > 0)
        then (if (from > to_)
	        then (R1.empty, R2.empty)
	      else
                tabFromToP (0, (to_ - from) div step, f'))
      else raise Fail "0 step"
    end
 *)
  fun mkTabFromToStepP (outTy1, outTy2) = let
    val {seqPair, tabFromToP} = mkTabFromToP (outTy1, outTy2)
    val A.FB (tFTP, _, _) = tabFromToP
    val (_, _, _, empty1, ropeTy1) = rope outTy1
    val (_, _, _, empty2, ropeTy2) = rope outTy2
    val fTy = B.intTy --> outTy1 ** outTy2
    val domTy = T.TupleTy [B.intTy, B.intTy, B.intTy, fTy]
    val rngTy = ropeTy1 ** ropeTy2
    val tabFromToStepP = "tabFromToStepP" <: domTy --> rngTy
    val from = "from" <: B.intTy
    val to_ = "to_" <: B.intTy
    val step = "step" <: B.intTy
    val f = "f" <: fTy
    val f' = "f'" <: fTy
    val i = "i" <: B.intTy
    val f'Body = let
      val arg = AU.plus (vexp from) (AU.times (vexp step) (vexp i))
      in
        f @@ [arg]
      end
    val f'Lam = AU.mkFunWithParams (f', [i], f'Body)
    val emptyPair = AU.mkTupleExp (List.map vexp [empty1, empty2])
    val test1 = AU.intLT (vexp step, AU.zero)
    val test2 = AU.intGT (vexp to_, vexp from)
    val test3 = AU.intGT (vexp step, AU.zero)
    val test4 = AU.intGT (vexp from, vexp to_)
    val innerIf1 = let
      val arith = AU.intDiv (AU.minus (vexp from) (vexp to_), 
			     AU.intNeg (vexp step))
      val tapp = tFTP @@ [AU.zero, arith, vexp f']
      in
        AU.mkIfExp (test2, emptyPair, tapp)
      end
    val innerIf2 = let
      val arith = AU.intDiv (AU.minus (vexp to_) (vexp from), vexp step)
      val tapp = tFTP @@ [AU.zero, arith, vexp f']
      in
	AU.mkIfExp (test4, emptyPair, tapp)
      end
    val outerIf = AU.mkIfExp (test1, 
      innerIf1, 
      AU.mkIfExp (test3, innerIf2, AU.mkFail ("0 step", rngTy)))
    val body = AU.mkLetExp ([A.FunBind [f'Lam]], outerIf)
    val lam = AU.mkFunWithParams (tabFromToStepP, [from, to_, step, f], body)
    in
      {seqPair = seqPair,
       tabFromToP = tabFromToP,
       tabFromToStepP = lam}
    end

(* 
  fun fTab (n, f) = let
    val s = Shape.Lf (0, n)
    val rs = tabP (n, f)
    in case rs
      of (r1, r2) => (F1.FArray (r1, s), F2.FArray (r2, s))
    end
 *)
  fun mkFTab (outTy1, outTy2) = let
    val (fdcon1, fty1) = farray outTy1
    val (fdcon2, fty2) = farray outTy2
    val rty1 = ropeTy outTy1
    val rty2 = ropeTy outTy2
    val shapeTy = DTy.shape_tree ()
    val shapeLf = DD.lf ()
    val {seqPair, tabFromToP, tabP} = mkTabP (outTy1, outTy2)
    val A.FB (tab, _, _) = tabP
    val domTy = B.intTy ** (B.intTy --> outTy1 ** outTy2)
    val rngTy = fty1 ** fty2
    val fTab = "fTab" <: domTy --> rngTy
    val n = "n" <: B.intTy
    val f = "f" <: B.intTy --> outTy1 ** outTy2
    val r1 = "r1" <: rty1
    val r2 = "r2" <: rty2
    val s = "s" <: shapeTy
    val bindS = s <- AU.mkApplyExp (dcon shapeLf, [AU.zero, vexp n])
    val result = AU.mkTupleExp [fdcon1 @@- [r1, s], fdcon2 @@- [r2, s]]
    val tapp = tab @@< [n, f]
    val body = AU.mkLetExp ([bindS], tupBind ([r1, r2], tapp, result))
    val lam = AU.mkFunWithParams (fTab, [n, f], body)
    in
      {seqPair = seqPair,
       tabFromToP = tabFromToP,
       tabP = tabP,
       fTab = lam}
    end

(*
  fun fTabFTS (from, to_, step, f) = let
    val rs = tabFromToStepP (from, to_, step, f)
    in case rs
      of (r1, r2) = let
        val n = R1.length rL
        val s = Shape.Lf (0, n)
        in
          (F1.FArray (r1, s), F2.FArray (r2, s))
        end
    end
 *)
  fun mkFTabFTS (outTy1, outTy2) = let
    val (_, len1, _, _, ropeTy1) = rope outTy1
    val (_, _, _, _, ropeTy2) = rope outTy2
    val shapeTy = DTy.shape_tree ()
    val shapeLf = DD.lf ()
    val (fdcon1, fty1) = farray outTy1
    val (fdcon2, fty2) = farray outTy2
    val fnTy = B.intTy --> outTy1 ** outTy2
    val domTy = T.TupleTy [B.intTy, B.intTy, B.intTy, fnTy]
    val rngTy = fty1 ** fty2
    val fTabFTS = "fTabFromToStep" <: domTy --> rngTy
    val from = "from" <: B.intTy
    val to_ = "to_" <: B.intTy
    val step = "step" <: B.intTy
    val f = "f" <: fnTy
    val r1 = "r1" <: ropeTy1
    val r2 = "r2" <: ropeTy2
    val n = "n" <: B.intTy
    val s = "s" <: shapeTy
    val {seqPair, tabFromToP, tabFromToStepP} = mkTabFromToStepP (outTy1, outTy2)
    val A.FB (tab, _, _) = tabFromToStepP
    val tapp = tab @@< [from, to_, step, f]
    val innerLet = 
      AU.mkLetExp ([n <- len1 @@< [r1],
		    s <- AU.mkApplyExp (dcon shapeLf, [AU.zero, vexp n])],
		   AU.mkTupleExp [fdcon1 @@- [r1, s], fdcon2 @@- [r2, s]])
    val body = tupBind ([r1, r2], tapp, innerLet)
    val lam = AU.mkFunWithParams (fTabFTS, [from, to_, step, f], body)
    in
      {seqPair = seqPair,
       tabFromToP = tabFromToP,
       tabFromToStepP = tabFromToStepP,
       fTabFromToStep = lam}
    end

end

