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

  infix 3 **
  fun t1 ** t2 = T.TupleTy [t1, t2]

  infixr 2 -->
  fun t1 --> t2 = T.FunTy (t1, t2)

  infix 4 @@
  fun f @@ args = AU.mkApplyExp (vexp f, args)

  infix 4 @@<
  fun f @@< xs = AU.mkApplyExp (vexp f, List.map vexp xs)

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
        then (DD.intLEAF (), DV.irFromSeq (), DV.irCwB (), DV.irEmpty (), ty)
      else if FU.isDouble t 
        then (DD.dblLEAF (), DV.drFromSeq (), DV.drCwB (), DV.drEmpty (), ty)
      else
        (DD.ropeLEAF (), DV.ropeFromSeq (), DV.ropeCwB (), DV.ropeEmpty (), ty)
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
    fun dcon c = A.ConstExp (A.DConst (c, []))
    val domTy = T.TupleTy [B.intTy, seqTy1, seqTy2]
    val rngTy = ropeTy1 ** ropeTy2
    val rleaves = "rleaves" <: domTy --> rngTy
    val n = "n" <: B.intTy
    val s1 = "s1" <: seqTy1
    val s2 = "s2" <: seqTy2
    val body = AU.mkTupleExp [AU.mkApplyExp (dcon r1Lf, [vexp n, vexp s1]),
			      AU.mkApplyExp (dcon r2Lf, [vexp n, vexp s2])]
    val lam = AU.mkFunWithParams (rleaves, [n, s1, s2], body)
    in
      lam
    end

(*
fun tabRPair (lo, hi, f) = 
  if (lo > hi) then
    (R1.empty, R2.empty)
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
        val (rsL, rsR) = (| tabRPair (lo, m, f),
			    tabRPair (m', hi, f) |)
        in
          rcat (rsL, rsR)
        end
 *)
  fun mkRPair (outTy1, outTy2) = let
    val seqTy1 = seqTy outTy1
    val seqTy2 = seqTy outTy2
    val (lf1, fromSeq1, cwb1, empty1, ropeTy1) = rope outTy1
    val (lf2, fromSeq2, cwb2, empty2, ropeTy2) = rope outTy2
    val seqPairLam as A.FB (seqPair, _, _) = mkSPair (outTy1, outTy2)
    val rcatLam as A.FB (rcat, _, _) = 
      mkRCat (ropeTy1, ropeTy2, cwb1, cwb2)
    val rleavesLam as A.FB (rleaves, _, _) = 
      mkRLeaves (seqTy1, seqTy2, ropeTy1, ropeTy2, lf1, lf2)
    val domTy = T.TupleTy [B.intTy, B.intTy, B.intTy --> outTy1 ** outTy2]
    val rngTy = ropeTy1 ** ropeTy2
    val tabRPair = "tabRPair" <: domTy --> rngTy
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
      in
        AU.mkLetExp ([bind1, bind2], 
          tupBind ([rsL, rsR], ptup [tabRPair @@< [lo, m, f], tabRPair @@< [m', hi, f]],
            rcat @@< [rsL, rsR]))
      end
    val bindNElts = nElts <- AU.plusOne (AU.minus (vexp hi) (vexp lo))
    val if1Else = AU.mkLetExp ([nElts <- AU.plusOne (AU.minus (vexp hi) (vexp lo))], 
      AU.mkIfExp (if2Test, if2Then, if2Else))
    val body = AU.mkLetExp ([A.FunBind [rcatLam], A.FunBind [rleavesLam]],
      AU.mkIfExp (if1Test, if1Then, if1Else))
    val ropePairLam = AU.mkFunWithParams (tabRPair, [lo, hi, f], body)
    in
      {seqPair = seqPairLam,
       ropePair = ropePairLam}
    end

(* 
fun tabFPair (n, f) = let
  val s = Shape.Lf (0, n)
  val rs = rtab (0, n-1, f)
  in case rs
    of (r1, r2) => (F1.FArray (r1, s), F2.FArray (r2, s))
  end
 *)
  fun mkFPair (outTy1, outTy2) = let
    fun dcon c = A.ConstExp (A.DConst (c, []))
    val (fdcon1, fty1) = farray outTy1
    val (fdcon2, fty2) = farray outTy2
    val rty1 = ropeTy outTy1
    val rty2 = ropeTy outTy2
    val shapeTy = DTy.shape_tree ()
    val shapeLf = DD.lf ()
    val {seqPair, ropePair} = mkRPair (outTy1, outTy2)
    val A.FB (rtab, _, _) = ropePair
    val domTy = B.intTy ** (B.intTy --> outTy1 ** outTy2)
    val rngTy = fty1 ** fty2
    val tabFPair = "tabFPair" <: domTy --> rngTy
    val n = "n" <: B.intTy
    val f = "f" <: B.intTy --> outTy1 ** outTy2
    val r1 = "r1" <: rty1
    val r2 = "r2" <: rty2
    val s = "s" <: shapeTy
    val bindS = s <- AU.mkApplyExp (dcon shapeLf, [AU.mkInt 0, vexp n])
    fun appDCon (c, xs) = AU.mkApplyExp (dcon c, List.map vexp xs)
    val result = AU.mkTupleExp [appDCon (fdcon1, [r1, s]), appDCon (fdcon2, [r2, s])]
    val applyRTab = 
      AU.mkApplyExp (vexp rtab, [AU.mkInt 0, AU.minusOne (vexp n), vexp f])
    val body = AU.mkLetExp ([bindS], tupBind ([r1, r2], applyRTab, result))
    val lam = AU.mkFunWithParams (tabFPair, [n, f], body)
    in
      {seqPair = seqPair,
       ropePair = ropePair,
       farrayPair = lam}
    end

end
