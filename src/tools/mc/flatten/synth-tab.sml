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

  fun seq t = 
    if FU.isInt t then
      (DTy.int_seq (), DV.iseqUpd (), DV.iseqCreate ())
    else if FU.isDouble t then
      (DTy.dbl_seq (), DV.dseqUpd (), DV.dseqCreate ())
    else
      (DTy.arr_seq t, DV.arrSeqSub (), raise Fail "todo")

  fun rope t =
    if FU.isInt t then
      (DV.irFromSeq (), DTy.int_rope ())
    else if FU.isDouble t then
      (DV.drFromSeq (), DTy.dbl_rope ())
    else
      (DV.ropeFromSeq (), DTy.rope t)

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
fun tabSPair (n, f) = let
  val s1 = S1.unsafeCreate n
  val s2 = S2.unsafeCreate n
  fun tabF i =
    if (i>=n) 
      then (s1, s2)
    else let
      val (a, b) = f i
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
    val (seqTy1, update1, create1) = seq outTy1
    val (seqTy2, update2, create2) = seq outTy2
    val domTy = B.intTy ** (B.intTy --> (outTy1 ** outTy2))
    val rngTy = seqTy1 ** seqTy2
    val tabSPair = "tabSPair" <: domTy --> rngTy
    val n = "n" <: B.intTy
    val f = "f" <: B.intTy --> outTy1 ** outTy2
    val s1 = "s1" <: seqTy1
    val s2 = "s2" <: seqTy2
    val tabF = "tabF" <: B.intTy --> seqTy1 ** seqTy2
    val i = "i" <: B.intTy
    val a = "a" <: outTy1
    val b = "b" <: outTy2
    (* make tabF first *)
    val test = AU.intGTE (vexp i, vexp n)
    val thenBranch = AU.mkTupleExp [vexp s1, vexp s2]
    val elseBranch = tupBind ([a, b], f @@< [i], 
      AU.mkSeqExp ([update1 @@< [s1, i, a],
		    update2 @@< [s2, i, b]],
		   tabF @@ [AU.plusOne (vexp i)]))
    val tabFBody = AU.mkIfExp (test, thenBranch, elseBranch)
    val tabFLam = AU.mkFunWithParams (tabF, [i], tabFBody)
    (* now make tabSPair *)
    val binds = [s1 <- create1 @@< [n],
		 s2 <- create2 @@< [n],
		 A.FunBind [tabFLam]]
    val tabSBody = AU.mkLetExp (binds, tabF @@ [AU.mkInt 0])
    val tabSLam = AU.mkFunWithParams (tabSPair, [n, f], tabSBody)
    in
      tabSLam
    end

(* 
fun tabRPair (n, f) = let
  val (s1, s2) = tabSPair (n, f)
  in
    (R1.fromSeq s1, R2.fromSeq s2)
  end   
 *)
  fun mkRPair (outTy1, outTy2) = let
    val (seqTy1, _, _) = seq outTy1
    val (seqTy2, _, _) = seq outTy2
    val (fromSeq1, ropeTy1) = rope outTy1
    val (fromSeq2, ropeTy2) = rope outTy2
    val seqPairLam as A.FB (seqPair, _, _) = mkSPair (outTy1, outTy2)
    val domTy = B.intTy ** (B.intTy --> (outTy1 ** outTy2))
    val rngTy = ropeTy1 ** ropeTy2
    val tabRPair = "tabRPair" <: domTy --> rngTy
    val n  = "n" <: B.intTy
    val f  = "f" <: (B.intTy --> (outTy1 ** outTy2))
    val s1 = "s1" <: seqTy1
    val s2 = "s2" <: seqTy2
    val body = tupBind ([s1, s2], seqPair @@< [n, f],
      AU.mkTupleExp [fromSeq1 @@< [s1], fromSeq2 @@< [s2]])
    val ropePairLam = AU.mkFunWithParams (tabRPair, [n, f], body)
    in
      {seqPair = seqPairLam,
       ropePair = ropePairLam}
    end

(* 
fun tabFPair (n, f) = let
  val s = Shape.Lf (0, n)
  val rs = tabRPair (n, f)
  in case rs
    of (r1, r2) => (F1.FArray (r1, s), F2.FArray (r2, s))
  end
 *)
  fun mkFPair (outTy1, outTy2) = let
    fun dcon c = A.ConstExp (A.DConst (c, []))
    val (fdcon1, fty1) = farray outTy1
    val (fdcon2, fty2) = farray outTy2
    val (_, rty1) = rope outTy1
    val (_, rty2) = rope outTy2
    val shapeTy = DTy.shape_tree ()
    val shapeLf = DD.lf ()
    val {seqPair, ropePair} = mkRPair (outTy1, outTy2)
    val A.FB (rtab, _, _) = ropePair
    val domTy = B.intTy ** (B.intTy --> (outTy1 ** outTy2))
    val rngTy = fty1 ** fty2
    val tabFPair = "tabFPair" <: domTy --> rngTy
    val n = "n" <: B.intTy
    val f = "f" <: B.intTy --> outTy1 ** outTy2
    val r1 = "r1" <: rty1
    val r2 = "r2" <: rty2
    val s = "s" <: shapeTy
    val bindS = s <- AU.mkApplyExp (dcon shapeLf, [AU.mkInt 0, vexp n])
    val result = AU.mkTupleExp [AU.mkApplyExp (dcon fdcon1, [vexp r1, vexp s]),
				AU.mkApplyExp (dcon fdcon2, [vexp r2, vexp s])]
    val body = AU.mkLetExp ([bindS], tupBind ([r1, r2], rtab @@< [n, f], result))
    val lam = AU.mkFunWithParams (tabFPair, [n, f], body)
    in
      {seqPair = seqPair,
       ropePair = ropePair,
       farrayPair = lam}
    end

end
