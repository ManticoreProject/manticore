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

  infix 9 **
  fun t1 ** t2 = T.TupleTy [t1, t2]

  infixr 8 -->
  fun t1 --> t2 = T.FunTy (t1, t2)

  infix @@
  fun f @@ args = AU.mkApplyExp (vexp f, args)

  infix @@<
  fun f @@< xs = AU.mkApplyExp (vexp f, List.map vexp xs)

  infix <-
  fun x <- e = A.ValBind (vpat x, e)

  infix 1 <:
  fun x <: t = Var.new (x, t)

  fun seq t = 
    if FU.isInt t then
      (DTy.int_seq (), DV.iseqUpd (), DV.iseqCreate ())
    else if FU.isDouble t then
      (DTy.dbl_seq (), DV.dseqSub (), DV.dseqCreate ())
    else
      (DTy.arr_seq t, DV.arrSeqSub (), raise Fail "todo")

  fun rope t =
    if FU.isInt t then
      (DD.intLEAF (), DD.intCAT (), DV.irEmpty (), DTy.int_rope ())
    else if FU.isDouble t then
      (DD.dblLEAF (), DD.dblCAT (), DV.drEmpty (), DTy.dbl_rope ())
    else
      (DD.ropeLEAF (), DD.ropeCAT (), DV.ropeEmpty (), DTy.rope t)

  fun farray t =
    if FU.isInt t then
      (DD.intFArray (), DTy.int_farray ())
    else if FU.isDouble t then
      (DD.dblFArray (), DTy.dbl_farray ())
    else
      (DD.farray (), DTy.farray t)

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
        lp (i+1)
      end
  in
    lp 0
  end      
 *)

(* 
fun tabRPair (n, f) = let
  val (s1, s2) = tabSPair (n, f)
  in
    (R1.fromSeq s1, R2.fromSeq s2)
  end   
 *)
fun mkRPair _ = raise Fail "todo"
(* 
fun tabFPair (n, f) = let
  val rs = tabRPair (n, f)
  val s = Shape.Lf (0, n)
  in case rs
    of (r1, r2) => (F1.FArray (r1, s), F2.FArray (r2, s))
  end
 *)

  fun mkFPair (outTy1, outTy2) = let
    fun dcon c = A.ConstExp (A.DConst (c, []))
    val (fdcon1, fty1) = farray outTy1
    val (fdcon2, fty2) = farray outTy2
    val (_, _, _, rty1) = rope outTy1
    val (_, _, _, rty2) = rope outTy2
    val shapeTy = DTy.shape_tree ()
    val shapeLf = DD.lf ()
    val {seqPair, ropePair} = mkRPair (outTy1, outTy2)
    val A.FB (rtab, _, _) = ropePair
    val domTy = B.intTy ** (B.intTy --> (outTy1 ** outTy2))
    val rngTy = fty1 ** fty2
    val tabFPair = "tabFPair" <: domTy --> rngTy
    val n =        "n"        <: B.intTy
    val f =        "f"        <: B.intTy --> outTy1 ** outTy2
    val rs =       "rs"       <: rty1 ** rty2
    val r1 =       "r1"       <: rty1
    val r2 =       "r2"       <: rty2
    val s =        "s"        <: shapeTy
    val bindRs = rs <- (rtab @@< [n, f])
    val bindS = s <- AU.mkApplyExp (dcon shapeLf, [AU.mkInt 0, vexp n])
    val result = AU.mkTupleExp [AU.mkApplyExp (dcon fdcon1, [vexp r1, vexp s]),
				AU.mkApplyExp (dcon fdcon1, [vexp r2, vexp s])]
    val body = AU.mkCaseExp (vexp rs,
      [A.PatMatch (varsPat [r1, r2], AU.mkLetExp ([bindRs, bindS], result))])
    val lam = AU.mkFunWithParams (tabFPair, [n, f], body)
    in
      {seqPair = seqPair,
       ropePair = ropePair,
       farrayPair = lam}
    end

end
