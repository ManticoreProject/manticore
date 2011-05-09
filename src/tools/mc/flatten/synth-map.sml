(* synth-map.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure SynthMap = struct

  structure A = AST
  structure T = Types
  structure B = Basis
  structure D = DelayedBasis

  structure AU = ASTUtil
  structure FU = FlattenUtil

  structure DD  = D.DataCon
  structure DV  = D.Var
  structure DTy = D.Ty

  fun pairPat (x, y) = A.TuplePat [A.VarPat x, A.VarPat y]
  fun varsPat xs = A.TuplePat (List.map A.VarPat xs)

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
  fun x <- e = A.ValBind (A.VarPat x, e)

  infix 1 <:
  fun x <: t = Var.new (x, t)

  fun seq t = 
    if FU.isInt t then
      (DTy.int_seq (), DV.iseqSub (), DV.iseqTab (), DV.iseqUpd (), DV.iseqEmpty ())
    else if FU.isDouble t then
      (DTy.dbl_seq (), DV.dseqSub (), DV.dseqTab (), DV.dseqUpd (), DV.dseqEmpty ())
    else
      (DTy.arr_seq t, DV.arrSeqSub (), DV.arrSeqTab (), DV.arrSeqUpd (), DV.arrSeqEmpty ())

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
  fun seqMap (f, len, s1, s2) = let
     val s1_0 = S1.sub (s1, 0)  
     val s2_0 = S2.sub (s2, 0)
     val seq = S3.tabulate (len, f (s1_0, s2_0))
     fun lp i = 
       if (i >= len) then
         seq
       else let
         val x = f (S1.sub (s1, i), S2.sub (s2, i))
         in
           (S3.update (seq, i, x);
            lp (i+1))
         end
     in
       lp 0
     end
*)
  
  fun synthSeqPairMap (inTy1, inTy2, outTy) = let
    val (seqTy1, sub1, tab1, update1, empty1) = seq inTy1
    val (seqTy2, sub2, tab2, update2, empty2) = seq inTy2
    val (seqTy3, sub3, tab3, update3, empty3) = seq outTy
    val domTy = T.TupleTy [inTy1 ** inTy2 --> outTy, B.intTy, seqTy1, seqTy2]
    val rngTy = seqTy3 
    val seqMap = "seqMap" <: domTy --> rngTy
    val f      = "f"      <: inTy1 ** inTy2 --> outTy
    val len    = "len"    <: B.intTy
    val s1     = "s1"     <: seqTy1
    val s2     = "s2"     <: seqTy2
    val s1_0   = "s1_0"   <: inTy1
    val s2_0   = "s2_0"   <: inTy2
    val seq    = "seq"    <: seqTy3
    val lp     = "lp"     <: B.intTy --> seqTy3
    val i      = "i"      <: B.intTy
    val x      = "x"      <: outTy
    val lpElse = let
      val arg1 = sub1 @@< [s1, i]
      val arg2 = sub2 @@< [s2, i]
      val bind = x <- (f @@ [arg1, arg2])
      val upd = update3 @@< [seq, i, x]
      val body = A.SeqExp (upd, lp @@ [AU.plusOne (vexp i)])
      in
        AU.mkLetExp ([bind], body)
      end
    val lpBody = AU.mkIfExp (AU.intGTE (vexp i, vexp len), vexp seq, lpElse)
    val lpLam = AU.mkFunWithParams (lp, [i], lpBody)
    val s1_0Bind = s1_0 <- (sub1 @@ [vexp s1, AU.mkInt 0])
    val s2_0Bind = s2_0 <- (sub1 @@ [vexp s2, AU.mkInt 0])
    val applyF = f @@< [s1_0, s2_0]
    val seqBind = seq <- (tab3 @@ [vexp len, A.FunExp (Var.new ("x", B.intTy), applyF, outTy)])
    val lpBind = A.FunBind [lpLam]
    val seqMapBody = AU.mkLetExp ([s1_0Bind, s2_0Bind, seqBind, lpBind], lp @@ [AU.mkInt 0])
    val seqMapLam = AU.mkFunWithParams (seqMap, [f, len, s1, s2], seqMapBody)
    in
      seqMapLam
    end

(* NOTE: I need to introduce simple patterns only, as I am downstream of match-compile.
fun ropeMap f = let
  fun mapF ropes = (case ropes
    of (rope1, rope2) => (case rope1
         of R1.LEAF lf1 => (case lf1
           of (n1, s1) => (case rope2
             of R2.LEAF lf2 => (case lf2
               of (_, s2) =>
                 if (n1 < 1) then
                   R3.empty
                 else
                   R3.mkLeaf (s_map_int_dbl (f, n1, s1, s2)))))
	  | R1.CAT cat1 => (case cat1
              of (d1, len1, r1L, r1R) => (case rope2
                of R2.CAT cat2 => (case cat2
                  of (_, _, r2L, r2R) =>
                    R3.CAT (| d1, len1, mapF (r1L, r2L), mapF (r1R, r2R) |))))
         (* end case *))
    (* end case *))
  in
    mapF
  end
*)

  fun synthRopePairMap (inTy1, inTy2, outTy) = let
    val (inLeaf1, inCat1, inE1, inRope1) = rope inTy1
    val (inLeaf2, inCat2, inE2, inRope2) = rope inTy2
    val (outLeaf, outCat, outE, outRope) = rope outTy
    val (seqTy1, sub1, tab1, update1, empty1) = seq inTy1
    val (seqTy2, sub2, tab2, update2, empty2) = seq inTy2
    val (seqTy3, sub3, tab3, update3, empty3) = seq outTy
    val seqMapLam as A.FB (seqMap, _, _) = synthSeqPairMap (inTy1, inTy2, outTy)
    val domTy = inTy1 ** inTy2 --> outTy
    val rngTy = inRope1 ** inRope2 --> outRope
    val ropeMap = "ropeMap" <: domTy --> rngTy
    val f =       "f"       <: domTy
    val mapF =    "mapF"    <: rngTy
    val ropes =   "ropes"   <: inRope1 ** inRope2
    val rope1 =   "rope1"   <: inRope1
    val rope2 =   "rope2"   <: inRope2
    val lf1 =     "lf1"     <: B.intTy ** seqTy1
    val lf2 =     "lf2"     <: B.intTy ** seqTy2
    val cat1 =    "cat1"    <: T.TupleTy [B.intTy, B.intTy, inRope1, inRope1]
    val cat2 =    "cat2"    <: T.TupleTy [B.intTy, B.intTy, inRope2, inRope2]
    val n1 =      "n1"      <: B.intTy
    val s1 =      "s1"      <: seqTy1
    val s2 =      "s2"      <: seqTy2
    val d1 =      "d1"      <: B.intTy
    val len1 =    "len1"    <: B.intTy
    val r1L =     "r1L"     <: inRope1
    val r1R =     "r1R"     <: inRope1
    val r2L =     "r2L"     <: inRope2
    val r2R =     "r2R"     <: inRope2
    fun outLEAF es = AU.mkApplyExp (A.ConstExp (A.DConst (outLeaf, [])), es)
    fun outCAT es =  AU.mkApplyExp (A.ConstExp (A.DConst (outCat, [])), es)
    val lfRHS = AU.mkIfExp (AU.intLT (vexp n1, AU.mkInt 1),
			    vexp outE,
			    outLEAF [vexp n1, seqMap @@< [f, n1, s1, s2]])
    val catRHS = (* FIXME make this parallel *)
      outCAT [vexp d1, vexp len1, mapF @@< [r1L, r2L], mapF @@< [r1R, r2R]]
    val mapFBody = AU.mkCaseExp (vexp ropes,
      [A.PatMatch (varsPat [rope1, rope2], AU.mkCaseExp (vexp rope1,
        [A.PatMatch (A.ConPat (inLeaf1, [], A.VarPat lf1), AU.mkCaseExp (vexp lf1,
          [A.PatMatch (varsPat [n1, s1], AU.mkCaseExp (vexp rope2,
            [A.PatMatch (A.ConPat (inLeaf2, [], A.VarPat lf2), AU.mkCaseExp (vexp lf2,
              [A.PatMatch (A.TuplePat [A.WildPat B.intTy, A.VarPat s2], 
			   lfRHS)]))]))])),
	 A.PatMatch (A.ConPat (inCat1, [], A.VarPat cat1), AU.mkCaseExp (vexp cat1,
           [A.PatMatch (varsPat [d1, len1, r1L, r1R], AU.mkCaseExp (vexp rope2,
             [A.PatMatch (A.ConPat (inCat2, [], A.VarPat cat2), AU.mkCaseExp (vexp cat2,
               [A.PatMatch (A.TuplePat [A.WildPat B.intTy, A.WildPat B.intTy, A.VarPat r2L, A.VarPat r2R],
			    catRHS)]))]))]))]))])
    val mapFLam = AU.mkFunWithParams (mapF, [ropes], mapFBody)
    val ropeMapBody = AU.mkLetExp ([A.FunBind [mapFLam]], vexp mapF)
    val ropeMapLam = AU.mkFunWithParams (ropeMap, [f], ropeMapBody) 
    in
      {seqMap  = seqMapLam,
       ropeMap = ropeMapLam}
    end

(*
fun farrayMap f = let
  fun mapF farrays = (case farrays
    of (farray1, farray2) => (case farray1
      of F1.FArray f1 => (case f1
        of (d1, s1) => (case farray2
          of F2.FArray f2 => (case f2
            of (d2, s2)) =>
              if not(S.same(s1,s2)) then
                raise Fail "shape"
              else let
                val r = r_map f (s1, s2)
                in
                  F3.FArray (r, s1)             
                end))))
  in
    mapF
  end
*)

  fun synthFArrayMap (inTy1, inTy2, outTy) = let
    val _ = case outTy of T.TupleTy _ => raise Fail "todo: TupleTy" | _ => ()
    val (_, _, _, inRope1) = rope inTy1
    val (_, _, _, inRope2) = rope inTy2
    val (_, _, _, outRope) = rope outTy
    val (fdcon1, fTy1) = farray inTy1
    val (fdcon2, fTy2) = farray inTy2
    val (fdcon3, fTy3) = farray outTy
    val shapeTy = DTy.shape_tree ()
    val {seqMap, ropeMap} = synthRopePairMap (inTy1, inTy2, outTy)
    val A.FB (rmap, _, _) = ropeMap
    val domTy = inTy1 ** inTy2 --> outTy
    val rngTy = fTy1 ** fTy2 --> fTy3
    val farrayMap = "farrayMap" <: domTy --> rngTy
    val f =         "f"         <: domTy
    val mapF =      "mapF"      <: rngTy
    val farrays =   "farrays"   <: fTy1 ** fTy2
    val farray1 =   "farray1"   <: fTy1
    val farray2 =   "farray2"   <: fTy2
    val f1 =        "f1"        <: inRope1 ** shapeTy
    val f2 =        "f2"        <: inRope2 ** shapeTy
    val d1 =        "d1"        <: inRope1
    val s1 =        "s1"        <: shapeTy
    val d2 =        "d2"        <: inRope2
    val s2 =        "s2"        <: shapeTy
    val r =         "r"         <: outRope
    val notSame = AU.mkNot (DV.shapeSame() @@< [s1, s2])
    val bindR = r <- AU.mkCurriedApplyExp (vexp rmap, [vexp f, A.TupleExp [vexp d1, vexp d2]])
    val matchRHS = AU.mkIfExp (notSame,
			       AU.mkFail ("shape", fTy3),
			       AU.mkLetExp ([bindR],
					    AU.mkApplyExp (A.ConstExp (A.DConst (fdcon3, [])),
							   [vexp r, vexp s1])))
    val mapFBody = AU.mkCaseExp (vexp farrays,
      [A.PatMatch (varsPat [farray1, farray2], AU.mkCaseExp (vexp farray1,
        [A.PatMatch (A.ConPat (fdcon1, [], A.VarPat f1), AU.mkCaseExp (vexp f1,
          [A.PatMatch (varsPat [d1, s1], AU.mkCaseExp (vexp farray2,
            [A.PatMatch (A.ConPat (fdcon2, [], A.VarPat f2), AU.mkCaseExp (vexp f2,
              [A.PatMatch (varsPat [d2, s2], matchRHS)]))]))]))]))])
    val mapFLam = AU.mkFunWithParams (mapF, [farrays], mapFBody)
    val farrayMapBody = AU.mkLetExp ([A.FunBind [mapFLam]], vexp mapF)
    val farrayMapLam = AU.mkFunWithParams (farrayMap, [f], farrayMapBody)
    in
      {seqMap = seqMap,
       ropeMap = ropeMap,
       farrayMap = farrayMapLam}
    end

end
