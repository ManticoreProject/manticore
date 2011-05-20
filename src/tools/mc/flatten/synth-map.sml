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

  val vpat = A.VarPat
  fun pairPat (x, y) = A.TuplePat [vpat x, vpat y]
  fun varsPat xs = A.TuplePat (List.map vpat xs)

  fun vexp x = A.VarExp (x, [])

  infix 5 **
  fun t1 ** t2 = T.TupleTy [t1, t2]

  infixr 4 -->
  fun t1 --> t2 = T.FunTy (t1, t2)

  infix @@
  fun f @@ args = AU.mkApplyExp (vexp f, args)

  infix @@<
  fun f @@< xs = AU.mkApplyExp (vexp f, List.map vexp xs)

  infix <-
  fun x <- e = A.ValBind (vpat x, e)

  infix 1 <:
  fun x <: t = Var.new (x, t)

  fun tupBind (xs, tupExp, exp) = let
    val tup = "tup" <: TypeOf.exp tupExp
    in
      AU.mkLetExp ([tup <- tupExp],
        AU.mkCaseExp (vexp tup, [A.PatMatch (varsPat xs, exp)]))
    end

  val ptup = AU.mkPTupleExp

  fun seq t = 
    if FU.isInt t then
      (DTy.int_seq (), DV.iseqSub (), DV.iseqTab (), DV.iseqUpd (), DV.iseqEmpty ())
    else if FU.isDouble t then
      (DTy.dbl_seq (), DV.dseqSub (), DV.dseqTab (), DV.dseqUpd (), DV.dseqEmpty ())
    else
      (DTy.arr_seq t, DV.arrSeqSub (), DV.arrSeqTab (), DV.arrSeqUpd (), DV.arrSeqEmpty ())

  fun ropeTy t = 
    if FU.isInt t then DTy.int_rope ()
    else if FU.isDouble t then DTy.dbl_rope ()
    else DTy.rope t

  fun rope t = let
    val ty = ropeTy t
    in
      if FU.isInt t then
        (DD.intLeaf (), DD.intCat (), DV.irLength (), DV.irEmpty (), ty)
      else if FU.isDouble t then
        (DD.dblLeaf (), DD.dblCat (), DV.drLength (), DV.drEmpty (), ty)
      else
        (DD.ropeLeaf (), DD.ropeCat (), DV.ropeLength (), DV.ropeEmpty (), ty)
    end

  fun farray t =
    if FU.isInt t then
      (DD.intFArray (), DTy.int_farray ())
    else if FU.isDouble t then
      (DD.dblFArray (), DTy.dbl_farray ())
    else
      (DD.farray (), DTy.farray t)

(* 
  fun seqMap (f, len, s1, s2) = let
     val f' i = f (S1.sub (s1, i), S2.sub (s2, i))
     in
       S3.tabulate (len, f')
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
    val f'     = "f'"     <: B.intTy --> outTy
    val i      = "i"      <: B.intTy
    val f'Body = f @@ [sub1 @@< [s1, i], sub2 @@< [s2, i]]
    val f'Lam = AU.mkFunWithParams (f', [i], f'Body)
    val f'Bind = A.FunBind [f'Lam]
    val seqMapBody = AU.mkLetExp ([f'Bind], tab3 @@< [len, f'])
    val seqMapLam = AU.mkFunWithParams (seqMap, [f, len, s1, s2], seqMapBody)
    in
      seqMapLam
    end

(* NOTE: I need to introduce simple patterns only, as I am downstream of match-compile.
  fun ropeMap f = let
    fun mapF (rope1, rope2) = (case rope1
      of R1.LEAF s1 => (case rope2
           of R2.LEAF s2 => let 
                val n = R1.length rope1
                in 
                  if (n < 1) then R3.empty ()
                  else R3.mkLeaf (s_map_int_dbl (f, n, s1, s2))
	        end)
       | R1.CAT cat1 => (case cat1
           of (d1, len1, r1L, r1R) => (case rope2
	        of R2.CAT cat2 => (case cat2
	             of (_, _, r2L, r2R) =>
	                  R3.CAT (| d1, len1, mapF (r1L, r2L), mapF (r1R, r2R) |))))
      (* end case *))
    in
      mapF
    end
*)
  fun synthRopePairMap (inTy1, inTy2, outTy) = let
    val (inLeaf1, inCat1, inLen1, inE1, inRope1) = rope inTy1
    val (inLeaf2, inCat2, inLen2, inE2, inRope2) = rope inTy2
    val (outLeaf, outCat, outLen, outE, outRope) = rope outTy
    val (seqTy1, sub1, tab1, update1, empty1) = seq inTy1
    val (seqTy2, sub2, tab2, update2, empty2) = seq inTy2
    val (seqTy3, sub3, tab3, update3, empty3) = seq outTy
    val seqMapLam as A.FB (seqMap, _, _) = synthSeqPairMap (inTy1, inTy2, outTy)
    val domTy = inTy1 ** inTy2 --> outTy
    val rngTy = inRope1 ** inRope2 --> outRope
    val ropeMap = "ropeMap" <: domTy --> rngTy
    val f =       "f"       <: domTy
    val mapF =    "mapF"    <: rngTy
    val rope1 =   "rope1"   <: inRope1
    val rope2 =   "rope2"   <: inRope2
    val cat1 =    "cat1"    <: T.TupleTy [B.intTy, B.intTy, inRope1, inRope1]
    val cat2 =    "cat2"    <: T.TupleTy [B.intTy, B.intTy, inRope2, inRope2]
    val n =       "n"       <: B.intTy
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
    val lfRHS = 
      AU.mkLetExp ([n <- (inLen1 @@< [rope1])],
        AU.mkIfExp (AU.intLT (vexp n, AU.mkInt 1),
		    AU.mkForce (vexp outE),
		    outLEAF [seqMap @@< [f, n, s1, s2]]))
    val catRHS = ptup [vexp d1, vexp len1, mapF @@< [r1L, r2L], mapF @@< [r1R, r2R]]
    val mapFBody = AU.mkCaseExp (vexp rope1,
      [A.PatMatch (A.ConPat (inLeaf1, [], vpat s1), AU.mkCaseExp (vexp rope2,
        [A.PatMatch (A.ConPat (inLeaf2, [], vpat s2), 
          lfRHS)])),
       A.PatMatch (A.ConPat (inCat1, [], vpat cat1), AU.mkCaseExp (vexp cat1,
         [A.PatMatch (varsPat [d1, len1, r1L, r1R], AU.mkCaseExp (vexp rope2,
           [A.PatMatch (A.ConPat (inCat2, [], vpat cat2), AU.mkCaseExp (vexp cat2,
             [A.PatMatch (A.TuplePat [A.WildPat B.intTy, A.WildPat B.intTy, vpat r2L, vpat r2R],
	       catRHS)]))]))]))])
    val mapFLam = AU.mkFunWithParams (mapF, [rope1, rope2], mapFBody)
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
    val inRope1 = ropeTy inTy1
    val inRope2 = ropeTy inTy2
    val outRope = ropeTy outTy
    val (fdcon1, fTy1) = farray inTy1
    val (fdcon2, fTy2) = farray inTy2
    val (fdcon3, fTy3) = farray outTy
    val shapeTy = DTy.shape ()
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
        [A.PatMatch (A.ConPat (fdcon1, [], vpat f1), AU.mkCaseExp (vexp f1,
          [A.PatMatch (varsPat [d1, s1], AU.mkCaseExp (vexp farray2,
            [A.PatMatch (A.ConPat (fdcon2, [], vpat f2), AU.mkCaseExp (vexp f2,
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
