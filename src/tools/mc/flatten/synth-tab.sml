(* synth2-tab.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure SynthTab = struct

(* tabs of pairs *)
(* 2D tabs *)

  structure A = AST
  structure T = Types
  structure B = Basis
  structure D = DelayedBasis

  structure AU = ASTUtil
  structure FU = FlattenUtil
  structure TU = TypeUtil

  structure DD  = D.DataCon
  structure DV  = D.Var
  structure DTy = D.Ty

  fun todo () = raise Fail "todo"
  fun assert msg test = if test then () else raise Fail msg

  fun dup (n, x) = List.tabulate (n, fn _ => x)

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

  infix 1 ?
  fun test ? (tru, fls) = AU.mkIfExp (test, tru, fls)

  val ptup = AU.mkPTupleExp

  fun seqTy t =
    if FU.isInt t then DTy.int_seq ()
    else if FU.isDouble t then DTy.dbl_seq ()
    else (print ("seq for " ^ TU.toString t ^ "\n"); DTy.arr_seq t)

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
      if FU.isInt t then 
        (DD.intLeaf (), DV.irTab (), DV.irLength (), 
	 DV.irConcat (), DV.irEmpty (), ty)
      else if FU.isDouble t then
        (DD.dblLeaf (), DV.drTab (), DV.drLength (), 
	 DV.drConcat (), DV.drEmpty (), ty)
      else
        (DD.ropeLeaf (), DV.ropeTab (), DV.ropeLength (), 
	 DV.ropeConcat (), DV.ropeEmpty (), ty)
    end

  fun farrayTy t =
    if FU.isInt t then
      DTy.int_farray ()
    else if FU.isDouble t then
      DTy.dbl_farray ()
    else
      DTy.farray t

  fun farray t = let
    val ty = farrayTy t
    in
      if FU.isInt t then
        (DD.intFArray (), ty)
      else if FU.isDouble t then
        (DD.dblFArray (), ty)
      else
        (DD.farray (), ty)
    end

  fun shape () = (DD.lf (), DD.nd (), DTy.shape ())

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
    val tabFBody = test ? (thenBranch, elseBranch)
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
      (R1.concat (r1L, r1R), R2.concat (r2L, r2R))))
*)
  fun mkRCat (ropeTy1, ropeTy2, cat1, cat2) = let
    val domTy = (ropeTy1 ** ropeTy2) ** (ropeTy1 ** ropeTy2)
    val rngTy = ropeTy1 ** ropeTy2
    val rcat = "rcat" <: domTy --> rngTy
    val rsL = "rsL" <: ropeTy1 ** ropeTy2
    val rsR = "rsR" <: ropeTy1 ** ropeTy2
    val r1L = "r1L" <: ropeTy1
    val r2L = "r2L" <: ropeTy2
    val r1R = "r1R" <: ropeTy1
    val r2R = "r2R" <: ropeTy2
    val result = AU.mkTupleExp [cat1 @@< [r1L, r1R], cat2 @@< [r2L, r2R]]
    val body = AU.mkCaseExp (vexp rsL, 
      [A.PatMatch (varsPat [r1L, r2L], AU.mkCaseExp (vexp rsR,
        [A.PatMatch (varsPat [r1R, r2R], result)]))])
    val lam = AU.mkFunWithParams (rcat, [rsL, rsR], body)
    in
      lam
    end

(*
fun tabFromToP (lo, hi, f) = let
  fun rcat ... (* see above *)
  in
    if (lo > hi) then (R1.empty (), R2.empty ())
    else let
      val nElts = hi-lo+1
      in
        if (LeafSize.getMax () > nElts) then let
          val (s1, s2) = seqPair (lo, hi, f)
          in
            (R1.Leaf s1, R2.Leaf s2)
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
    val (lf1, rtab1, len1, cat1, empty1, ropeTy1) = rope outTy1
    val (lf2, rtab2, len2, cat2, empty2, ropeTy2) = rope outTy2
    val seqPairLam as A.FB (seqPair, _, _) = mkSPair (outTy1, outTy2)
    val rcatLam as A.FB (rcat, _, _) = 
      mkRCat (ropeTy1, ropeTy2, cat1, cat2)
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
    val getMax = AU.mkForce (vexp (DV.maxLeafSize ()))
    val if1Test = AU.intGT (vexp lo, vexp hi)
    val if1Then = AU.mkTupleExp (map (AU.mkForce o vexp) [empty1, empty2])
    val if2Test = AU.intGT (getMax, vexp nElts)
    val if2Then = tupBind ([s1, s2], 
			   seqPair @@< [lo, hi, f],
			   AU.mkTupleExp [lf1 @@- [s1], lf2 @@- [s2]])
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
      if2Test ? (if2Then, if2Else))
    val body = AU.mkLetExp ([A.FunBind [rcatLam]], if1Test ? (if1Then, if1Else))
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
    val body = tFTP @@ [AU.zero, AU.minusOne (vexp n), vexp f]
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
	        then (R1.empty (), R2.empty ())
              else
                tabFromToP (0, (from-to_) div (~step), f'))
      else if (step > 0)
        then (if (from > to_)
	        then (R1.empty (), R2.empty ())
	      else
                tabFromToP (0, (to_ - from) div step, f'))
      else raise Fail "0 step"
    end
 *)
  fun mkTabFromToStepP (outTy1, outTy2) = let
    val {seqPair, tabFromToP} = mkTabFromToP (outTy1, outTy2)
    val A.FB (tFTP, _, _) = tabFromToP
    val (_, _, _, _, empty1, ropeTy1) = rope outTy1
    val (_, _, _, _, empty2, ropeTy2) = rope outTy2
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
    val emptyPair = AU.mkTupleExp (List.map (AU.mkForce o vexp) [empty1, empty2])
    val test1 = AU.intLT (vexp step, AU.zero)
    val test2 = AU.intGT (vexp to_, vexp from)
    val test3 = AU.intGT (vexp step, AU.zero)
    val test4 = AU.intGT (vexp from, vexp to_)
    val innerIf1 = let
      val arith = AU.intDiv (AU.minus (vexp from) (vexp to_), 
			     AU.intNeg (vexp step))
      val tapp = tFTP @@ [AU.zero, arith, vexp f']
      in
        test2 ? (emptyPair, tapp)
      end
    val innerIf2 = let
      val arith = AU.intDiv (AU.minus (vexp to_) (vexp from), vexp step)
      val tapp = tFTP @@ [AU.zero, arith, vexp f']
      in
	test4 ? (emptyPair, tapp)
      end
    val outerIf = test1 ? (innerIf1, test3 ? (innerIf2, AU.mkFail ("0 step", rngTy)))
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
    val shapeTy = DTy.shape ()
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
    val (_, _, len1, _, _, ropeTy1) = rope outTy1
    val (_, _, _, _, _, ropeTy2) = rope outTy2
    val shapeTy = DTy.shape ()
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

  (* synthesize monomorphic 2d tabulation *)

(* In the code that follows, it's important that the seq used by the rope has 
 * fast creation and fast update. Otherwise the benefit will be limited.
 *)

(*
  fun tab2D = (iFrom, iTo, iStep, jFrom, jTo, jStep, f) = let
    val width = Range.nElts (iFrom, iTo, iStep)
    val height = Range.nElts (jFrom, jTo, jStep)
    fun f' k = let
      val i = iFrom + (k div width) * iStep
      val j = jFrom + (k mod width) * jStep
      in
        f (i, j)
      end
    val rope = Rope.tabulate (width * height, f')
    fun lf n = Lf (n * width, (n + 1) * width)
    if
      if (width<0) then raise Fail "width<0"
      else if (height<0) then raise Fail "height<0"
      else FArray (rope, Nd (List.tabulate (height, lf)))
    end
*)

(* Note: The generated function will consume a function of some type *)
(*   int * int -> tau *)
(* The argument "resultTy" here is the name given that tau. *)
(* It's the only one you need to build everything else. *)
  fun mkTab2D resultTy = let
    val _ = case resultTy of T.TupleTy _ => raise Fail "tupleTy" | _ => ()
    (* gather basis items *)
    val (_, rtab, _, _, _, ropeTy) = rope resultTy
    val (fdcon, farrayTy) = farray resultTy
    val (shapeLf, shapeNd, shapeTy) = shape ()
    val nElts = DV.rngNElts ()
    (* calculate types *)
    val fTy = B.intTy ** B.intTy --> resultTy
    val domTy = T.TupleTy [B.intTy, B.intTy, B.intTy, B.intTy, B.intTy, B.intTy, fTy]
    val rngTy = farrayTy
    (* make the variables *)
    val tab2D = "tab2D" <: domTy --> rngTy
    val iFrom = "iFrom" <: B.intTy
    val iTo = "iTo" <: B.intTy
    val iStep = "iStep" <: B.intTy
    val jFrom = "jFrom" <: B.intTy
    val jTo = "jTo" <: B.intTy
    val jStep = "jStep" <: B.intTy
    val f = "f" <: fTy
    val width = "width" <: B.intTy
    val height = "height" <: B.intTy
    val rope = "rope" <: ropeTy
    (* generate function f' *)
    val f' = "f'" <: B.intTy --> resultTy
    val k = "k" <: B.intTy
    val i = "i" <: B.intTy
    val j = "j" <: B.intTy
    val f'Body = let
      val iBind = i <- AU.plus (vexp iFrom) 
		               (AU.times (vexp iStep) 
					 (AU.intDiv (vexp k, vexp width)))
      val jBind = j <- AU.plus (vexp jFrom) 
		               (AU.times (vexp jStep) 
					 (AU.intMod (vexp k, vexp width)))
      in
        AU.mkLetExp ([iBind, jBind], f @@< [i, j])
      end 
    val f'Lam = AU.mkFunWithParams (f', [k], f'Body)
    (* generate function lf *)
    val lf = "lf" <: B.intTy --> shapeTy
    val n = "n" <: B.intTy
    val lfBody = let
      val nw = AU.times (vexp n) (vexp width)
      val nw' = AU.times (AU.plusOne (vexp n)) (vexp width)
      in
        AU.mkApplyExp (dcon shapeLf, [nw, nw'])
      end
    val lfLam = AU.mkFunWithParams (lf, [n], lfBody)
    (* generate everything else *)
    val widthBind = width <- nElts @@< [iFrom, iTo, iStep]
    val heightBind = height <- nElts @@< [jFrom, jTo, jStep]
    val widthTest = AU.intLT (vexp width, AU.zero)
    val widthFail = AU.mkFail ("width<0", rngTy)
    val heightTest = AU.intLT (vexp height, AU.zero)
    val heightFail = AU.mkFail ("height<0", rngTy)
    val ropeBind = let
      val n = AU.times (vexp width) (vexp height)
      in
        rope <- (rtab @@ [n, vexp f'])
      end
    val result = let
      val listTab = A.VarExp (DV.listTab (), [shapeTy])
      val app = AU.mkApplyExp (listTab, [vexp height, vexp lf])
      val nd = AU.mkApplyExp (dcon shapeNd, [app])
      in
        AU.mkApplyExp (dcon fdcon, [vexp rope, nd])
      end
    val tab2DBody = let
      fun fb lam = A.FunBind [lam]
      val binds = [widthBind, heightBind, fb f'Lam, ropeBind, fb lfLam]
      in
        AU.mkLetExp (binds, widthTest ? (widthFail, 
          heightTest ? (heightFail, result)))
      end	       
    val tab2DLam = let
      val params = [iFrom, iTo, iStep, jFrom, jTo, jStep, f]
      in
        AU.mkFunWithParams (tab2D, params, tab2DBody)
      end
    in
      tab2DLam
    end

(* selectMapMaker : int -> var *)
  fun selectMapMaker 2 = DV.ixMap2D ()
    | selectMapMaker 3 = DV.ixMap3D ()
    | selectMapMaker 4 = DV.ixMap4D ()
    | selectMapMaker 5 = DV.ixMap5D ()
    | selectMapMaker n = raise Fail ("selectMapMaker " ^ Int.toString n)

(* 
  fun tabHD ((t1),...,(td),f) = let
    val map = PArrayUtil.mapdD ((t1),...,(td))
    fun f' k = f (map k)
    val nElts = Range.nElts t1 * ... * Range.nElts td
    val data = ${MONO}Rope.tabulate (nElts, f')
    val shape = Shape.regularShape [t1,...td]
    in
      ${MONO}FArray (data, shape)
    end
*)
  fun mkTabHD (dim : int, eltTy : T.ty) = let
    val _ = (case eltTy of T.TupleTy _ => raise Fail "todo: tup ty" | _ => ())
    val _ = if (dim<2) orelse (dim>5) then 
              raise Fail ("dim " ^ Int.toString dim ^ " not yet supported for regular pcomps")
	    else ()
    (* gather basis items *)
    val (_, rtab, _, _, _, ropeTy) = rope eltTy
    val (fdcon, farrayTy) = farray eltTy
    val (shapeLf, shapeNd, shapeTy) = shape ()
    val mkMap = selectMapMaker dim
    val shapeReg = DV.shapeRegular ()
    val nEltsFn = DV.rngNElts ()
    (* types *)
    val tripleTy = T.TupleTy [B.intTy, B.intTy, B.intTy]
    val fTy = T.TupleTy (dup (dim, B.intTy)) --> eltTy
    val domTy = T.TupleTy (dup (dim, tripleTy) @ [fTy])
    val rngTy = farrayTy
    (* variables *)
    val tabHD = ("tab" ^ Int.toString dim ^ "D") <: domTy --> rngTy
val _ = print (concat ["***** building ", Var.nameOf tabHD, "\n"])
    val ts = List.tabulate (dim, fn i => Var.new ("t" ^ Int.toString (i+1), tripleTy))
    val f = "f" <: fTy
    val map = "map" <: B.intTy --> T.TupleTy (dup (dim, B.intTy))
    val nElts = "nElts" <: B.intTy
    val data = "data" <: ropeTy
    val shape = "shape" <: shapeTy
    (* make the function f' *)
    val f' = "f'" <: B.intTy --> eltTy
    val k = "k" <: B.intTy
    val f'body = f @@ [map @@< [k]]
    val f'lam = AU.mkFunWithParams (f', [k], f'body)
    (* make everything else *)
    val mapBind = map <- (mkMap @@< ts)
    val nEltsBind = let
      fun lp [t] = nEltsFn @@< [t]
	| lp (t::ts) = AU.times (nEltsFn @@< [t]) (lp ts)
	| lp [] = raise Fail "bug"
      in
        nElts <- (lp ts)
      end
    val dataBind = data <- rtab @@< [nElts, f']
    val shapeBind = shape <- shapeReg @@ [AU.mkList (List.map vexp ts, tripleTy)]
    val farray = fdcon @@- [data, shape]
    val binds = [mapBind, A.FunBind [f'lam], nEltsBind, dataBind, shapeBind]
    val tabHDBody = AU.mkLetExp (binds, farray)
    val tabHDLam = AU.mkFunWithParams (tabHD, ts @ [f], tabHDBody)
    in
      tabHDLam
    end

end



(* old, but working, implementation of tab2D

(*
  fun tab2D = (iFrom, iTo, iStep, jFrom, jTo, jStep, f) = let
    fun f' (i, j) = f (iFrom + i * iStep, jFrom + j * jStep)
    val width = Range.nElts (iFrom, iTo, iStep)
    val height = Range.nElts (jFrom, jTo, jStep)
    val seq = Seq.create (width * height)
    fun fillCols (r, c) = 
      if (c >= width) then ()
      else let
        val index = r * width + c
        in
          Seq.update (seq, index, f' (r, c));
          fillCols (r, c+1)
        end
    fun fillRows z = 
      if (z >= height) then ()
      else (fillCols (z, 0); fillRows (z+1))
    val rope = (fillRows 0; Rope.fromSeq seq)
    fun lf n = Lf (n*width, n*width+width)
    in
      if (width<0) then raise Fail "width<0"
      else if (height<0) then raise Fail "height<0"
      else FArray (rope, Nd (List.tabulate (height, lf)))
    end
*)

(* Note: The generated function will consume a function of some type *)
(*   int * int -> tau *)
(* The argument "resultTy" here is the name given that tau. *)
(* It's the only one you need to build everything else. *)
  fun mkTab2D resultTy = let
    val _ = case resultTy of T.TupleTy _ => raise Fail "tupleTy" | _ => ()
    (* gather basis items *)
    val (seqUpd, seqCreate, seqTy) = seq resultTy
    val (_, ropeFromSeq, _, _, _, ropeTy) = rope resultTy
    val (fdcon, farrayTy) = farray resultTy
    val (shapeLf, shapeNd, shapeTy) = shape ()
    val nElts = DV.rngNElts ()
    (* calculate types *)
    val fTy = B.intTy ** B.intTy --> resultTy
    val domTy = T.TupleTy [B.intTy, B.intTy, B.intTy, B.intTy, B.intTy, B.intTy, fTy]
    val rngTy = farrayTy
    (* make the variables *)
    val tab2D = "tab2D" <: domTy --> rngTy
    val iFrom = "iFrom" <: B.intTy
    val iTo = "iTo" <: B.intTy
    val iStep = "iStep" <: B.intTy
    val jFrom = "jFrom" <: B.intTy
    val jTo = "jTo" <: B.intTy
    val jStep = "jStep" <: B.intTy
    val f = "f" <: fTy
    val width = "width" <: B.intTy
    val height = "height" <: B.intTy
    val seq = "seq" <: seqTy
    val rope = "rope" <: ropeTy
    (* generate function f' *)
    val f' = "f'" <: fTy
    val i = "i" <: B.intTy
    val j = "j" <: B.intTy
    val f'Body = f @@ [AU.plus (vexp iFrom) (AU.times (vexp i) (vexp iStep)),
		       AU.plus (vexp jFrom) (AU.times (vexp j) (vexp jStep))]
    val f'Lam = AU.mkFunWithParams (f', [i, j], f'Body)
    (* generate function fillCols *)
    val fillCols = "fillCols" <: B.intTy ** B.intTy --> B.unitTy
    val r = "r" <: B.intTy
    val c = "c" <: B.intTy
    val index = "index" <: B.intTy
    val cTest = AU.intGTE (vexp c, vexp width)
    val elseBranchC = let
      val arith = AU.plus (AU.times (vexp r) (vexp width)) (vexp c)
      val upd = seqUpd @@ [vexp seq, vexp index, f' @@< [r, c]]
      val call = fillCols @@ [vexp r, AU.plusOne (vexp c)]
      in
        AU.mkLetExp ([index <- arith], AU.mkSeqExp ([upd], call))
      end
    val fillColsBody = cTest ? (AU.unitExp, elseBranchC)
    val fillColsLam = AU.mkFunWithParams (fillCols, [r, c], fillColsBody)
    (* generate function fillRows *)
    val fillRows = "fillRows" <: B.intTy --> B.unitTy
    val z = "z" <: B.intTy    
    val zTest = AU.intGTE (vexp z, vexp height)
    val elseBranchR = let
      val callCols = fillCols @@ [vexp z, AU.zero]
      val callSelf = fillRows @@ [AU.plusOne (vexp z)]
      in
        AU.mkSeqExp ([callCols], callSelf)
      end
    val fillRowsBody = zTest ? (AU.unitExp, elseBranchR)
    val fillRowsLam = AU.mkFunWithParams (fillRows, [z], fillRowsBody)
    (* generate function lf *)
    val lf = "lf" <: B.intTy --> shapeTy
    val n = "n" <: B.intTy
    val lfBody = let
      val arith = AU.times (vexp n) (vexp width)
      in
        AU.mkApplyExp (dcon shapeLf, [arith, AU.plus arith (vexp width)])
      end
    val lfLam = AU.mkFunWithParams (lf, [n], lfBody)
    (* generate everything else *)
    val widthBind = width <- nElts @@< [iFrom, iTo, iStep]
    val heightBind = height <- nElts @@< [jFrom, jTo, jStep]
    val seqBind = seq <- (seqCreate @@ [AU.times (vexp width) (vexp height)])
    val ropeBind = rope <- AU.mkSeqExp ([fillRows @@ [AU.zero]], ropeFromSeq @@< [seq])
    val result = let
      val listTab = A.VarExp (DV.listTab (), [shapeTy])
      val app = AU.mkApplyExp (listTab, [vexp height, vexp lf])
      val nd = AU.mkApplyExp (dcon shapeNd, [app])
      in
        AU.mkApplyExp (dcon fdcon, [vexp rope, nd])
      end
    val widthTest = AU.intLT (vexp width, AU.zero)
    val widthFail = AU.mkFail ("width<0", rngTy)
    val heightTest = AU.intLT (vexp height, AU.zero)
    val heightFail = AU.mkFail ("height<0", rngTy)
    val tab2DBody = let
      fun fb lam = A.FunBind [lam]
      val binds = [fb f'Lam, widthBind, heightBind, seqBind, 
		   fb fillColsLam, fb fillRowsLam, ropeBind, fb lfLam]
      in
        AU.mkLetExp (binds, widthTest ? (widthFail,
          heightTest ? (heightFail, 
            result)))
      end	       
    val tab2DLam = let
      val params = [iFrom, iTo, iStep, jFrom, jTo, jStep, f]
      in
        AU.mkFunWithParams (tab2D, params, tab2DBody)
      end
    in
      tab2DLam
    end

*)
