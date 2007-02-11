structure T = struct

  structure AMD64TargetSpec = AMD64TargetSpecFn (
			      val abiName = "SVID"
			      val osName = "linux" )
  structure AMD64CG = AMD64GenFn (structure Spec = AMD64TargetSpec)

  structure M = CFG
  structure V = M.Var
  structure AM = AtomMap
  structure LM = M.Label.Map
  structure BE = AMD64CG.BackEnd
  structure Instr = BE.MLTreeComp.I
  structure Cells = Instr.C
  structure LC = BE.LabelCode
  structure P = Prim
  structure Ty = CFGTy

  val ty = (Word.toInt BE.Spec.wordSzB) * 8

  fun compile (cfg, outFile) =
      let val outStrm = TextIO.openOut outFile
	  fun doit () = AMD64CG.Gen.codeGen {dst=outStrm, code=cfg}
      in	  
	  AsmStream.withStream outStrm doit ();
	  TextIO.closeOut outStrm
      end (* compile *)

    fun var (name, ty) = CFG.Var.newWithKind(Atom.atom name, CFG.VK_None, ty)
    fun newGlobal (lStr, ty) = CFG.Label.newWithKind(Atom.atom lStr, CFG.Export lStr, ty)
    fun newLab (lStr, ty) = CFG.Label.newWithKind(Atom.atom lStr, CFG.Local, ty)
    fun freshLab ty = CFG.Label.newWithKind(Atom.atom "L", CFG.Local, ty)

    fun func (lab, params, bodyFn) = let
	  val params as [clos, arg, ret, exh] = List.map var params
	  val (body, exit) = bodyFn params
	  in	    
	    CFG.mkFunc (lab, CFG.StdFunc{clos=clos, arg=arg, ret=ret, exh=exh}, body, exit)
	  end

    fun cont (lab, params, bodyFn) = let
	  val params as [clos, arg] = List.map var params
	  val (body, exit) = bodyFn params
	  in
	    CFG.mkFunc (lab, CFG.StdCont{clos=clos, arg=arg}, body, exit)
	  end

    fun known (lab, params, bodyFn) = let
	  val params = List.map var params
	  val (body, exit) = bodyFn params
	  in
	    CFG.mkFunc (lab, CFG.KnownFunc params, body, exit)
	  end

    fun xbb (lab, params, bodyFn) = let
	  val params = List.map var params
	  val (body, exit) = bodyFn params
	  in
	    CFG.mkFunc (lab, CFG.Block params, body, exit)
	  end

    fun mkLet (ty, rhs, bodyFn) = let
	  val tmp = var ("_t", ty)
	  val (code, xfer) = bodyFn tmp
	  in
	    ((rhs tmp) :: code, xfer)
	  end

    fun mkExit xfer = ([], xfer)

    fun lit i x = CFG.mkLiteral(x, Literal.Int i)
    fun alloc args x = CFG.mkAlloc(x, args)
    fun enum arg x = CFG.mkEnum(x, arg)
    fun label lab x = CFG.mkLabel(x, lab)
    fun select (i, y) x = CFG.mkSelect(x, i, y)

  (* prim ops *)
    fun lte (a, b) x = CFG.mkPrim(x, P.I64Lte(a, b))
    fun eq (a, b) x = CFG.mkPrim(x, P.I64Eq(a, b))
    fun add (a, b) x = CFG.mkPrim(x, P.I64Add(a, b))
    fun sub (a, b) x = CFG.mkPrim(x, P.I64Sub(a, b))

  (* some types *)
    val iTy = Ty.T_Raw Ty.T_Long
    val aTy = Ty.T_Any
    val f2aiTy = Ty.T_Code[aTy, iTy]	(* (_ * int) code *)
    val tif2aiaTy = Ty.T_Tuple[iTy, f2aiTy, aTy]
    val tif2aiaaTy = Ty.T_Tuple[iTy, f2aiTy, aTy, aTy]
    val wIntTy = Ty.T_Tuple [Ty.T_Raw Ty.T_Long]
    val listTy = Ty.T_Tuple [aTy, aTy]

  fun intLit i = Literal.Int i
  fun mkVar (vStr, k) = var(vStr, M.T_Any)
  fun mkLabel lStr = Label.label lStr ()

  fun t outFile = 
      let val t = newGlobal ("begin", M.T_Any)
	  val consHCl = newGlobal ("consHCl", M.T_Any)
	  val consLl = newGlobal ("consLl", M.T_Any)
	  val consLEl = newGlobal ("consLEl", M.T_Any)
	  val consLLl = newGlobal ("consLLl", M.T_Any)
	  fun mkVarParam v = (v, M.T_Any)
	  val vs = [mkVarParam "cl", mkVarParam "arg", ("k", f2aiTy),
		    mkVarParam "exh"]

	  val kL = freshLab aTy
	  val L2 = freshLab aTy

	  fun consLt l mty = xbb (l, [("k", f2aiTy), ("cl", aTy), 
				    ("x", iTy), ("xs", mty)], 
			fn [k, cl, x, xs] =>
			mkLet(aTy, alloc [x, xs], fn t =>
			      mkExit(CFG.StdThrow{k=k, arg=t, clos=cl})))
	  val consLE = consLt consLEl Ty.unitTy
	  val consLF = consLt consLLl listTy

	  val consL = xbb (consLl, [("k", f2aiTy), ("cl", aTy), 
				    ("x", iTy), ("xs", aTy)], 
			fn [k, cl, x, xs] =>
		mkLet (Ty.unitTy, enum 0w0, fn nilv =>
		mkLet (iTy, eq (nilv, xs), fn condi =>
		mkExit (M.If (condi, (consLEl, [k,cl,x,xs]), 
			      (consLLl, [k,cl,x,xs]))))))

	  val consHC = xbb (consHCl, [("k", f2aiTy), ("cl", aTy), 
				      ("x", iTy), ("xs", aTy)], 
			 fn [k, cl, x, xs] =>
			   mkExit (M.HeapCheck {szb=Word.* (0w8, 0w3), 
				gc=(consLl, [k, cl, x, xs]), 
				nogc=(consLl, [k, cl, x, xs])}))

	  val lsK = cont (kL, [("cl", f2aiTy), ("ls", wIntTy)], 
		       fn [cl, ls] =>
		mkLet (iTy, select (0, ls), fn x =>
		mkExit (M.StdThrow {k=cl, arg=ls, clos=cl})))

	  fun bodyFn [clos, arg, ret, exh] =
	      mkLet (iTy, lit 7, fn il1 =>
		mkLet (Ty.unitTy, enum 0w0, fn nilv =>
		mkExit (M.Goto (consHCl, [ret, clos, il1, nilv]))))

      in 	  
	  compile (M.MODULE {code=[
		func (t, vs, bodyFn), consLE, consLF, lsK, consL, consHC
				   ], funcs=LM.empty}, outFile) 
      end (* t *)

end
