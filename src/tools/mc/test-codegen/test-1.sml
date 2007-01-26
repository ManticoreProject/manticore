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

    fun var (name, ty) = CFG.Var.new(Atom.atom name, CFG.VK_None, ty)
    fun newLab (lStr, ty) = CFG.Label.new(Atom.atom lStr, CFG.Local, ty)

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
    fun label lab x = CFG.mkLabel(x, lab)
    fun select (i, y) x = CFG.mkSelect(x, i, y)

  (* prim ops *)
    fun lte (a, b) x = CFG.mkPrim(x, P.I32Lte(a, b))
    fun eq (a, b) x = CFG.mkPrim(x, P.I32Eq(a, b))
    fun add (a, b) x = CFG.mkPrim(x, P.I32Add(a, b))
    fun sub (a, b) x = CFG.mkPrim(x, P.I32Sub(a, b))

  (* some types *)
    val iTy = Ty.T_Raw Ty.T_Int
    val bTy = Ty.T_Bool
    val aTy = Ty.T_Any

  fun intLit i = Literal.Int i
  fun mkVar (vStr, k) = var(vStr, M.T_Any)
  fun mkLabel lStr = Label.label lStr ()

  fun t outFile = 
      let val t = newLab ("mantentry", M.T_Any)
	  fun mkVarParam v = (v, M.T_Any)
	  val vs = [mkVarParam "cl", mkVarParam "arg", mkVarParam "ret", 
		    mkVarParam "exh"]
		  
	  fun bodyFn [clos, arg, ret, exh] =
	      mkLet (M.T_Any, lit 1024, fn il =>
	        mkLet (M.T_Any, alloc [il], fn lt =>
		  mkLet (M.T_Any, label t, fn f =>					     
  		     mkExit (M.StdThrow {k=ret, clos=clos, arg=il}))))
      in 	  
	  compile (M.MODULE {code=[func (t, vs, bodyFn)], funcs=LM.empty}, outFile) 
      end (* t *)

end
