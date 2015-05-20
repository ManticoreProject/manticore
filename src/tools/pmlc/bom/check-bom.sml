(* check-bom.sml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Check various invariants in the BOM representation.
 *)

structure CheckBOM : sig

  (* check the program and return true if there were any errors discovered.
   * The string argument is the name of the compiler phase that preceeds this
   * call to check.
   *)
    val check : string * BOM.program -> bool

  end = struct

    structure B = BOM
    structure BV = BOMVar
    structure VTbl = BV.Tbl
    structure VSet = BV.Set
    structure BTy = BOMTy
    structure DC = BOMDataCon

(* TODO: switch to new error printing *)
    val v2s = BV.toString
    fun vl2s xs = String.concat["(", String.concatWith "," (List.map v2s xs), ")"]

    fun pr s = TextIO.output(TextIO.stdErr, concat s)

  (* infrastructure for printing error messages *)
    datatype token
      = S of string | NL | A of Atom.atom
      | V of BV.t | VS of BV.t list
      | VTY of BV.t | VTYS of BV.t list
      | TY of BTy.t | TYS of BTy.t list

    fun err toks = let
	  fun vty (x, l) = BV.toString x :: ":" :: BTy.toString(BV.typeOf x) :: l
	  fun tok2str (tok, l) = (case tok
		 of S s => s :: l
		  | NL => (case l of [] => ["\n"] | _ => "\n== " :: l)
		  | A a => Atom.toString a :: l
		  | V x => BV.toString x :: l
		  | VS[] => "()" :: l
		  | VS xs => List.foldr (fn (x, l) => BV.toString x :: l) l xs
		  | VTY x => vty (x, l)
		  | VTYS[] => "()" :: l
		  | VTYS xs => List.foldr vty l xs
		  | TY ty => BTy.toString ty :: l
		  | TYS [] => "()" :: l
		  | TYS[ty] => BTy.toString ty :: l
		  | TYS(ty::tys) => "(" :: BTy.toString ty ::
			List.foldr (fn (ty, l) => ", " :: BTy.toString ty :: l) (")" :: l) tys
		(* end case *))
	  in
	    pr ("** " :: List.foldr tok2str [] toks)
	  end

    val debug = false

  (* for checking census counts *)
    structure ChkVC = CheckVarCountsFn (
      struct
	type var = B.var
	val useCntOf = BV.useCount
	val appCntOf = BV.appCntOf
	val toString = v2s
	structure Tbl = BV.Tbl
      end)

  (* placeholder for testing variable kind equality *)
    fun eqVK _ = true

    val vkToString = BV.kindToString

  (* the context of a BOM expression *)
    datatype context
      = TAIL of (B.var * B.ty list)
      | BIND of (B.var list * B.exp)

    fun typesOf xs = List.map BV.typeOf xs

  (* get the binding of a variable, chasing through casts and renamings *)
    fun resolveBinding x = let
          fun munchStmts (B.E_Pt (_, B.E_Stmt (_, _, e))) = munchStmts e
            | munchStmts (e) = e
	  fun lp x = (if debug
                      then print (concat["resolve: ", v2s x, " kind: ", vkToString (BV.kindOf x), "\n"])
                      else ();
                      case BV.kindOf x
		 of k as BV.VK_Let(e) => let
                        val inner = munchStmts e
                    in
                        case inner
                         of B.E_Pt (_, B.E_Ret [x]) => lp x
                          | _ => k
                    end
		  | BV.VK_RHS(B.E_Cast(_, y)) => lp y
		  | BV.VK_RHS(B.E_Select(i,y)) => lp y
		  | k => k
		(* end case *))
	  in
	    lp x
	  end

  (* check for assignments of unpromoted values; return true if okay and false
   * otherwise.
   * The following cases report true because they are currently uncheckable:
   * - VK_Param. We do not do data flow, so we do not know if the arguments were promoted
   * - VK_Let(E_Apply). We do not do data flow, so we do not know if the return values
   * of the called function(s) are guaranteed to have been promoted
   * - VK_Let(E_HLOp). We do not provide annotations on HLOPs as to whether their data is
   * promoted or not. HLOPs encountered will be expanded into full BOM later and can be
   * checked for promotion at that time.
   * - VK_None. Default var kind, and commonly comes up for language constructs like
   * pattern-bound variables in case statements where we don't have an appropriate VK.
   *)
(* FIXME: we should do promotion checking as a separate pass that uses flow analysis
    fun checkAssign (ty, x) = let
	  val k = BTy.kindOf ty
	  in
	    if (k = BTy.K_BOXED) orelse (k = BTy.K_UNIFORM)
	      then (case resolveBinding x
		 of BV.VK_RHS(B.E_Promote _) => true
		  | BV.VK_RHS(B.E_Const _) => true
		  | BV.VK_RHS(B_E_HostVProc) => true
                  | BV.VK_Param => true
                  | BV.VK_Let(B.E_Pt (_, B.E_Apply _)) => true
                  | BV.VK_Let(B.E_Pt (_, B.E_HLOp _)) => true
                  | BV.VK_None => true
		  | _ => false
		(* end case *))
	      else true
	  end
*)

    fun check (phase, program) = let
	  val B.PROGRAM{externs, hlops, body, ...} = program
	  val anyErrors = ref false
	  val anyWarnings = ref false
	(* report an error *)
	  fun error msg = (
		if !anyErrors orelse !anyWarnings then ()
		else (
		  pr ["***** Bogus BOM after ", phase, " *****\n"];
		  anyErrors := true);
		err  msg)
	  fun warning msg = (
		if !anyErrors orelse !anyWarnings then ()
		else (
		  pr ["***** Possibly Bogus BOM after ", phase, " *****\n"];
		  anyWarnings := true);
		err msg)
(*
	  fun cerror msg = pr ("== "::msg)
*)
	(* for tracking census counts *)
	  val counts = ChkVC.init (error o List.map S)
	  val bindVar = ChkVC.bind counts
	  val useVar = ChkVC.use counts
	  val appVar = ChkVC.appUse counts
	(* match the parameter types against argument variables *)
	  fun checkArgTypes (ctx, paramTys, argTys) = let
	      (* chk1 : ty * ty -> unit *)
	        fun chk1 (pty, aty) =
		      if BTy.equal (aty, pty)
                        then ()
		        else error [
			    S "type mismatch in ", S(concat ctx), NL,
			    S "  expected  ", TY pty, NL,
			    S "  but found ", TY aty, NL
			  ]
	        in 
	          if (length paramTys = length argTys)
                    then ListPair.app chk1 (paramTys, argTys)
                    else error [
			S "wrong number of arguments in ", S(concat ctx), NL,
			S "  expected (", TYS paramTys, S ")", NL,
			S "  found    (", TYS argTys, S ")", NL
		      ]
	        end
	(* match a list of variables to a context *)
	  fun chkContext (cxt, xs) = (case cxt
		 of TAIL(f, tys) => checkArgTypes (["return from ", v2s f], tys, typesOf xs)
		  | BIND(ys, rhs) => checkArgTypes (
		      ["binding ", vl2s ys, " = ", BOMUtil.expToString rhs],
		      typesOf ys, typesOf xs)
		(* end case *))
	(* create a tail context *)
	  fun tailContext f = let
		val (_, _, rng) = BTy.asFunTy(BV.typeOf f)
		in
		  TAIL(f, rng)
		end
	(* Check that a variable is bound *)
	  fun chkVar (x, ctx) = useVar x
	  fun chkApplyVar (x, cxt) = appVar x
	  fun chkVars (xs, ctx) = List.app (fn x => chkVar(x, ctx)) xs
	  fun chkBinding (x, binding) = (
		bindVar x;
		if eqVK(BV.kindOf x, binding)
		  then ()
		  else error[
		      S "binding of ", V x, S " is ",
		      S (vkToString(BV.kindOf x)), S " (expected ",
		      S (vkToString binding), S ")\n"
		    ])
	  fun chkBindings (lhs, binding) =
		List.app (fn x => chkBinding(x, binding)) lhs
	  fun chkPat (B.P_DCon(dc, xs)) = (
		List.app bindVar xs;
		checkArgTypes (["pattern: ", DC.nameOf dc, vl2s xs], DC.domainOf dc, typesOf xs))
	    | chkPat (B.P_Const _) = ()
	(* *)
	  fun insertFB (fb as B.FB{f, ...}) = chkBinding (f, BV.VK_Fun fb)
	  fun chkFB (B.FB{f, params, exh, body}) = let
                val (argTys, exhTys, retTys) =
                      case BV.typeOf f
                       of BTy.T_Fun(argTys, exhTys, retTys) =>
			    (argTys, exhTys, retTys)
                        | BTy.T_Cont(argTys) => (argTys, [], [])
                        | ty => (error[
			      S "expected function/continuation type for ", VTY f, NL
			    ];
			    ([],[],[]))
                      (* end case *)
                in
		  chkBindings (params, BV.VK_Param);
		  checkArgTypes(["Fun ", v2s f, " params"],
		    argTys, typesOf params);
		  chkBindings (exh, BV.VK_Param);
		  checkArgTypes(["Fun ", v2s f, " exh"],
		    exhTys, typesOf exh);
		  chkE (tailContext f, body)
                end
	  and chkE (cxt, B.E_Pt(_, t)) = (case t
		 of B.E_Let(lhs, rhs, e) => (
		      chkBindings (lhs, BV.VK_Let rhs);
		      chkE(BIND(lhs, rhs), rhs);
		      chkE(cxt, e))
		  | B.E_Stmt(lhs, rhs, e) => (
		      chkBindings (lhs, BV.VK_RHS rhs);
		      chkRHS (lhs, rhs);
		      chkE(cxt, e))
		  | B.E_Fun(fbs, e) => (
		      List.app insertFB fbs;
		      List.app chkFB fbs;
		      chkE (cxt, e))
		  | B.E_Cont(fb as B.FB{f, params, exh, body}, e) => (
		      chkBinding (f, BV.VK_Cont fb);
		      chkBindings (params, BV.VK_Param);
		      chkE(cxt, e);
		      if not(null exh)
			then error[
			    S "continuation ", V f, S " has non-empty return list"
			  ]
			else ();
		      chkE(cxt, body))
		  | B.E_If(cond, e1, e2) => let
		      val args = CondUtil.varsOf cond
		      val paramTys = BOMUtil.condArgTys cond
		      fun chkParamArg (paramTy, arg) = if BTy.equal(BV.typeOf arg, paramTy)
			    then ()
			    else error [
				S "type mismatch in ", S(CondUtil.nameOf cond), S "(... ", V arg, S " ...)", NL,
			        S "  expected  ", TY paramTy, NL,
			        S "  but found ", TY(BV.typeOf arg), NL
			      ]
		      in
			chkVars (args, "If");
			chkE(cxt, e1); chkE(cxt, e2)
		      end
(* FIXME: need to check that the type of x covers the types of the cases! *)
		  | B.E_Case(x, cases, dflt) => let
		      fun chk' (pat, e) = (chkPat pat; chkE(cxt, e))
		      in
			chkVar (x, "Case");
			List.app chk' cases;
			Option.app (fn e => chkE(cxt, e)) dflt
		      end
		  | B.E_Typecase(tv, cases, dflt) => raise Fail "Typecase"
		  | B.E_Apply(f, args, rets) => (
		      chkApplyVar (f, "Apply");
		      case BV.typeOf f
		       of BTy.T_Fun(argTys, exhTys, retTys) => (
			    chkVars (args, "Apply args");
			    chkVars (rets, "Apply rets");
			    checkArgTypes (["Apply ", v2s f, " args"], argTys, typesOf args);
			    checkArgTypes (["Apply ", v2s f, " rets"], exhTys, typesOf rets);
			    case cxt
			     of TAIL(g, tys) =>
				  checkArgTypes (["Apply ", v2s f, " in ", v2s g], tys, retTys)
			      | BIND(ys, _) =>
				  checkArgTypes (
				    ["binding ", vl2s ys, " to Apply ", v2s f],
				    typesOf ys, retTys)
			    (* end case *))
			| ty => error[VTY f, S " is not a function\n"]
		      (* end case *))
		  | B.E_Throw(k, args) => (
		      chkApplyVar (k, "Throw");
		      case BV.typeOf k
		       of BTy.T_Cont(argTys) => (
			    chkVars (args, "Throw args");
			    checkArgTypes (["Throw ", v2s k, " args"], argTys, typesOf args))
			| ty => error[VTY k, S " is not a continuation\n"]
		      (* end case *))
		  | B.E_Ret args => (
		      chkVars(args, "Return");
		      chkContext (cxt, args))
		  | B.E_HLOp(hlop, args, rets) => let
		      val HLOp.HLOp{sign as {params=paramTys, exh=exhTys, results=resTys}, returns, ...} = hlop
		      in
			chkVars (args, "HLOp args");
			chkVars (rets, "HLOp rets");
			checkArgTypes (["HLOP ", (HLOp.toString hlop), " args"], paramTys, typesOf args);
			checkArgTypes (["HLOP ", (HLOp.toString hlop), " rets"], exhTys, typesOf rets);
			case (returns, cxt)
			 of (true, TAIL(g, tys)) =>
			      checkArgTypes (["return type of HLOP ", (HLOp.toString hlop), " in ", v2s g], tys, resTys)
			  | (true, BIND(ys, _)) =>
			      checkArgTypes (
				["binding ", vl2s ys, " to HLOP ", (HLOp.toString hlop)],
				typesOf ys, resTys)
			  | (false, _) => ()
			(* end case *)
		      end
		(* end case *))
	  and chkRHS (lhs, rhs) = (case (typesOf lhs, rhs)
		 of (lhsTys, B.E_Prim p) => chkPrim (lhs, lhsTys, p)
(* FIXME: record allocation *)
		  | ([ty], B.E_Alloc(allocTy, xs)) => let
		      val tys = typesOf xs
		      val expectedTys = (case allocTy
			     of BTy.T_Record flds => List.map #2 flds
			      | BTy.T_Packed flds => List.map #3 flds
			      | _ => (
				  error [
				      S "allocation type not record or packed record", NL,
				      S "  found ", TY allocTy, NL
				    ];
				  tys)
			    (* end case *))
		      in
			chkVars(xs, "Alloc");
			if BTy.equal (allocTy, ty)
			  then ()
			  else error [
			      S "type mismatch in: ", VS lhs, S " = Alloc ", VS xs, NL,
			      S "  lhs type ", TY ty, NL,
			      S "  rhs type ", TY allocTy, NL
			    ];
			if ListPair.allEq BTy.equal (tys, expectedTys)
			  then ()
			  else error [
			      S "type mismatch in Alloc: ", VS lhs, S " = ", VS xs, NL,
			      S "  expected ", TYS expectedTys, NL,
			      S "  found    ", TYS tys, NL
			    ]
		      end
                  | ([ty], B.E_DCon(dc, args)) => (
                      chkVars(args, DC.nameOf dc);
		      checkArgTypes ([DC.nameOf dc, vl2s args], DC.domainOf dc, typesOf args))
		  | ([ty], B.E_Select(i, x)) => (
                      chkVar(x, "Select");
                      case BV.typeOf x
                       of BTy.T_Record tys =>
			    if (i < List.length tys) andalso BTy.equal(#2(List.nth (tys, i)), ty)
			      then ()
			      else error[
				  S "type mismatch in Select: ",
				  VTYS lhs, S " = #", S(Int.toString i), S "(", VTY x, S ")", NL
				]
			| BTy.T_Packed flds =>
			    if (i < List.length flds) andalso BTy.equal(#3(List.nth (flds, i)), ty)
			      then ()
			      else error[
				  S "type mismatch in Select: ",
				  VTYS lhs, S " = #", S(Int.toString i), S "(", VTY x, S ")", NL
				]
			| ty => error[
			      VTY x, S " is not a tuple/record: ",
			      VS lhs, S " = #", S(Int.toString i), S "(", V  x, S ")", NL
			    ]
		      (* end case *))
		  | ([], B.E_Update(i, x, y)) => let
		      fun chkUpdate proj flds = if (i < List.length flds)
			    then (case proj (List.nth(flds, i))
			       of (true, ty) =>
				    if BTy.equal(BV.typeOf y, ty)
				      then ()
				      else error[
					  S "type mismatch in #", S(Int.toString i),
					  S "(", V x, S ") := ", V y, NL
					]
				| (false, _) => error[
				      S "update of non-mutable field in #",
				      S "(", V x, S ") := ", V y, NL
				    ]
			      (* end case *))
			    else error [
				S "index out of bounds in #", S(Int.toString i),
				S "(", V x, S ") := ", V y, NL
			      ]
		      in
			chkVar(x, "Update");
			chkVar(y, "Update");
			case BV.typeOf x
			 of BTy.T_Record flds => chkUpdate (fn fld => fld) flds
			  | BTy.T_Packed flds => chkUpdate (fn (_, mut, ty) => (mut, ty)) flds
			  | ty => error[
				VTY x, S " is not a mutable record",
				S "#", S(Int.toString i), S "(", V x, S ") := ", V y, NL
			      ]
			(* end case *)
		      end
		  | ([ty], B.E_AddrOf(i, x)) => (
                      chkVar(x, "AddrOf");
                      case BV.typeOf x
                       of BTy.T_Record tys => 
			    if (i < List.length tys) andalso BTy.equal(BTy.addrTy(#2(List.nth (tys, i))), ty)
			      then ()
                              else error[S "type mismatch in AddrOf: ", VS lhs, S " = &(", V x, S ")", NL]
			| BTy.T_Packed flds =>
			    if (i < List.length flds) andalso BTy.equal(#3(List.nth (flds, i)), ty)
			      then ()
			      else error[
				  S "type mismatch in Select: ",
				  VTYS lhs, S " = &", S(Int.toString i), S "(", VTY x, S ")", NL
				]
			| ty => error[
			      VTY x, S " is not a tuple/record ", VS lhs, S " = &(", V x, S ")", NL
			    ]
		      (* end case *))
		  | ([ty], B.E_Cast(ty', x)) => (
		      chkVar (x, "Cast");
		      if BTy.equal(ty', ty)
			then ()
			else error [
			    S "type mismatch:", VTYS lhs, S " = (", TY ty', S ")", VTY x, NL
			  ];
		      if BTy.validCast{srcTy=BV.typeOf x, dstTy=ty'}
			then ()
			else error [
			    S "invalid cast:", VTYS lhs, S" = (", TY ty', S ")", VTY x, NL
			  ])
		  | ([ty], B.E_Promote y) => (
		      chkVar (y, "Promote");
		      if BTy.equal(ty, BV.typeOf y) orelse BTy.equal(ty, BTy.T_Any) then ()
			else error [S "type mismatch in Promote: ", VS lhs, S " = ", V y, NL])
		  | ([ty], B.E_CCall(cf, args)) => (
(* FIXME: check that the return type matches *)
		      chkApplyVar (cf, "CCall"); 
                      chkVars (args, "CCall args"))
		  | ([], B.E_CCall(cf, args)) => (
		      chkApplyVar (cf, "CCall"); 
                      chkVars (args, "CCall args"))
		  | ([ty], B.E_HostVProc) => (
                      if BTy.equal(BTy.vprocTy, ty)
                         then ()
                         else error[S "type mismatch in HostVProc: ", VS lhs, S " = host_vproc()\n"])
		  | ([ty], B.E_VPLoad(n, vp)) => (
                      chkVar(vp, "VPLoad");
                      if BTy.equal(BV.typeOf vp, BTy.vprocTy)
                        then ()
                        else error[
			    S "type mismatch in VPLoad: ", VS lhs, S " = vpload(", 
			    S(IntInf.toString n), S ", ", V vp, S ")", NL
			  ])
		  | ([], B.E_VPStore(n, vp, x)) => (
		      chkVar(vp, "VPStore"); 
                      chkVar(x, "VPStore");
                      if BTy.equal(BV.typeOf vp, BTy.vprocTy)
			then ()
			else error[
			    S "type mismatch in VPStore: ", VS lhs, S " = vpstore(", 
			    S(IntInf.toString n), S ", ", V vp, S ", ", V x, S ")", NL
			  ])
		  | ([ty], B.E_VPAddr(n, vp)) => (
                      chkVar(vp, "VPAddr");
                      if BTy.equal(BV.typeOf vp, BTy.vprocTy)
                        then ()
                        else error[
			    S "type mismatch in VPAddr: ", VS lhs, S " = vpaddr(", 
			    S(IntInf.toString n), S ", ", V vp, S ")", NL
			  ])
		  | ([ty], B.E_Const(lit, ty')) => let
		      fun err () = error [
			      S "literal has bogus type: ", VS lhs, S" = ", 
			      S(Literal.toString lit), S":", TY ty', NL
			    ]
		      in
		      (* first, check the literal against ty' *)
			case (lit, ty')
			 of (Literal.Int _, BTy.T_Raw rty) =>
			      if RawTypes.isInt rty then () else err()
			  | (Literal.Float _, BTy.T_Raw rty) =>
			      if RawTypes.isFloat rty then () else err()
			  | (Literal.String _, _) =>
			      if BTy.equal(ty', BTy.charVecTy) then () else err()
			  | _ => err()
			(* end case *);
		      (* then check ty' against ty *)
			if BTy.equal(ty', ty)
			  then ()
			  else error [
			      S "type mismatch in Const: ",  VS lhs, S " = ", 
			      S(Literal.toString lit), S ":", TY ty', 
			      S "; expected ", TY ty, NL
			    ]
		      end
		  | _ => error[S "bogus rhs for ", VS lhs, NL]
		(* end case *))
	  and chkPrim (lhs, lhsTys, p) = let
		val args = PrimUtil.varsOf p
		val (paramTys, resTy) = BOMUtil.signOfPrim p
		fun chkParamArg (paramTy, arg) = if BTy.equal(BV.typeOf arg, paramTy)
		      then ()
		      else error [
			  S "type mismatch in ", S(PrimUtil.nameOf p), S "(... ", V arg, S " ...)", NL,
			  S "  expected  ", TY paramTy, NL, 
			  S "  but found ", TY(BV.typeOf arg), NL
			]
		in
		  chkVars (args, PrimUtil.nameOf p);
		  case (lhsTys, resTy)
		   of ([], NONE) => ()
		    | ([lhsTy], SOME rhsTy) => if BTy.equal (rhsTy, lhsTy)
			then ()
			else err [
			    S "type mismatch in: ", VTYS lhs, S " = ", S(PrimUtil.nameOf p), VS args, NL,
			    S "  lhs type ", TY lhsTy, NL,
			    S "  rhs type ", TY rhsTy, NL
			  ]
		    | _ => error[
			  S "arity mismatch in ", VS lhs, S " = ", S(PrimUtil.nameOf p), VS args, NL
			]
		  (* end case *);
		  ListPair.appEq chkParamArg (paramTys, args)
		end
	(* check an external function *)
	  fun chkExtern (CFunctions.CFun{var, name, ...}) = (
		bindVar var;
		case BV.kindOf var
		 of BV.VK_CFun _ => ()
		  | vk => error[
			S "extern ", V var, S " has kind ", S(vkToString vk), NL
		      ]
		(* end case *))
	  in
	    List.app chkExtern externs;
	    chkFB body; insertFB body;
(* FIXME
	  (* check census counts *)
	    ChkVC.checkCounts counts;
*)
	  (* report errors, if any *)
	    if !anyErrors
	      then let
	    (* FIXME: we should generate this name from the input file name! *)
		val outFile = "broken-BOM"
		val outS = TextIO.openOut outFile
		in
		  pr ["broken BOM dumped to ", outFile, "\n"];
		  PrintBOM.output (outS, program);
		  TextIO.closeOut outS;
		  raise Fail "broken BOM"
		end
	      else ();
	  (* return the error status *)
	    !anyErrors
	  end

    val check = BasicControl.mkTracePass {
	    passName = "bom-check",
	    pass = check,
	    verbose = 2
	  }

  end
