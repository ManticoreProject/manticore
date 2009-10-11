(* check-bom.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Check various invariants in BOM representation.
 *)

structure CheckBOM : sig

    val check : string * BOM.module -> bool

  end = struct

    structure B = BOM
    structure BV = B.Var
    structure VTbl = BV.Tbl
    structure VSet = BV.Set
    structure BTy = BOMTy
    structure BTU = BOMTyUtil

    val v2s = BV.toString
    fun vl2s xs = String.concat["(", String.concatWith "," (List.map v2s xs), ")"]
    fun v2s' x = concat[v2s x, ":", BTU.toString(BV.typeOf x)]
    fun vl2s' xs = String.concat["(", String.concatWith "," (List.map v2s' xs), ")"]

    val t2s = BTU.toString
    fun tl2s ts = concat["(", String.concatWith "," (map t2s ts), ")"]

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

    fun vkToString B.VK_None = "VK_None"
      | vkToString (B.VK_Let rhs) =
	  concat["VK_Let(", BOMUtil.expToString rhs, ")"]
      | vkToString (B.VK_RHS rhs) =
	  concat["VK_RHS(", BOMUtil.rhsToString rhs, ")"]
      | vkToString B.VK_Param = "VK_Param"
      | vkToString (B.VK_Fun _) = "VK_Fun"
      | vkToString (B.VK_Cont _) = "VK_Cont"
      | vkToString (B.VK_CFun cf) =
	  concat["VK_CFun(", CFunctions.nameOf cf, ")"]

  (* the context of a BOM expression *)
    datatype context
      = TAIL of (B.var * B.ty list)
      | BIND of (B.var list * B.exp)

    fun pr s = TextIO.output(TextIO.stdErr, concat s)

    fun typesOf xs = List.map BV.typeOf xs

  (* get the binding of a variable, chasing through casts and renamings *)
    fun resolveBinding x = let
	  fun lp x = (case BV.kindOf x
		 of B.VK_Let(B.E_Pt(_, B.E_Ret[y])) => lp y
		  | B.VK_RHS(B.E_Cast(_, y)) => lp y
		  | k => k
		(* end case *))
	  in
	    lp x
	  end

  (* check for assignments of unpromoted values; return true if okay and false
   * otherwise.
   *)
    fun checkAssign (ty, x) = let
	  val k = BTU.kindOf ty
	  in
	    if (k = BTy.K_BOXED) orelse (k = BTy.K_UNIFORM)
	      then (case resolveBinding x
		 of B.VK_RHS(B.E_Promote _) => true
		  | B.VK_RHS(B.E_Const _) => true
		  | _ => false
		(* end case *))
	      else true
	  end

    fun check (phase, module) = let
	  val B.MODULE{name, externs, hlops, rewrites, body} = module
	  val anyErrors = ref false
	  val anyWarnings = ref false
	(* report an error *)
	  fun error msg = (
		if !anyErrors orelse !anyWarnings then ()
		else (
		  pr ["***** Bogus BOM in ", Atom.toString name, " after ", phase, " *****\n"];
		  anyErrors := true);
		pr ("** " :: msg))
	  fun warning msg = (
		if !anyErrors orelse !anyWarnings then ()
		else (
		  pr ["***** Possibly Bogus BOM in ", Atom.toString name, " after ", phase, " *****\n"];
		  anyWarnings := true);
		pr ("?? " :: msg))
	  fun cerror msg = pr ("== "::msg)
	(* for tracking census counts *)
	  val counts = ChkVC.init error
	  val bindVar = ChkVC.bind counts
	  val useVar = ChkVC.use counts
	  val appVar = ChkVC.appUse counts
	(* match the parameter types against argument variables *)
	  fun checkArgTypes (cmp, ctx, paramTys, argTys) = let
	      (* chk1 : ty * ty -> unit *)
	        fun chk1 (pty, aty) =
		      if (cmp (aty, pty))
                        then ()
		        else (
			  error  ["type mismatch in ", ctx, "\n"];
			  cerror ["  expected  ", BTU.toString pty, "\n"];
			  cerror ["  but found ", BTU.toString aty, "\n"])
	        in 
	          if (length paramTys = length argTys)
                    then ListPair.app chk1 (paramTys, argTys)
                    else let
	            (* str : ty list -> string *)
                      fun str ts = String.concatWith "," (map BTU.toString ts)
                      in 
                        error  ["wrong number of arguments in ", ctx, "\n"];
			cerror ["  expected (", str paramTys, ")\n"];
			cerror ["  found    (", str argTys, ")\n"]
                      end
	        end
	(* match a list of variables to a context *)
	  fun chkContext (cxt, xs) = (case cxt
		 of TAIL(f, tys) => checkArgTypes (BTU.match, "return from " ^ v2s f, tys, typesOf xs)
		  | BIND(ys, rhs) => checkArgTypes (
		      BTU.match, concat["binding ", vl2s ys, " = ", BOMUtil.expToString rhs],
		      typesOf ys, typesOf xs)
		(* end case *))
	(* create a tail context *)
	  fun tailContext f = let
		val (_, _, rng) = BTU.asFunTy(BV.typeOf f)
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
		      "binding of ", v2s x, " is ",
		      vkToString(BV.kindOf x), " (expected ",
		      vkToString binding, ")\n"
		    ])
	  fun chkBindings (lhs, binding) =
		List.app (fn x => chkBinding(x, binding)) lhs
(* FIXME: we should check the kind of the xs, but we don't have a kind for pattern-bound
 * variables yet!
 *)
	  fun chkPat (B.P_DCon(BTy.DCon{name, argTy, myTyc, ...}, xs)) = (
		List.app bindVar xs;
		checkArgTypes (BTU.match, concat["pattern: ", name, vl2s xs],
		  argTy, typesOf xs))
	    | chkPat (B.P_Const _) = ()
	(* *)
	  fun insertFB (fb as B.FB{f, ...}) = (
		chkBinding (f, B.VK_Fun fb);
		if BV.isHLOp f then useVar f else ())
	  fun chkFB (B.FB{f, params, exh, body}) = let
                val (argTys, exhTys, retTys) =
                      case BV.typeOf f
                       of BTy.T_Fun(argTys, exhTys, retTys) =>
			    (argTys, exhTys, retTys)
                        | BTy.T_Cont(argTys) => (argTys, [], [])
                        | ty => (error[
			      "expected function/continuation type for ",
			      v2s f, ":", BTU.toString(BV.typeOf f)
			    ];
			    ([],[],[]))
                      (* end case *)
                in
		  chkBindings (params, B.VK_Param);
		  checkArgTypes(BTU.equal, concat["Fun ", v2s f, " params"],
		    argTys, typesOf params);
		  chkBindings (exh, B.VK_Param);
		  checkArgTypes(BTU.equal, concat["Fun ", v2s f, " exh"],
		    exhTys, typesOf exh);
		  chkE (tailContext f, body)
                end
	  and chkE (cxt, B.E_Pt(_, t)) = (case t
		 of B.E_Let(lhs, rhs, e) => (
		      chkBindings (lhs, B.VK_Let rhs);
		      chkE(BIND(lhs, rhs), rhs);
		      chkE(cxt, e))
		  | B.E_Stmt(lhs, rhs, e) => (
		      chkBindings (lhs, B.VK_RHS rhs);
		      chkRHS (lhs, rhs);
		      chkE(cxt, e))
		  | B.E_Fun(fbs, e) => (
		      List.app insertFB fbs;
		      List.app chkFB fbs;
		      chkE (cxt, e))
		  | B.E_Cont(fb as B.FB{f, params, exh, body}, e) => (
		      chkBinding (f, B.VK_Cont fb);
		      chkBindings (params, B.VK_Param);
		      chkE(cxt, e);
		      if not(null exh)
			then error[
			    "continuation ", v2s f, " has non-empty return list"
			  ]
			else ();
		      chkE(cxt, body))
		  | B.E_If(cond, e1, e2) => let
		      val args = CondUtil.varsOf cond
		      val paramTys = BOMUtil.condArgTys cond
		      fun chkParamArg (paramTy, arg) = if BTU.match(BV.typeOf arg, paramTy)
			    then ()
			    else (
			      error  ["type mismatch in ", CondUtil.nameOf cond, "(... ", v2s arg, " ...)\n"];
			      cerror ["  expected  ", BTU.toString paramTy, "\n"];
			      cerror ["  but found ", BTU.toString(BV.typeOf arg), "\n"])
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
		  | B.E_Apply(f, args, rets) => (
		      chkApplyVar (f, "Apply");
		      case BV.typeOf f
		       of BTy.T_Fun(argTys, exhTys, retTys) => (
			    chkVars (args, "Apply args");
			    chkVars (rets, "Apply rets");
			    checkArgTypes (BTU.match, concat["Apply ", v2s f, " args"], argTys, typesOf args);
			    checkArgTypes (BTU.match, concat["Apply ", v2s f, " rets"], exhTys, typesOf rets);
			    case cxt
			     of TAIL(g, tys) =>
				  checkArgTypes (BTU.match, concat["Apply ", v2s f, " in ", v2s g], tys, retTys)
			      | BIND(ys, _) =>
				  checkArgTypes (
                                    BTU.match, 
				    concat["binding ", vl2s ys, " to Apply ", v2s f],
				    typesOf ys, retTys)
			    (* end case *))
			| ty => error[v2s f, " : ", BTU.toString ty, " is not a function\n"]
		      (* end case *))
		  | B.E_Throw(k, args) => (
		      chkApplyVar (k, "Throw");
		      case BV.typeOf k
		       of BTy.T_Cont(argTys) => (
			    chkVars (args, "Throw args");
			    checkArgTypes (BTU.match, concat["Throw ", v2s k, " args"], argTys, typesOf args))
			| ty => error[v2s k, ":", BTU.toString ty, " is not a continuation\n"]
		      (* end case *))
		  | B.E_Ret args => (
		      chkVars(args, "Return");
		      chkContext (cxt, args))
		  | B.E_HLOp(hlop, args, rets) => let
		      val HLOp.HLOp{sign as {params=paramTys, exh=exhTys, results=resTys}, returns, ...} = hlop
		      val paramTys = List.map (fn (HLOp.PARAM ty) => ty) paramTys
		      in
			chkVars (args, "HLOp args");
			chkVars (rets, "HLOp rets");
			checkArgTypes (BTU.match, concat["HLOP ", (HLOp.toString hlop), " args"], paramTys, typesOf args);
			checkArgTypes (BTU.match, concat["HLOP ", (HLOp.toString hlop), " rets"], exhTys, typesOf rets);
			case (returns, cxt)
			 of (true, TAIL(g, tys)) =>
			      checkArgTypes (BTU.match, concat["return type of HLOP ", (HLOp.toString hlop), " in ", v2s g], tys, resTys)
			  | (true, BIND(ys, _)) =>
			      checkArgTypes (
                                BTU.match, 
				concat["binding ", vl2s ys, " to HLOP ", (HLOp.toString hlop)],
				typesOf ys, resTys)
			  | (false, _) => ()
			(* end case *)
		      end
		(* end case *))
	  and chkRHS (lhs, rhs) = (case (typesOf lhs, rhs)
		 of ([ty], B.E_Const(lit, ty')) => (
		    (* first, check the literal against ty' *)
		      case (lit, ty')
		       of (Literal.Enum _, BTy.T_Enum _) => ()
			| (Literal.Enum _, BTy.T_TyCon _) => ()
(* NOTE: the following shouldn't be necessary, but case-simplify doesn't put in enum types! *)
			| (Literal.Enum _, BTy.T_Any) => ()
			| (Literal.StateVal w, _) => () (* what is the type of StateVals? *)
			| (Literal.Tag s, _) => () (* what is the type of Tags? *)
			| (Literal.Int _, BTy.T_Raw BTy.T_Byte) => ()
			| (Literal.Int _, BTy.T_Raw BTy.T_Short) => ()
			| (Literal.Int _, BTy.T_Raw BTy.T_Int) => ()
			| (Literal.Int _, BTy.T_Raw BTy.T_Long) => ()
			| (Literal.Float _, BTy.T_Raw BTy.T_Float) => ()
			| (Literal.Float _, BTy.T_Raw BTy.T_Double) => ()
			| (Literal.Char _, BTy.T_Raw BTy.T_Int) => ()
			| (Literal.String _, BTy.T_Any) => ()
			| _ => error[
			    "literal has bogus type: ",  vl2s lhs, " = ", 
			    Literal.toString lit, ":", BTU.toString ty', "\n"
			    ]
		      (* end case *);
		    (* then check ty' against ty *)
		      if BTU.equal(ty', ty)
			then ()
			else error[
			    "type mismatch in Const: ",  vl2s lhs, " = ", 
			    Literal.toString lit, ":", BTU.toString ty', 
			    "; expected ", BTU.toString ty, "\n"
			  ])
		  | ([ty], B.E_Cast(ty', x)) => (
		      chkVar (x, "Cast");
		      if BTU.match(ty', ty)
			then ()
			else error [
			    "type mismatch:", vl2s' lhs, " = (", BTU.toString ty',
			    ")", v2s' x, "\n"
			  ];
		      if BTU.validCast(BV.typeOf x, ty')
			then ()
			else error [
			    "invalid cast:", vl2s' lhs, " = (", BTU.toString ty',
			    ")", v2s' x, "\n"
			  ])
		  | ([ty], B.E_Select(i, x)) => (
                      chkVar(x, "Select");
                      case BV.typeOf x
                       of BTy.T_Tuple(_, tys) =>
			    if (i < List.length tys) andalso BTU.match(List.nth (tys, i), ty)
			      then ()
			      else error[
				  "type mismatch in Select: ",
				   vl2s' lhs, " = #", Int.toString i, "(", v2s' x, ")\n"
				]
			| ty => error[v2s x, ":", BTU.toString ty, " is not a tuple: ",
                                    vl2s lhs, " = #", Int.toString i, "(", v2s x, ")\n"]
		      (* end case *))
		  | ([], B.E_Update(i, x, y)) => (
                      chkVar(x, "Update");
                      chkVar(y, "Update");
                      case BV.typeOf x
                       of BTy.T_Tuple(true, tys) =>
			    if (i < List.length tys)
			      then let
				val ty = List.nth(tys, i)
				val k = BTU.kindOf ty
				in
				  if BTU.equal(BV.typeOf y, ty)
				    then ()
				    else error[
					"type mismatch in #", Int.toString i,
					"(", v2s x, ") := ", v2s y, "\n"
				      ];
				  if checkAssign (ty, y)
				    then ()
				    else warning[
					"possible unpromoted update in #", Int.toString i,
					"(", v2s x, ") := ", v2s y, "\n"
				      ]
				end
			      else error [
				  "index out of bounds in #", Int.toString i,
				  "(", v2s x, ") := ", v2s y, "\n"
				]
			| ty => error[v2s x, ":", BTU.toString ty, " is not a mutable tuple",
                                    "#", Int.toString i, "(", v2s x, ") := ", v2s y, "\n"]
		      (* end case *))
		  | ([ty], B.E_AddrOf(i, x)) => (
                      chkVar(x, "AddrOf");
                      case BV.typeOf x
                       of BTy.T_Tuple(_, tys) => 
			    if (i < List.length tys) andalso BTU.match(BTy.T_Addr(List.nth (tys, i)), ty)
			      then ()
                              else error["type mismatch in AddrOf: ", vl2s lhs, " = &(", v2s x, ")\n"]
			| ty => error[v2s x, ":", BTU.toString ty, " is not a tuple",
                                    vl2s lhs, " = &(", v2s x, ")\n"]
		      (* end case *))
		  | ([ty], B.E_Alloc(allocTy, xs)) => (
                      chkVars(xs, "Alloc");
                      if BTU.match (allocTy, ty)
			then ()
			else (error  ["type mismatch in: ", vl2s lhs, " = Alloc ", vl2s xs, "\n"];
			      cerror ["  lhs type ", t2s ty, "\n"];
			      cerror ["  rhs type ", t2s allocTy, "\n"]);
		      if (BTU.match(BTy.T_Tuple(true, typesOf xs), allocTy)
			orelse BTU.match(BTy.T_Tuple(false, typesOf xs), allocTy))
                        then ()
                        else (error  ["type mismatch in Alloc: ", vl2s lhs, " = ", vl2s xs, "\n"];
			      cerror ["  expected ", t2s allocTy, "\n"];
			      cerror ["  found    ", tl2s (typesOf xs), "\n"]))
		  | ([ty], B.E_Promote y) => (
		      chkVar (y, "Promote");
		      if BTU.equal(ty, BV.typeOf y) then ()
			else error ["type mismatch in Promote: ", vl2s lhs, " = ", v2s y, "\n"])
		  | (lhsTys, B.E_Prim p) => chkPrim (lhs, lhsTys, p)
                  | ([ty], B.E_DCon(BTy.DCon{name, argTy, myTyc, ...}, args)) => (
                      chkVars(args, name);
		      checkArgTypes (BTU.match, name ^ vl2s args, argTy, typesOf args))
		  | ([ty], B.E_CCall(cf, args)) => (
(* FIXME: check that the return type matches *)
		      chkApplyVar (cf, "CCall"); 
                      chkVars (args, "CCall args"))
		  | ([], B.E_CCall(cf, args)) => (
		      chkApplyVar (cf, "CCall"); 
                      chkVars (args, "CCall args"))
		  | ([ty], B.E_HostVProc) => (
                      if BTU.match(BTy.T_VProc, ty)
                         then ()
                         else error["type mismatch in HostVProc: ", vl2s lhs, " = host_vproc()\n"])
		  | ([ty], B.E_VPLoad(n, vp)) => (
                      chkVar(vp, "VPLoad");
                      if BTU.equal(BV.typeOf vp, BTy.T_VProc)
                        then ()
                        else error[
			    "type mismatch in VPLoad: ", vl2s lhs, " = vpload(", 
			    IntInf.toString n, ", ", v2s vp, ")\n"
			  ])
		  | ([], B.E_VPStore(n, vp, x)) => (
		      chkVar(vp, "VPStore"); 
                      chkVar(x, "VPStore");
                      if BTU.equal(BV.typeOf vp, BTy.T_VProc)
                         then ()
                         else error["type mismatch in VPStore: ",
                                  vl2s lhs, " = vpstore(", 
                                  IntInf.toString n, ", ", v2s vp, ", ", v2s x, ")\n"])
		  | ([ty], B.E_VPAddr(n, vp)) => (
                      chkVar(vp, "VPAddr");
                      if BTU.equal(BV.typeOf vp, BTy.T_VProc)
                        then ()
                        else error[
			    "type mismatch in VPAddr: ", vl2s lhs, " = vpaddr(", 
			    IntInf.toString n, ", ", v2s vp, ")\n"
			  ])
		  | _ => error["bogus rhs for ", vl2s lhs, "\n"]
		(* end case *))
	  and chkPrim (lhs, lhsTys, p) = let
		val args = PrimUtil.varsOf p
		val (paramTys, resTy) = BOMUtil.signOfPrim p
		fun chkParamArg (paramTy, arg) = if BTU.match(BV.typeOf arg, paramTy)
		      then ()
		      else (
			error  ["type mismatch in ", PrimUtil.nameOf p, "(... ", v2s arg, " ...)\n"];
			cerror ["  expected  ", BTU.toString paramTy, "\n"];
			cerror ["  but found ", BTU.toString(BV.typeOf arg), "\n"])
		in
		  chkVars (args, PrimUtil.nameOf p);
		  case (lhsTys, resTy)
		   of ([], NONE) => ()
		    | ([lhsTy], SOME rhsTy) => if BTU.match (rhsTy, lhsTy)
			then ()
			else (
			  error  ["type mismatch in: ", vl2s lhs, " = ", PrimUtil.nameOf p, vl2s args, "\n"];
			  cerror ["  lhs type ", t2s lhsTy, "\n"];
			  cerror ["  rhs type ", t2s rhsTy, "\n"])
		    | _ => error[
			  "arity mismatch in ", vl2s lhs, " = ", PrimUtil.nameOf p, vl2s args, "\n"
			]
		  (* end case *);
		  ListPair.appEq chkParamArg (paramTys, args);
		(* check polymorphic array updates for missing promotions *)
		  case p
		   of Prim.ArrStore(a, i, x) => if checkAssign(BTy.T_Any, x)
			then ()
			else warning[
			    "possible unpromoted update in ArrStore(", v2s a, ",",
			    v2s i, ",", v2s x, ")\n"
			  ]
		    | Prim.CAS(loc, old, new) => if checkAssign(BV.typeOf new, new)
			then ()
			else warning[
			    "possible unpromoted update in CAS(", v2s loc, ",",
			    v2s old, ",", v2s new, ")\n"
			  ]
		    | _ => ()
		  (* end case *)
		end
	(* check an external function *)
	  fun chkExtern (CFunctions.CFun{var, name, ...}) = (
		bindVar var;
		case BV.kindOf var
		 of B.VK_CFun _ => ()
		  | vk => error[
			"extern ", v2s var, " has kind ", vkToString vk
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
		  PrintBOM.output (outS, module);
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
