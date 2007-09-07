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

    val v2s = BV.toString
    fun vl2s xs = String.concat["(", String.concatWith "," (List.map v2s xs), ")"]
    fun v2s' x = concat[v2s x, ":", BTy.toString(BV.typeOf x)]
    fun vl2s' xs = String.concat["(", String.concatWith "," (List.map v2s' xs), ")"]

(**** Fluet version ****
    fun addFB (B.FB{f, ...}, env) = VSet.add(env, f)

    fun addVars (env, xs) = VSet.addList(env, xs)

    fun addPat (env, B.P_DCon (_, xs)) = addVars (env, xs)
      | addPat (env, B.P_Const _) = env

    fun checkTypes (s : string, B.MODULE{name,externs,body}) = let
	  val anyErr = ref false
	  fun err msg = (
		anyErr := true;
		print(concat("Error: " :: msg));
		print "\n")
	  fun chkVar (env, x, cxt) = if VSet.member(env, x)
		then ()
		else err["variable ", v2s x, " is unbound ", cxt]
	  fun chkVars (env, xs, cxt) = List.app (fn x => chkVar(env, x, cxt)) xs
	(* match the parameter types against arguments *)
	  fun checkArgs (paramTys, args, cxt) = let
		fun chk ([], []) = ()
		  | chk (l, []) = err[Int.toString(length l), " too few arguments in ", cxt]
		  | chk ([], l) = err[Int.toString(length l), " too many arguments in ", cxt]
		  | chk (ty::tys, x::xs) = (
		      if (BTy.match(BV.typeOf x, ty))
			then ()
			else err[
			    "type mismatch in ", cxt, "\n  expected  ", BTy.toString ty,
			    "\n  but found ", v2s x, ":", BTy.toString(BV.typeOf x)
			  ];
		      chk(tys, xs))
		in
		  chk (paramTys, args)
		end
	  fun chkExp (env, B.E_Pt(_,e), expTys) = (case e
                 of B.E_Let(lhs, rhs, e) => (
                      chkExp (env, rhs, List.map BV.typeOf lhs);
                      chkExp (addVars(env, lhs), e, expTys))
		  | B.E_Stmt(lhs, rhs, e) => (
		      chkRHS(env, lhs, rhs);
		      chkExp (addVars(env, lhs), e, expTys))
		  | B.E_Fun(fbs, e) => let
		      val env = List.foldl addFB env fbs
		      in
			List.app (fn fb => chkFB(env, fb)) fbs;
			chkExp(env, e, expTys)
		      end
		  | B.E_Cont(fb, e) => let
		      val env = addFB (fb, env)
		      in
			chkFB(env, fb); 
                        chkExp(env, e, expTys)
		      end
		  | B.E_If(x, e1, e2) => (
                      chkVar(env, x, "If"); 
                      chkExp(env, e1, expTys); 
                      chkExp(env, e2, expTys))
		  | B.E_Case(x, cases, dflt) => (
		      chkVar(env, x, "Switch");
		      List.app (fn (p, e) => chkExp (addPat (env, p), e, expTys)) cases;
		      Option.app (fn e => chkExp (env, e, expTys)) dflt)
		  | B.E_Apply(f, args, exhs) => (
		      chkVar (env, f, "Apply function");
		      case BV.typeOf f
		       of BTy.T_Fun(argTys, exhTys, retTys) => (
			    chkVars (env, args, "Apply args");
			    chkVars (env, exhs, "Apply exhs");
			    checkArgs (argTys, args, concat["Apply ", v2s f, " args"]);
			    checkArgs (exhTys, exhs, concat["Apply ", v2s f, " exhs"]))
			| ty => err[v2s f, ":", BTy.toString ty, " is not a function"]
		      (* end case *))
		  | B.E_Throw(k, args) => (
		      chkVar (env, k, "Throw");
		      case BV.typeOf k
		       of BTy.T_Cont(argTys) => (
			    chkVars (env, args, "Throw args");
			    checkArgs (argTys, args, "Throw " ^ v2s k))
			| ty => err[v2s k, ":", BTy.toString ty, " is not a continuation"]
		      (* end case *))
                  | B.E_Ret(args) => (
                      chkVars (env, args, "Return args");
                      checkArgs (expTys, args, concat ["Return ", vl2s args]))
                  | B.E_HLOp(hlop as HLOp.HLOp {sign as {params as argTys, exh as exhTys, 
                                                         results as resTys}, ...},
                             args, exhs) => (
                      chkVars (env, args, "HLOP args");
                      chkVars (env, exhs, "HLOP exhs");
                      let 
                      val argTys = List.map (fn HLOp.PARAM ty => ty 
                                              | _ => raise Fail "B.E_HLop: argTys") argTys 
                      in
                      checkArgs (argTys, args, "HLOP " ^ (HLOp.toString hlop))
                      end;
		      checkArgs (exhTys, exhs, "HLOP " ^ (HLOp.toString hlop))
		(* end case *)))
	  and chkRHS (env, lhs, rhs) = (case (List.map BV.typeOf lhs, rhs)
		 of ([ty], B.E_Const(lit, ty')) => (
		      if BTy.equal(ty', ty)
			then ()
			else err["type mismatch in Const: ", 
                                 vl2s lhs, " = ", 
                                 Literal.toString lit, ":", BTy.toString ty'])
		  | ([ty], B.E_Cast(ty', x)) => (
		      chkVar (env, x, "Cast");
		      if BTy.match(ty', ty) andalso BTy.validCast(BV.typeOf x, ty')
			then ()
			else err["type mismatch in Cast: ", 
                                 vl2s lhs, " = (", BTy.toString ty', ")", v2s x])
		  | ([ty], B.E_Select(i, x)) => (
                      chkVar(env, x, "Select");
                      case BV.typeOf x
                       of BTy.T_Tuple(_, tys) => 
                             if BTy.equal(ty, List.nth (tys, i))
                                then ()
                                else err["type mismatch in Select: ",
                                         vl2s lhs, " = #", Int.toString i, 
                                         "(", v2s x, ")"]
			| ty => err[v2s x, ":", BTy.toString ty, " is not a tuple: ",
                                    vl2s lhs, " = #", Int.toString i, "(", v2s x, ")"]
		      (* end case *))
		  | ([], B.E_Update(i, x, y)) => (
                      chkVar(env, x, "Update");
                      chkVar(env, y, "Update");
                      case BV.typeOf x
                       of BTy.T_Tuple(true, tys) => 
                              if BTy.equal(BV.typeOf y, List.nth (tys, i))
                                 then ()
                              else err["type mismatch in Update: ",
                                       "#", Int.toString i, "(", v2s x, ") := ", v2s y]
			| ty => err[v2s x, ":", BTy.toString ty, " is not a mutable tuple",
                                    "#", Int.toString i, "(", v2s x, ") := ", v2s y]
		      (* end case *))
		  | ([ty], B.E_AddrOf(i, x)) => (
                      chkVar(env, x, "AddrOf");
                      case BV.typeOf x
                       of BTy.T_Tuple(_, tys) => 
                              if BTy.equal(ty, BTy.T_Addr(List.nth (tys, i)))
                                 then ()
                              else err["type mismatch in AddrOf: ",
                                       vl2s lhs, " = &(", v2s x, ")"]
			| ty => err[v2s x, ":", BTy.toString ty, " is not a tuple",
                                    vl2s lhs, " = &(", v2s x, ")"]
		      (* end case *))
		  | ([ty], B.E_Alloc (allocTy, xs)) => (
                      chkVars(env, xs, "Alloc");
                      if BTy.equal (ty, allocTy) andalso
                         (BTy.equal(ty, BTy.T_Tuple(true, List.map BV.typeOf xs))
                          orelse BTy.equal(ty, BTy.T_Tuple(false, List.map BV.typeOf xs)))
                        then ()
                        else err["type mismatch in Alloc: ", 
                                 vl2s lhs, " = ", vl2s xs])
		  | ([ty], B.E_Prim p) => (
                      chkVars(env, PrimUtil.varsOf p, PrimUtil.nameOf p))
                  | ([ty], B.E_DCon (dcon, args)) => (
                      chkVars(env, args, BOMTyCon.dconName dcon))
		  | ([ty], B.E_CCall(cf, args)) => (
		      chkVar(env, cf, "CCall"); 
                      chkVars(env, args, "CCall args"))
		  | ([], B.E_CCall(cf, args)) => (
		      chkVar(env, cf, "CCall"); 
                      chkVars(env, args, "CCall args"))
		  | ([ty], B.E_HostVProc) => (
                      if BTy.equal(ty, BTy.T_VProc)
                         then ()
                         else err["type mismatch in HostVProc: ",
                                  vl2s lhs, " = host_vproc()"])
		  | ([ty], B.E_VPLoad(n, vp)) => (
                      chkVar(env, vp, "VPLoad");
                      if BTy.equal(BV.typeOf vp, BTy.T_VProc)
                         then ()
                         else err["type mismatch in VPLoad: ",
                                  vl2s lhs, " = vpload(", 
                                  IntInf.toString n, ", ", v2s vp, ")"])
		  | ([], B.E_VPStore(n, vp, x)) => (
		      chkVar(env, vp, "VPStore"); 
                      chkVar(env, x, "VPStore");
                      if BTy.equal(BV.typeOf vp, BTy.T_VProc)
                         then ()
                         else err["type mismatch in VPStore: ",
                                  vl2s lhs, " = vpstore(", 
                                  IntInf.toString n, ", ", v2s vp, ", ", v2s x, ")"])
		  | _ => err["bogus rhs for ", vl2s lhs]
		(* end case *))
	  and chkFB (env, fb as B.FB{f, params, exh, body}) = (let
		fun chk ([], []) = ()
		  | chk (_, []) = err["too few parameters in ", v2s f]
		  | chk ([], _) = err["too many parameters in ", v2s f]
		  | chk (ty::tys, x::xs) = (
		      if (BTy.equal(BV.typeOf x, ty))
			then ()
			else err[
			    "type mismatch in ", v2s f, "\n  expected  ", BTy.toString ty,
			    "\n  but found ", v2s x, ":", BTy.toString(BV.typeOf x)
			  ];
		      chk(tys, xs))
                val (argTys, exhTys, retTys) =
                      case BV.typeOf f
                       of BTy.T_Fun(argTys, exhTys, retTys) =>
                              (argTys, exhTys, retTys)
                        | BTy.T_Cont(argTys) =>
                              (argTys, [], [])
                        | ty => (err["expected function/continuation type for ",
                                     v2s f, ":", BTy.toString(BV.typeOf f)];
                                 ([],[],[]))
                      (* end case *)
                in
                chk(argTys, params);
                chk(exhTys, exh);
		chkExp (addVars(addVars(env, params), exh), body, retTys)
                end)
	  val env = List.foldl
		(fn (cf, env) => VSet.add(env, CFunctions.varOf cf))
		  VSet.empty externs
	  in
	    chkFB (env, body);
            !anyErr
	  end (* check *)
**** Fluet version ****)

(**** Reppy version ****)
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
      | vkToString (B.VK_Extern x) = concat["VK_Extern(", x, ")"]

  (* the context of a BOM expression *)
    datatype context
      = TAIL of (B.var * B.ty list)
      | BIND of B.var list

    fun typesOf xs = List.map BV.typeOf xs

    fun check (phase, module) = let
	  val B.MODULE{name, externs, body} = module
	  val anyErrors = ref false
	(* report an error *)
	  fun pr s = TextIO.output(TextIO.stdErr, concat s)
	  fun error msg = (
		if !anyErrors then ()
		else (
		  pr ["***** Bogus BOM in ", Atom.toString name, " after ", phase, " *****\n"];
		  anyErrors := true);
		pr ("** " :: msg))
	  fun cerror msg = pr ("== "::msg)
	(* match the parameter types against argument variables *)
	  fun checkArgTypes (ctx, paramTys, argTys) = let
		fun chk ([], []) = ()
		  | chk (l, []) = error[Int.toString(length l), " too few arguments in ", ctx, "\n"]
		  | chk ([], l) = error[Int.toString(length l), " too many arguments in ", ctx, "\n"]
		  | chk (pty::ptys, aty::atys) = (
		      if (BTy.match(aty, pty))
			then ()
			else (
			  error ["type mismatch in ", ctx, "\n"];
			  cerror["  expected  ", BTy.toString pty, "\n"];
			  cerror["  but found ", BTy.toString aty, "\n"]);
		      chk(ptys, atys))
		in
		  chk (paramTys, argTys)
		end
	(* a table mapping variables to their census counts *)
	  val counts = VTbl.mkTable (256, Fail "count table")
	  fun insert x = (case VTbl.find counts x
		 of SOME _ => error["multiple bindings of ", v2s x, "\n"]
		  | NONE => VTbl.insert counts (x, {
			appCnt = BV.appCntOf x,
			useCnt = BV.useCount x
		      })
		(* end case *))
	(* match a list of variables to a context *)
	  fun chkContext (cxt, xs) = (case cxt
		 of TAIL(f, tys) => checkArgTypes ("return from " ^ v2s f, tys, typesOf xs)
		  | BIND ys => checkArgTypes ("binding " ^ vl2s ys, typesOf ys, typesOf xs)
		(* end case *))
	(* create a tail context *)
	  fun tailContext f = let
		val (_, _, rng) = BTy.asFunTy(BV.typeOf f)
		in
		  TAIL(f, rng)
		end
	(* create a bind context *)
	  fun bindContext vl = BIND vl
	(* Check that a variable is bound *)
	  fun chkVar (x, ctx) = if VTbl.inDomain counts x
		then ()
		else error["unbound variable ", v2s x, " in ", ctx, "\n"]
	  fun chkVars (xs, ctx) = List.app (fn x => chkVar(x, ctx)) xs
	  fun chkBinding (x, binding) = if eqVK(BV.kindOf x, binding)
		then ()
		else error[
		    "binding of ", v2s x, " is ",
		    vkToString(BV.kindOf x), " (expected ",
		    vkToString binding, ")\n"
		  ]
	  fun chkBindings (lhs, binding) =
		List.app (fn x => chkBinding(x, binding)) lhs
(* FIXME: we should check the kind of the xs, but we don't have a kind for pattern-bound
 * variables yet!
 *)
	  fun chkPat (B.P_DCon(_, xs)) = List.app insert xs
	    | chkPat (B.P_Const _) = ()
	(* *)
	  fun insertFB (B.FB{f, ...}) = insert f
	  fun chkFB (lambda as B.FB{f, params, exh, body}) = (
		chkBinding (f, B.VK_Fun lambda);
		chkBindings (params, B.VK_Param);
		chkBindings (exh, B.VK_Param);
		List.app insert params;
		List.app insert exh;
		chkE (tailContext f, body))
	  and chkE (cxt, B.E_Pt(_, t)) = (case t
		 of B.E_Let(lhs, rhs, e) => (
		      chkBindings (lhs, B.VK_Let rhs);
		      chkE(bindContext lhs, rhs);
		      List.app insert lhs;
		      chkE(cxt, e))
		  | B.E_Stmt(lhs, rhs, e) => (
		      chkBindings (lhs, B.VK_RHS rhs);
		      chkRHS (lhs, rhs);
		      List.app insert lhs;
		      chkE(cxt, e))
		  | B.E_Fun(fbs, e) => (
		      List.app insertFB fbs;
		      chkE(cxt, e);
		      List.app chkFB fbs)
		  | B.E_Cont(fb as B.FB{f, params, exh, body}, e) => (
		      chkBinding (f, B.VK_Cont fb);
		      chkBindings (params, B.VK_Param);
		      insert f;
		      chkE(cxt, e);
		      List.app insert params;
		      if not(null exh)
			then error[
			    "continuation ", v2s f, " has non-empty return list"
			  ]
			else ();
		      chkE(cxt, body))
		  | B.E_If(x, e1, e2) => (
		      chkVar(x, "If"); chkE(cxt, e1); chkE(cxt, e2))
		  | B.E_Case(x, cases, dflt) => let
		      fun chk' (pat, e) = (chkPat pat; chkE(cxt, e))
		      in
			chkVar(x, "Case");
			List.app chk' cases;
			Option.app (fn e => chkE(cxt, e)) dflt
		      end
		  | B.E_Apply(f, args, rets) => (
		      chkVar (f, "Apply");
		      case BV.typeOf f
		       of BTy.T_Fun(argTys, exhTys, retTys) => (
			    chkVars (args, "Apply args");
			    chkVars (rets, "Apply rets");
			    checkArgTypes (concat["Apply ", v2s f, " args"], argTys, typesOf args);
			    checkArgTypes (concat["Apply ", v2s f, " rets"], exhTys, typesOf rets);
			    case cxt
			     of TAIL(g, tys) =>
				  checkArgTypes (concat["Apply ", v2s f, " in ", v2s g], tys, retTys)
			      | BIND ys =>
				  checkArgTypes (
				    concat["binding ", vl2s ys, " to Apply ", v2s f],
				    typesOf ys, retTys)
			    (* end case *))
			| ty => error[v2s f, " : ", BTy.toString ty, " is not a function"]
		      (* end case *))
		  | B.E_Throw(k, args) => (
		      chkVar(k, "Throw");
		      case BV.typeOf k
		       of BTy.T_Cont(argTys) => (
			    chkVars (args, "Throw args");
			    checkArgTypes (concat["Throw ", v2s k, " args"], argTys, typesOf args))
			| ty => error[v2s k, ":", BTy.toString ty, " is not a continuation"]
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
			checkArgTypes (concat["HLOP ", (HLOp.toString hlop), " args"], paramTys, typesOf args);
			checkArgTypes (concat["HLOP ", (HLOp.toString hlop), " rets"], exhTys, typesOf rets);
			case (returns, cxt)
			 of (true, TAIL(g, tys)) =>
			      checkArgTypes (concat["HLOP ", (HLOp.toString hlop), " in ", v2s g], tys, resTys)
			  | (true, BIND ys) =>
			      checkArgTypes (
				concat["binding ", vl2s ys, " to HLOP ", (HLOp.toString hlop)],
				typesOf ys, resTys)
			  | (false, _) => ()
			(* end case *)
		      end
		(* end case *))
	  and chkRHS (lhs, rhs) = (case (typesOf lhs, rhs)
		 of ([ty], B.E_Const(lit, ty')) => (
		      if BTy.equal(ty', ty)
			then ()
			else error[
			    "type mismatch in Const: ",  vl2s lhs, " = ", 
			    Literal.toString lit, ":", BTy.toString ty', "\n"
			  ])
		  | ([ty], B.E_Cast(ty', x)) => (
		      chkVar (x, "Cast");
		      if BTy.match(ty', ty) andalso BTy.validCast(BV.typeOf x, ty')
			then ()
			else error["type mismatch in Cast: ", 
                                 vl2s lhs, " = (", BTy.toString ty', ")", v2s x, "\n"])
		  | ([ty], B.E_Select(i, x)) => (
                      chkVar(x, "Select");
                      case BV.typeOf x
                       of BTy.T_Tuple(_, tys) => 
                             if BTy.equal(ty, List.nth (tys, i))
                                then ()
                                else error[
				    "type mismatch in Select: ",
                                     vl2s' lhs, " = #", Int.toString i, "(", v2s' x, ")\n"
				  ]
			| ty => error[v2s x, ":", BTy.toString ty, " is not a tuple: ",
                                    vl2s lhs, " = #", Int.toString i, "(", v2s x, ")\n"]
		      (* end case *))
		  | ([], B.E_Update(i, x, y)) => (
                      chkVar(x, "Update");
                      chkVar(y, "Update");
                      case BV.typeOf x
                       of BTy.T_Tuple(true, tys) => 
                              if BTy.equal(BV.typeOf y, List.nth (tys, i))
                                 then ()
                              else error["type mismatch in Update: ",
                                       "#", Int.toString i, "(", v2s x, ") := ", v2s y, "\n"]
			| ty => error[v2s x, ":", BTy.toString ty, " is not a mutable tuple",
                                    "#", Int.toString i, "(", v2s x, ") := ", v2s y, "\n"]
		      (* end case *))
		  | ([ty], B.E_AddrOf(i, x)) => (
                      chkVar(x, "AddrOf");
                      case BV.typeOf x
                       of BTy.T_Tuple(_, tys) => 
                              if BTy.equal(ty, BTy.T_Addr(List.nth (tys, i)))
                                 then ()
                              else error["type mismatch in AddrOf: ",
                                       vl2s lhs, " = &(", v2s x, ")\n"]
			| ty => error[v2s x, ":", BTy.toString ty, " is not a tuple",
                                    vl2s lhs, " = &(", v2s x, ")\n"]
		      (* end case *))
		  | ([ty], B.E_Alloc (allocTy, xs)) => (
                      chkVars(xs, "Alloc");
                      if BTy.equal (ty, allocTy) andalso
                         (BTy.equal(ty, BTy.T_Tuple(true, List.map BV.typeOf xs))
                          orelse BTy.equal(ty, BTy.T_Tuple(false, List.map BV.typeOf xs)))
                        then ()
                        else error["type mismatch in Alloc: ", vl2s lhs, " = ", vl2s xs, "\n"])
		  | ([ty], B.E_Prim p) => (
                      chkVars(PrimUtil.varsOf p, PrimUtil.nameOf p))
                  | ([ty], B.E_DCon (dcon, args)) => (
                      chkVars(args, BOMTyCon.dconName dcon))
		  | ([ty], B.E_CCall(cf, args)) => (
		      chkVar(cf, "CCall"); 
                      chkVars(args, "CCall args"))
		  | ([], B.E_CCall(cf, args)) => (
		      chkVar(cf, "CCall"); 
                      chkVars(args, "CCall args"))
		  | ([ty], B.E_HostVProc) => (
                      if BTy.equal(ty, BTy.T_VProc)
                         then ()
                         else error["type mismatch in HostVProc: ", vl2s lhs, " = host_vproc()\n"])
		  | ([ty], B.E_VPLoad(n, vp)) => (
                      chkVar(vp, "VPLoad");
                      if BTy.equal(BV.typeOf vp, BTy.T_VProc)
                         then ()
                         else error["type mismatch in VPLoad: ",
                                  vl2s lhs, " = vpload(", 
                                  IntInf.toString n, ", ", v2s vp, ")\n"])
		  | ([], B.E_VPStore(n, vp, x)) => (
		      chkVar(vp, "VPStore"); 
                      chkVar(x, "VPStore");
                      if BTy.equal(BV.typeOf vp, BTy.T_VProc)
                         then ()
                         else error["type mismatch in VPStore: ",
                                  vl2s lhs, " = vpstore(", 
                                  IntInf.toString n, ", ", v2s vp, ", ", v2s x, ")\n"])
		  | _ => error["bogus rhs for ", vl2s lhs, "\n"]
		(* end case *))
	(* check the module's main function *)
	  fun chkFB (lambda as B.FB{f, params, exh, body}) = (
		chkBinding (f, B.VK_Fun lambda);
		chkBindings (params, B.VK_Param);
		chkBindings (exh, B.VK_Param);
		List.app insert params;
		List.app insert exh;
		chkE(tailContext f, body))
	(* check an external function *)
	  fun chkExtern (CFunctions.CFun{var, name, ...}) = (
		insert var;
		case BV.kindOf var
		 of B.VK_Extern _ => ()
		  | vk => error[
			"extern ", v2s var, " has kind ", vkToString vk
		      ]
		(* end case *))
	(* check old counts against new counts *)
	  fun checkCnt (x, {appCnt, useCnt}) =
		if (appCnt <> BV.appCntOf x) orelse (useCnt <> BV.useCount x)
		  then error[
		      "inconsistent counts for ", v2s x, ": recorded <",
		      Int.toString useCnt, ":", Int.toString appCnt,
		      "> vs. actual <", Int.toString(BV.useCount x), ":",
		      Int.toString(BV.appCntOf x), ">\n"
		    ]
		  else ()
	  in
	  (* record census counts and do initial checking *)
	    List.app chkExtern externs;
	    chkFB body; insertFB body;
	  (* recompute census information *)
	    Census.census module;
	  (* check new and old census information *)
	    VTbl.appi checkCnt counts;
if !anyErrors
  then (
    print "******************** broken BOM ********************\n";
    PrintBOM.print module;
    print "********************\n";
    raise Fail "broken BOM")
  else ();
	  (* return the error status *)
	    !anyErrors
	  end
(**** Reppy version ****)

    val check = BasicControl.mkTracePass {
	    passName = "bom-check",
	    pass = check,
	    verbose = 2
	  }

  end
