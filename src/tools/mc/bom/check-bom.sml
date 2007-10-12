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
        (* checkArgTypes : string * ty list * ty list -> unit *)
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
		 of TAIL(f, tys) => checkArgTypes (BTU.match, "return from " ^ v2s f, tys, typesOf xs)
		  | BIND ys => checkArgTypes (BTU.match, "binding " ^ vl2s ys, typesOf ys, typesOf xs)
		(* end case *))
	(* create a tail context *)
	  fun tailContext f = let
		val (_, _, rng) = BTU.asFunTy(BV.typeOf f)
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
	  fun chkFB (lambda as B.FB{f, params, exh, body}) = (let
                val (argTys, exhTys, retTys) =
                      case BV.typeOf f
                       of BTy.T_Fun(argTys, exhTys, retTys) =>
                              (argTys, exhTys, retTys)
                        | BTy.T_Cont(argTys) =>
                              (argTys, [], [])
                        | ty => (error["expected function/continuation type for ",
                                       v2s f, ":", BTU.toString(BV.typeOf f)];
                                 ([],[],[]))
                      (* end case *)
                in
		chkBinding (f, B.VK_Fun lambda);
		chkBindings (params, B.VK_Param);
                checkArgTypes(BTU.equal, concat["Fun ", v2s f, " params"], argTys, typesOf params);
		chkBindings (exh, B.VK_Param);
                checkArgTypes(BTU.equal, concat["Fun ", v2s f, " exh"], exhTys, typesOf exh);
		List.app insert params;
		List.app insert exh;
		chkE (tailContext f, body)
                end)
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
			    checkArgTypes (BTU.match, concat["Apply ", v2s f, " args"], argTys, typesOf args);
			    checkArgTypes (BTU.match, concat["Apply ", v2s f, " rets"], exhTys, typesOf rets);
			    case cxt
			     of TAIL(g, tys) =>
				  checkArgTypes (BTU.match, concat["Apply ", v2s f, " in ", v2s g], tys, retTys)
			      | BIND ys =>
				  checkArgTypes (
                                    BTU.match, 
				    concat["binding ", vl2s ys, " to Apply ", v2s f],
				    typesOf ys, retTys)
			    (* end case *))
			| ty => error[v2s f, " : ", BTU.toString ty, " is not a function\n"]
		      (* end case *))
		  | B.E_Throw(k, args) => (
		      chkVar(k, "Throw");
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
			  | (true, BIND ys) =>
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
		      if BTU.equal(ty', ty)
			then ()
			else error[
			    "type mismatch in Const: ",  vl2s lhs, " = ", 
			    Literal.toString lit, ":", BTU.toString ty', 
			    " (* expected ", BTU.toString ty, " *)\n"
			  ])
		  | ([ty], B.E_Cast(ty', x)) => (
		      chkVar (x, "Cast");
		      if BTU.match(ty', ty) andalso BTU.validCast(BV.typeOf x, ty')
			then ()
			else error["type mismatch in Cast: ", 
                                 vl2s lhs, " = (", BTU.toString ty', ")(", v2s' x, ")\n"])
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
			    if BTU.equal(BV.typeOf y, List.nth (tys, i))
			      then ()
			      else error["type mismatch in Update: ",
				     "#", Int.toString i, "(", v2s x, ") := ", v2s y, "\n"]
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
		  | ([ty], B.E_Alloc (allocTy, xs)) => (
                      chkVars(xs, "Alloc");
                      if BTU.match (allocTy, ty)
			then ()
			else (error  ["type mismatch in Alloc: ", vl2s lhs, " = ", vl2s xs, "\n"];
			      cerror ["  lhs type ", t2s ty, "\n"];
			      cerror ["  rhs type ", t2s allocTy, "\n"]);
		      if (BTU.match(BTy.T_Tuple(true, typesOf xs), allocTy)
			orelse BTU.match(BTy.T_Tuple(false, typesOf xs), allocTy))
                        then ()
                        else (error  ["type mismatch in Alloc: ", vl2s lhs, " = ", vl2s xs, "\n"];
			      cerror ["  expected ", t2s allocTy, "\n"];
			      cerror ["  found    ", tl2s (typesOf xs), "\n"]))
		  | ([ty], B.E_GAlloc (allocTy, xs)) => (
                      chkVars(xs, "GAlloc");
                      if BTU.match (allocTy, ty)
			then ()
			else (error  ["type mismatch in GAlloc: ", vl2s lhs, " = ", vl2s xs, "\n"];
			      cerror ["  lhs type ", t2s ty, "\n"];
			      cerror ["  rhs type ", t2s allocTy, "\n"]);
		      if (BTU.match(BTy.T_Tuple(true, typesOf xs), allocTy)
			orelse BTU.match(BTy.T_Tuple(false, typesOf xs), allocTy))
                        then ()
                        else (error  ["type mismatch in GAlloc: ", vl2s lhs, " = ", vl2s xs, "\n"];
			      cerror ["  expected ", t2s allocTy, "\n"];
			      cerror ["  found    ", tl2s (typesOf xs), "\n"]))
		  | ([ty], B.E_Promote y) => (
		      chkVar (y, "Promote");
		      if BTU.equal(ty, BV.typeOf y) then ()
			else error ["type mismatch in Promote: ", vl2s lhs, " = ", v2s y, "\n"])
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
                      if BTU.match(BTy.T_VProc, ty)
                         then ()
                         else error["type mismatch in HostVProc: ", vl2s lhs, " = host_vproc()\n"])
		  | ([ty], B.E_VPLoad(n, vp)) => (
                      chkVar(vp, "VPLoad");
                      if BTU.equal(BV.typeOf vp, BTy.T_VProc)
                         then ()
                         else error["type mismatch in VPLoad: ",
                                  vl2s lhs, " = vpload(", 
                                  IntInf.toString n, ", ", v2s vp, ")\n"])
		  | ([], B.E_VPStore(n, vp, x)) => (
		      chkVar(vp, "VPStore"); 
                      chkVar(x, "VPStore");
                      if BTU.equal(BV.typeOf vp, BTy.T_VProc)
                         then ()
                         else error["type mismatch in VPStore: ",
                                  vl2s lhs, " = vpstore(", 
                                  IntInf.toString n, ", ", v2s vp, ", ", v2s x, ")\n"])
		  | _ => error["bogus rhs for ", vl2s lhs, "\n"]
		(* end case *))
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
		  then (* error[
		      "inconsistent counts for ", v2s x, ": recorded <",
		      Int.toString useCnt, ":", Int.toString appCnt,
		      "> vs. actual <", Int.toString(BV.useCount x), ":",
		      Int.toString(BV.appCntOf x), ">\n"
		    ] *) ()
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

    val check = BasicControl.mkTracePass {
	    passName = "bom-check",
	    pass = check,
	    verbose = 2
	  }

  end
