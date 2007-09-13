(* check-cps.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CheckCPS : sig

    val check : CPS.module -> bool

  end = struct

    structure C = CPS
    structure V = C.Var
    structure Ty = CPSTy
    structure VSet = V.Set

    val v2s = V.toString
    fun vl2s [] = "()"
      | vl2s [x] = v2s x
      | vl2s (x::xs) = let
	  fun f (x, l) = "," :: v2s x :: l
	  in
	    String.concat("[" :: v2s x :: List.foldr f ["]"] xs)
	  end

    fun addFB (C.FB{f, ...}, env) = VSet.add(env, f)

    fun addVars (env, xs) = VSet.addList(env, xs)

    fun check (C.MODULE{name, externs, body}) = let
	  val anyErr = ref false
	  fun err msg = (
		anyErr := true;
		print(concat("Error: " :: msg));
		print "\n")
	  fun cerr msg = print(concat("== "::msg))
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
		      if (Ty.match(V.typeOf x, ty))
			then ()
			else err[
			    "type mismatch in ", cxt, "\n  expected  ", Ty.toString ty,
			    "\n  but found ", v2s x, ":", Ty.toString(V.typeOf x)
			  ];
		      chk(tys, xs))
		in
		  chk (paramTys, args)
		end
	  fun chkExp (env, e) = (case e
		 of C.Let(lhs, rhs, e) => (
		      chkRHS(env, lhs, rhs);
		      chkExp (addVars(env, lhs), e))
		  | C.Fun(fbs, e) => let
		      val env = List.foldl addFB env fbs
		      in
			List.app (fn fb => chkFB(env, fb)) fbs;
			chkExp(env, e)
		      end
		  | C.Cont(fb, e) => let
		      val env = addFB (fb, env)
		      in
			chkFB(env, fb); 
                        chkExp(env, e)
		      end
		  | C.If(x, e1, e2) => (
                      chkVar(env, x, "If"); 
                      chkExp(env, e1); 
                      chkExp(env, e2))
		  | C.Switch(x, cases, dflt) => (
		      chkVar(env, x, "Switch");
		      List.app (fn (_, e) => chkExp (env, e)) cases;
		      Option.app (fn e => chkExp (env, e)) dflt)
		  | C.Apply(f, args, rets) => (
		      chkVar (env, f, "Apply");
		      case V.typeOf f
		       of Ty.T_Fun(argTys, retTys) => (
			    chkVars (env, args, "Apply args");
			    chkVars (env, rets, "Apply rets");
			    checkArgs (argTys, args, concat["Apply ", v2s f, " args"]);
			    checkArgs (retTys, rets, concat["Apply ", v2s f, " rets"]))
			| ty => err[v2s f, ":", Ty.toString ty, " is not a function"]
		      (* end case *))
		  | C.Throw(k, args) => (
		      chkVar (env, k, "Throw");
		      case V.typeOf k
		       of Ty.T_Fun(argTys, []) => (
			    chkVars (env, args, "Throw args");
			    checkArgs (argTys, args, "Throw " ^ v2s k))
			| ty => err[v2s k, ":", Ty.toString ty, " is not a continuation"]
		      (* end case *))
		(* end case *))
	  and chkRHS (env, lhs, rhs) = (case (List.map V.typeOf lhs, rhs)
		 of (tys, C.Var xs) => chkVars(env, xs, "Var")
		  | ([ty], C.Cast(ty', x)) => (
		      chkVar (env, x, "Cast");
		      if Ty.match(ty', ty) andalso Ty.validCast(V.typeOf x, ty')
			then ()
			else err["type mismatch in Cast: ",
                                 vl2s lhs, " = (", Ty.toString ty', ")", v2s x])
		  | ([ty], C.Const(lit, ty')) => (
		      if Ty.equal(ty', ty)
			then ()
                        else err["type mismatch in Const: ",
                                 vl2s lhs, " = ", 
                                 Literal.toString lit, ":", Ty.toString ty'])
		  | ([ty], C.Select(i, x)) => (
                      chkVar(env, x, "Select");
                      case V.typeOf x
                       of Ty.T_Tuple(_, tys) => 
                            if Ty.equal(ty, List.nth (tys, i))
                               then ()
                               else err["type mismatch in Select: ",
                                         vl2s lhs, " = #", Int.toString i, 
                                         "(", v2s x, ")"]
			| ty => err[v2s x, ":", Ty.toString ty, " is not a tuple: ",
                                    vl2s lhs, " = #", Int.toString i, "(", v2s x, ")"]
		      (* end case *))
		  | ([], C.Update(i, x, y)) => (
                      chkVar(env, x, "Update");
                      chkVar(env, y, "Update");
                      case V.typeOf x
                       of Ty.T_Tuple(true, tys) => 
                            if Ty.equal(V.typeOf y, List.nth (tys, i))
                               then ()
                               else err["type mismatch in Update: ",
                                         vl2s lhs, " = #", Int.toString i, 
                                         "(", v2s x, ",", v2s y, ")"]
			| ty => err[v2s x, ":", Ty.toString ty, " is not a mutable tuple: ",
                                    vl2s lhs, " = #", Int.toString i, "(", v2s x, ",", v2s y, ")"]
		      (* end case *))
		  | ([ty], C.AddrOf(i, x)) => (
                      chkVar(env, x, "AddrOf");
                      case V.typeOf x
                       of Ty.T_Tuple(_, tys) => 
                            if Ty.equal(ty, Ty.T_Addr(List.nth (tys, i)))
                               then ()
                               else err["type mismatch in AddrOf: ",
                                        vl2s lhs, " = &(", v2s x, ")"]
			| ty => err[v2s x, ":", Ty.toString ty, " is not a tuple: ",
                                        vl2s lhs, " = &(", v2s x, ")"]
		      (* end case *))
		  | ([ty], C.Alloc xs) => (
                      chkVars(env, xs, "Alloc");
                      if Ty.equal(ty, Ty.T_Tuple(true, List.map V.typeOf xs))
                         orelse Ty.equal(ty, Ty.T_Tuple(false, List.map V.typeOf xs))
                        then ()
                        else let
                        (* tstr : var list -> string *)
		          val tstr = (String.concatWith ", ") o map (Ty.toString o V.typeOf)
			  in
			    err["type mismatch in Alloc: ", vl2s lhs, " = ", vl2s xs];
			    cerr["  expected ", tstr lhs, "\n"];
			    cerr["  found    ", tstr xs, "\n"]
			  end)
		  | ([ty], C.Wrap x) => (
                      chkVar(env, x, "Wrap");
                      case V.typeOf x
                       of Ty.T_Raw rt => 
                            if Ty.equal(ty, Ty.T_Wrap rt)
                               then ()
                               else err["type mismatch in Wrap: ",
                                        vl2s lhs, " = wrap(", v2s x, ")"]
			| ty => err[v2s x, ":", Ty.toString ty, " is not a raw: ",
                                    vl2s lhs, " = wrap(", v2s x, ")"]
		      (* end case *))
		  | ([ty], C.Unwrap x) => (
                      chkVar(env, x, "Unwrap");
                      case V.typeOf x
                       of Ty.T_Wrap rt => 
                            if Ty.equal(ty, Ty.T_Raw rt)
                               then ()
                               else err["type mismatch in Unwrap:", 
                                        vl2s lhs, " = unwrap(", v2s x, ")"]
			| ty => err[v2s x, ":", Ty.toString ty, " is not a wrap: ",
                                    vl2s lhs, " = unwrap(", v2s x, ")"]
		      (* end case *))
		  | ([ty], C.Prim p) => chkVars(env, PrimUtil.varsOf p, PrimUtil.nameOf p)
		  | ([ty], C.CCall(cf, args)) => (
		      chkVar(env, cf, "CCall"); 
                      chkVars(env, args, "CCall args"))
		  | ([], C.CCall(cf, args)) => (
		      chkVar(env, cf, "CCall"); 
                      chkVars(env, args, "CCall args"))
		  | ([ty], C.HostVProc) => (
                      if Ty.equal(ty, Ty.T_VProc)
                         then ()
                         else err["type mismatch in HostVProc: ",
                                  vl2s lhs, " = host_vproc()"])
		  | ([ty], C.VPLoad(n, vp)) => (
                      chkVar(env, vp, "VPLoad");
                      if Ty.equal(V.typeOf vp, Ty.T_VProc)
                         then ()
                         else err["type mismatch in VPLoad: ",
                                  vl2s lhs, " = vpload(", 
                                  IntInf.toString n, ", ", v2s vp, ")"])
		  | ([], C.VPStore(n, vp, x)) => (
		      chkVar(env, vp, "VPStore"); 
                      chkVar(env, x, "VPStore");
                      if Ty.equal(V.typeOf vp, Ty.T_VProc)
                         then ()
                         else err["type mismatch in VPStore: ",
                                  vl2s lhs, " = vpstore(", 
                                  IntInf.toString n, ", ", v2s vp, ", ", v2s x, ")"])
		  | _ => err["bogus rhs for ", vl2s lhs]
		(* end case *))
	  and chkFB (env, fb as C.FB{f, params, rets, body}) = (
		case V.typeOf f
		 of Ty.T_Fun(argTys, retTys) => let
		      fun chk ([], []) = ()
			| chk (_, []) = err["too few parameters in ", v2s f]
			| chk ([], _) = err["too many parameters in ", v2s f]
			| chk (ty::tys, x::xs) = (
			    if (Ty.equal(V.typeOf x, ty))
			      then ()
			      else err[
				  "type mismatch in ", v2s f, "\n  expected  ", Ty.toString ty,
				  "\n  but found ", v2s x, ":", Ty.toString(V.typeOf x)
				];
			    chk(tys, xs))
		      in
			chk(argTys, params);
			chk(retTys, rets)
		      end
		  | ty => err[
			"expected function/continuation type for ",
			v2s f, ":", Ty.toString(V.typeOf f)
		      ]
		(* end case *);
		chkExp (addVars(addVars(env, params), rets), body))
	  val env = List.foldl
		(fn (cf, env) => VSet.add(env, CFunctions.varOf cf))
		  VSet.empty externs
	  in
	    chkFB (env, body);
	    !anyErr
	  end (* check *)

    val check =
       BasicControl.mkTracePass
       {passName = "CPSCheck",
        pass = check,
        verbose = 2}
  end
