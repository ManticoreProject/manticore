(* check-cps.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CheckCPS : sig

    val check : string * CPS.module -> bool

  end = struct

    structure C = CPS
    structure CV = C.Var
    structure VSet = CV.Set
    structure CTy = CPSTy
    structure CTU = CPSTyUtil

    val v2s = CV.toString
    fun vl2s xs = String.concat["(", String.concatWith "," (List.map v2s xs), ")"]
    fun v2s' x = concat[v2s x, ":", CTU.toString(CV.typeOf x)]
    fun vl2s' xs = String.concat["(", String.concatWith "," (List.map v2s' xs), ")"]

    val t2s = CTU.toString
    fun tl2s ts = concat["(", String.concatWith "," (map t2s ts), ")"]

  (* placeholder for testing variable kind equality *)
    fun eqVK _ = true

    fun vkToString C.VK_None = "VK_None"
      | vkToString (C.VK_Let rhs) =
	  concat["VK_Let(", CPSUtil.rhsToString rhs, ")"]
      | vkToString (C.VK_Fun _) = "VK_Fun"
      | vkToString (C.VK_Cont _) = "VK_Cont"
      | vkToString (C.VK_Param _) = "VK_Param"
      | vkToString (C.VK_Extern x) = concat["VK_Extern(", x, ")"]

    fun typesOf xs = List.map CV.typeOf xs

    fun check (phase, module) = let
          val C.MODULE{name, externs, body} = module 
	  val anyErrors = ref false
	(* report an error *)
	  fun pr s = TextIO.output(TextIO.stdErr, concat s)
	  fun error msg = (
		if !anyErrors then ()
		else (
		  pr ["***** Bogus CPS in ", Atom.toString name, " after ", phase, " *****\n"];
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
			  error ["type mismatch in ", ctx, "\n"];
			  cerror ["  expected  ", CTU.toString pty, "\n"];
			  cerror ["  but found ", CTU.toString aty, "\n"])
	        in 
	          if (length paramTys = length argTys)
                    then ListPair.app chk1 (paramTys, argTys)
                    else let
	            (* str : ty list -> string *)
                      fun str ts = String.concatWith "," (map CTU.toString ts)
                      in 
                        error ["wrong number of arguments in ", ctx, "\n"];
			cerror ["  expected (", str paramTys, ")\n"];
			cerror ["  found    (", str argTys, ")\n"]
                      end
	        end
	(* Check that a variable is bound *)
          fun addFB (C.FB{f, ...}, env) = VSet.add(env, f)
          fun addVars (env, xs) = VSet.addList(env, xs)
	  fun chkVar (env, x, cxt) = if VSet.member(env, x)
		then ()
		else error["unbound variable ", v2s x, " in ", cxt, "\n"]
	  fun chkVars (env, xs, cxt) = List.app (fn x => chkVar(env, x, cxt)) xs
	  fun chkBinding (x, binding) = if eqVK(CV.kindOf x, binding)
		then ()
		else error[
		    "binding of ", v2s x, " is ",
		    vkToString(CV.kindOf x), " (expected ",
		    vkToString binding, ")\n"
		  ]
	  fun chkBindings (lhs, binding) =
		List.app (fn x => chkBinding(x, binding)) lhs
(* FIXME: we should check the kind of the xs, but we don't have a kind for pattern-bound
 * variables yet!
 *)
	  fun chkExp (env, e) = (case e
		 of C.Let(lhs, rhs, e) => (
		      chkBindings (lhs, C.VK_Let rhs);
		      chkRHS(env, lhs, rhs);
		      chkExp (addVars(env, lhs), e))
		  | C.Fun(fbs, e) => let
		      val env = List.foldl addFB env fbs
		      in
			List.app (fn fb => chkFB(env, fb, C.VK_Fun)) fbs;
			chkExp(env, e)
		      end
		  | C.Cont(fb, e) => let
		      val env = addFB (fb, env)
		      in
			chkFB(env, fb, C.VK_Cont); 
                        chkExp(env, e)
		      end
		  | C.If(x, e1, e2) => (
                      chkVar(env, x, "If"); 
                      chkExp(env, e1); 
                      chkExp(env, e2))
		  | C.Switch(x, cases, dflt) => (
		      chkVar(env, x, "Switch");
                      case CV.typeOf x
                       of CTy.T_Enum wt => let
                            fun chkCase (tag, exp) = (
			           if (tag <= wt)
                                      then ()
                                      else (
                                        error ["case out of range for Switch\n"];
                                        cerror ["  expected  ", CTU.toString (CV.typeOf x), "\n"];
                                        cerror ["  but found ", Word.toString tag, "\n"]);
                                   chkExp (env, exp))
                            in
                              List.app chkCase cases;
                              Option.app (fn e => chkExp (env, e)) dflt
                            end
                        | CTy.T_Raw rt => let
                            fun chkCase (tag, exp) = 
                                   chkExp (env, exp)
                            fun chk () = (
                                   List.app chkCase cases; 
                                   Option.app (fn e => chkExp (env, e)) dflt)
                            fun bad () = (
                                   error ["type mismatch in Switch argument\n"];
                                   cerror ["  but found ", CTU.toString (CV.typeOf x), "\n"])
                            in
                              case rt
                               of RawTypes.T_Byte => chk ()
                                | RawTypes.T_Short => chk ()
                                | RawTypes.T_Int => chk ()
                                | RawTypes.T_Long => chk ()
                                | RawTypes.T_Float => bad ()
                                | RawTypes.T_Double => bad ()
                                | RawTypes.T_Vec128 => bad ()
                            end
                        | _ => (
                            error ["type mismatch in Switch argument\n"];
                            cerror ["  expected  ", "enum or raw", "\n"];
                            cerror ["  but found ", CTU.toString (CV.typeOf x), "\n"])
		      (* end case *))
		  | C.Apply(f, args, rets) => (
		      chkVar (env, f, "Apply");
		      case CV.typeOf f
		       of CTy.T_Fun(argTys, retTys) => (
			    chkVars (env, args, "Apply args");
			    chkVars (env, rets, "Apply rets");
			    checkArgTypes (CTU.match, concat["Apply ", v2s f, " args"], argTys, typesOf args);
			    checkArgTypes (CTU.match, concat["Apply ", v2s f, " rets"], retTys, typesOf rets))
			| ty => error[v2s f, ":", CTU.toString ty, " is not a function\n"]
		      (* end case *))
		  | C.Throw(k, args) => (
		      chkVar (env, k, "Throw");
		      case CV.typeOf k
		       of CTy.T_Fun(argTys, []) => (
			    chkVars (env, args, "Throw args");
			    checkArgTypes (CTU.match, concat["Throw " ^ v2s k, " args"], argTys, typesOf args))
			| ty => error[v2s k, ":", CTU.toString ty, " is not a continuation\n"]
		      (* end case *))
		(* end case *))
	  and chkRHS (env, lhs, rhs) = (case (List.map CV.typeOf lhs, rhs)
		 of (tys, C.Var xs) => chkVars(env, xs, "Var")
		  | ([ty], C.Const(lit, ty')) => (
		    (* first, check the literal against ty' *)
		      case (lit, ty')
		       of (Literal.Enum _, CTy.T_Enum _) => ()
(* NOTE: the following shouldn't be necessary, but case-simplify doesn't put in enum types! *)
			| (Literal.Enum _, CTy.T_Any) => ()
			| (Literal.StateVal w, _) => () (* what is the type of StateVals? *)
			| (Literal.Tag s, _) => () (* what is the type of Tags? *)
			| (Literal.Int _, CTy.T_Raw CTy.T_Byte) => ()
			| (Literal.Int _, CTy.T_Raw CTy.T_Short) => ()
			| (Literal.Int _, CTy.T_Raw CTy.T_Int) => ()
			| (Literal.Int _, CTy.T_Raw CTy.T_Long) => ()
			| (Literal.Float _, CTy.T_Raw CTy.T_Float) => ()
			| (Literal.Float _, CTy.T_Raw CTy.T_Double) => ()
			| (Literal.Char _, CTy.T_Raw CTy.T_Int) => ()
			| (Literal.String _, CTy.T_Any) => ()
			| _ => error[
			    "literal has bogus type: ",  vl2s lhs, " = ", 
			    Literal.toString lit, ":", CTU.toString ty', "\n"
			    ]
		      (* end case *);
		    (* then check ty' against ty *)
		      if CTU.equal(ty', ty)
			then ()
			else error[
			    "type mismatch in Const: ",  vl2s lhs, " = ", 
			    Literal.toString lit, ":", CTU.toString ty', 
			    " (* expected ", CTU.toString ty, " *)\n"
			  ])
		  | ([ty], C.Cast(ty', x)) => (
		      chkVar (env, x, "Cast");
		      if CTU.match(ty', ty) andalso CTU.validCast(CV.typeOf x, ty')
			then ()
			else error[
                            "type mismatch in Cast: ", vl2s' lhs, " = (", CTU.toString ty', 
                            ")(", v2s' x, ")\n"
                          ])
		  | ([ty], C.Select(i, x)) => (
                      chkVar(env, x, "Select");
                      case CV.typeOf x
                       of CTy.T_Tuple(_, tys) => 
			    if (i < List.length tys) andalso CTU.match(List.nth (tys, i), ty)
			      then ()
			      else error[
				  "type mismatch in Select: ",
				   vl2s' lhs, " = #", Int.toString i, "(", v2s' x, ")\n"
				]
			| ty => error[v2s x, ":", CTU.toString ty, " is not a tuple: ",
                                    vl2s lhs, " = #", Int.toString i, "(", v2s x, ")\n"]
		      (* end case *))
		  | ([], C.Update(i, x, y)) => (
                      chkVar(env, x, "Update");
                      chkVar(env, y, "Update");
                      case CV.typeOf x
                       of CTy.T_Tuple(true, tys) => 
			    if (i < List.length tys) andalso CTU.equal(CV.typeOf y, List.nth (tys, i))
			      then ()
			      else error["type mismatch in Update: ",
				     "#", Int.toString i, "(", v2s x, ") := ", v2s y, "\n"]
			| ty => error[v2s x, ":", CTU.toString ty, " is not a mutable tuple: ",
                                    "#", Int.toString i, "(", v2s x, ") := ", v2s y, "\n"]
		      (* end case *))
		  | ([ty], C.AddrOf(i, x)) => (
                      chkVar(env, x, "AddrOf");
                      case CV.typeOf x
                       of CTy.T_Tuple(_, tys) => 
			    if (i < List.length tys) andalso CTU.match(CTy.T_Addr(List.nth (tys, i)), ty)
                              then ()
                              else error["type mismatch in AddrOf: ", vl2s lhs, " = &(", v2s x, ")\n"]
			| CTy.T_VProc => 
			  (* allow programs to take offsets from the vproc structure for atomic ops *)
			  ()
			| ty => error[v2s x, ":", CTU.toString ty, " is not a tuple: ",
                                    vl2s lhs, " = &(", v2s x, ")\n"]
		      (* end case *))
		  | ([ty], C.Alloc(ty', xs)) => (
                      chkVars(env, xs, "Alloc");
(* FIXME: check ty' too *)
                      if (CTU.match(CTy.T_Tuple(true, typesOf xs), ty))
                        orelse (CTU.match(CTy.T_Tuple(false, typesOf xs), ty))
                        then ()
                        else (error  ["type mismatch in Alloc: ", vl2s lhs, " = ", vl2s xs, "\n"];
			      cerror ["  lhs type ", t2s ty, "\n"];
			      cerror ["  found    ", tl2s (typesOf xs), "\n"]))
		  | ([ty], C.Promote x) => (
                      chkVar(env, x, "Promote");
		      if (CTU.equal(ty, CV.typeOf x))
			then ()
			else error ["type mismatch in Promote: ", vl2s lhs, " = ", v2s x, "\n"])
		  | ([], C.Prim p) => (
                      chkVars(env, PrimUtil.varsOf p, PrimUtil.nameOf p))
		  | ([ty], C.Prim p) => (
                      chkVars(env, PrimUtil.varsOf p, PrimUtil.nameOf p))
		  | ([ty], C.CCall(cf, args)) => (
		      chkVar(env, cf, "CCall"); 
                      chkVars(env, args, "CCall args"))
		  | ([], C.CCall(cf, args)) => (
		      chkVar(env, cf, "CCall"); 
                      chkVars(env, args, "CCall args"))
		  | ([ty], C.HostVProc) => (
                      if CTU.match(CTy.T_VProc, ty)
                         then ()
                         else error["type mismatch in HostVProc: ", vl2s lhs, " = host_vproc()\n"])
		  | ([ty], C.VPLoad(n, vp)) => (
                      chkVar(env, vp, "VPLoad");
                      if CTU.equal(CV.typeOf vp, CTy.T_VProc)
                         then ()
                         else error["type mismatch in VPLoad: ",
                                  vl2s lhs, " = vpload(", 
                                  IntInf.toString n, ", ", v2s vp, ")\n"])
		  | ([], C.VPStore(n, vp, x)) => (
		      chkVar(env, vp, "VPStore"); 
                      chkVar(env, x, "VPStore");
                      if CTU.equal(CV.typeOf vp, CTy.T_VProc)
                         then ()
                         else error["type mismatch in VPStore: ",
                                  vl2s lhs, " = vpstore(", 
                                  IntInf.toString n, ", ", v2s vp, ", ", v2s x, ")\n"])
		  | _ => error["bogus rhs for ", vl2s lhs, "\n"]
		(* end case *))
	  and chkFB (env, fb as C.FB{f, params, rets, body}, vk) = (let
                val (argTys, retTys) =
                      case CV.typeOf f
                       of CTy.T_Fun(argTys, retTys) =>
                              (argTys, retTys)
                        | ty => (error["expected function/continuation type for ",
                                       v2s f, ":", CTU.toString(CV.typeOf f), "\n"];
                                 ([],[]))
                      (* end case *)
                in
                chkBinding (f, vk fb);
                chkBindings (params, C.VK_Param fb);
                checkArgTypes(CTU.equal, concat["Fun ", v2s f, " params"], argTys, typesOf params);
                chkBindings (rets, C.VK_Param fb);
                checkArgTypes(CTU.equal, concat["Fun ", v2s f, " rets"], retTys, typesOf rets);
		chkExp (addVars(addVars(env, params), rets), body)
                end)
	  val env = List.foldl
		(fn (cf, env) => VSet.add(env, CFunctions.varOf cf))
		  VSet.empty externs
	  in
	    chkFB (env, body, C.VK_Fun);
if !anyErrors
  then (
    print "******************** broken CPS ********************\n";
    PrintCPS.print module;
    print "********************\n";
    raise Fail "broken CPS")
  else ();
	  (* return the error status *)
	    !anyErrors
	  end (* check *)

    val check =
       BasicControl.mkTracePass
       {passName = "cps-check",
        pass = check,
        verbose = 2}
  end
