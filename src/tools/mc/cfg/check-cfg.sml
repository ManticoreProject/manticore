(* check-cfg.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Check a CFG module for well-formedness
 *
 * TODO:
 *	Check that T_Addr does not appear in tuple or open-tuple types.
 *)

structure CheckCFG : sig

    val check : string * CFG.module -> bool

  end = struct

    structure V = CFG.Var
    structure L = CFG.Label
    structure VSet = CFG.Var.Set
    structure LSet = CFG.Label.Set
    structure LMap = CFG.Label.Map
    structure Ty = CFGTy
    structure TyU = CFGTyUtil
    structure Lit = Literal

    val v2s = V.toString
    fun vl2s xs = String.concat["(", String.concatWith "," (List.map v2s xs), ")"]
    fun v2s' x = concat[v2s x, ":", TyU.toString(V.typeOf x)]
    fun vl2s' xs = String.concat["(", String.concatWith "," (List.map v2s' xs), ")"]

    val l2s = L.toString
    fun l2s' l = concat[l2s l, ":", TyU.toString(L.typeOf l)]

    val t2s = TyU.toString
    fun tl2s ts = concat["(", String.concatWith "," (map t2s ts), ")"]

  (* placeholder for testing variable kind equality *)
    fun eqVK _ = true

    fun vkToString CFG.VK_None = "VK_None"
      | vkToString (CFG.VK_Let _) =
	  concat["VK_Let(", "_", ")"]
      | vkToString (CFG.VK_Param _) = "VK_Param"

    fun typesOf xs = List.map V.typeOf xs

    fun check (phase, module) = let
          val CFG.MODULE{name, externs, code} = module
	  val anyErrors = ref false
	(* report an error *)
	  fun pr s = TextIO.output(TextIO.stdErr, concat s)
	  fun warning msg =
		if !anyErrors then ()
		else (
		  pr ["***** Bogus CFG in ", Atom.toString name, " after ", phase, " *****\n"];
		  pr ("** " :: msg))
	  fun error msg = (
		if !anyErrors then ()
		else (
		  pr ["***** Bogus CFG in ", Atom.toString name, " after ", phase, " *****\n"];
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
			  cerror ["  expected  ", TyU.toString pty, "\n"];
			  cerror ["  but found ", TyU.toString aty, "\n"])
	        in 
	          if (length paramTys = length argTys)
                    then ListPair.app chk1 (paramTys, argTys)
                    else let
	            (* str : ty list -> string *)
                      fun str ts = String.concatWith "," (map TyU.toString ts)
                      in 
                        error ["wrong number of arguments in ", ctx, "\n"];
			cerror ["  expected (", str paramTys, ")\n"];
			cerror ["  found    (", str argTys, ")\n"]
                      end
	        end
	(* construct a set of the bound labels in the module *)
	  val lEnv = List.foldl
		(fn (f as CFG.FUNC{lab, ...}, lset) => LSet.add(lset, lab))
		  LSet.empty code
	(* Check that a variable is bound *)
          fun addVar (env, x) = VSet.add(env, x)
          fun addVars (env, xs) = VSet.addList(env, xs)
	  fun chkVar (env, x, cxt) = if VSet.member(env, x)
		then ()
		else error["unbound variable ", v2s x, " in ", cxt, "\n"]
	  fun chkVars (env, xs, cxt) = List.app (fn x => chkVar(env, x, cxt)) xs
	  fun chkBinding (x, binding) = if eqVK(V.kindOf x, binding)
		then ()
		else error[
		    "binding of ", v2s x, " is ",
		    vkToString(V.kindOf x), " (expected ",
		    vkToString binding, ")\n"
		  ]
	  fun chkBindings (lhs, binding) =
		List.app (fn x => chkBinding(x, binding)) lhs
          fun chkLbl (env, l, cxt) = (case (L.kindOf l, LSet.member(env, l))
                 of (CFG.LK_None, _) => error["no kind label ", l2s l, " in ", cxt, "\n"]
                  | (CFG.LK_Extern _, false) => ()
                  | (CFG.LK_Extern _, true) => error["extern local label ", l2s l, " in ", cxt, "\n"]
                  | (CFG.LK_Local _, true) => ()
                  | (CFG.LK_Local _, false) => error["unbound label ", l2s l, " in ", cxt, "\n"]
                (* end case *))
	(* check the entry against the declared type of the label;  The declared type is 
         * allowed to be more specific.
         *)
          fun chkEntry (lab, entry) = (case (entry, L.typeOf lab) 
                 of (CFG.StdFunc {clos, args, ret, exh},
                     Ty.T_StdFun {clos = closTy, args = argTys, ret = retTy, exh = exhTy}) => (
                      checkArgTypes(TyU.equal, concat["StdFun ", l2s lab, " clos"],
                                    [closTy], typesOf [clos]);
                      checkArgTypes(TyU.equal, concat["StdFun ", l2s lab, " args"],
                                    argTys, typesOf args);
                      checkArgTypes(TyU.equal, concat["StdFun ", l2s lab, " ret"],
                                    [retTy], typesOf [ret]);
                      checkArgTypes(TyU.equal, concat["StdFun ", l2s lab, " exh"],
                                    [exhTy], typesOf [exh]);
                      addVars(VSet.empty, clos::ret::exh::args))
                  | (CFG.StdCont {clos, args},
                     Ty.T_StdCont {clos = closTy, args = argTys}) => (
                      checkArgTypes(TyU.equal, concat["StdCont ", l2s lab, " clos"],
                                    [closTy], typesOf [clos]);
                      checkArgTypes(TyU.equal, concat["StdCont ", l2s lab, " args"],
                                    argTys, typesOf args);
                      addVars(VSet.empty, clos::args))
                  | (CFG.KnownFunc {clos, args}, 
                     Ty.T_KnownFunc {clos = closTy, args = argTys}) => (
                      checkArgTypes(TyU.equal, concat["KnownFunc ", l2s lab, " clos"],
                                    [closTy], typesOf [clos]);
                      checkArgTypes(TyU.equal, concat["KnownFunc ", l2s lab, " args"],
                                    argTys, typesOf args);
                      addVars(VSet.empty, clos::args))
                  | (CFG.Block {args}, Ty.T_Block {args = argTys}) => (
                      checkArgTypes(TyU.equal, concat["Block ", l2s lab, " args"],
                                    argTys, typesOf args);
                      addVars(VSet.empty, args))
                  | (conv, ty) => (
                      error["entry of ", l2s lab, " is ", 
                            (case conv 
                              of CFG.StdFunc _ => "stdfunc"
                               | CFG.StdCont _ => "stdcont"
                               | CFG.KnownFunc _ => "known"
                               | CFG.Block _ => "block"
                             (* end case *)), 
                            " (expected ", TyU.toString ty, ")\n"];
                      addVars(VSet.empty, CFG.paramsOfConv conv))
                (* end case *))
          fun chkExp (env, exp) = (case exp 
                 of CFG.E_Var (xs, ys) => (
                      chkVars (env, ys, "Var");
                      checkArgTypes (TyU.equal, "Var", typesOf xs, typesOf ys);
                      addVars (env, xs))
                  | CFG.E_Const (x, lit, ty) => let
                      fun err () = error[
                             "type mismatch in Const: ", v2s' x, " = ",
                             Literal.toString lit, "\n"]
                      in 
		      (* first, check the literal against ty *)
			case (lit, ty)
			 of (Literal.Enum _, Ty.T_Enum _) => ()
(* NOTE: the following shouldn't be necessary, but case-simplify doesn't put in enum types! *)
			  | (Literal.Enum _, Ty.T_Any) => ()
			  | (Literal.StateVal w, _) => () (* what is the type of StateVals? *)
			  | (Literal.Tag s, _) => () (* what is the type of Tags? *)
			  | (Literal.Int _, Ty.T_Raw Ty.T_Byte) => ()
			  | (Literal.Int _, Ty.T_Raw Ty.T_Short) => ()
			  | (Literal.Int _, Ty.T_Raw Ty.T_Int) => ()
			  | (Literal.Int _, Ty.T_Raw Ty.T_Long) => ()
			  | (Literal.Float _, Ty.T_Raw Ty.T_Float) => ()
			  | (Literal.Float _, Ty.T_Raw Ty.T_Double) => ()
			  | (Literal.Char _, Ty.T_Raw Ty.T_Int) => ()
			  | (Literal.String _, Ty.T_Any) => ()
			  | _ => error[
			      "literal has bogus type: ",  v2s x, " = ", 
			      Literal.toString lit, ":", TyU.toString ty, "\n"
			      ]
			(* end case *);
		      (* then check ty against x *)
			if TyU.equal(ty, V.typeOf x)
			  then ()
			  else err ();
			addVar (env, x)
                      end
                  | CFG.E_Cast (x, ty', y) => let
                      fun err () = error[
                             "type mismatch in Cast: ", v2s' x, " = ",
                             "(", TyU.toString ty', ")(", v2s' y, ")\n"];
                      in
			chkVar (env, y, "Cast");
			if TyU.match(ty', V.typeOf x) andalso TyU.validCast (V.typeOf y, ty')
			  then ()
			  else err ();
			addVar (env, x)
                      end
                  | CFG.E_Label (x, l) => let
                      fun err () = error[
                             "type mismatch in Label: ", v2s' x, " = ", l2s' l, "\n"]
                      in
			chkLbl (lEnv, l, "Label");
			case L.kindOf l
			 of CFG.LK_None => error["no kind label ", l2s l, " in Label\n"]
			  | CFG.LK_Extern _ => ()
			  | CFG.LK_Local _ => if TyU.match (V.typeOf x, L.typeOf l)
			      then ()
			      else err ()
			(* end case *);
			addVar (env, x)
                      end
                  | CFG.E_Select (x, i, y) => let
                      fun err () = error[
                             "type mismatch in Select: ", v2s' x, " = ",
                             "#", Int.toString i, "(", v2s' y, ")\n"]
                      in
			chkVar (env, y, "Select");
			
			case V.typeOf y
			 of Ty.T_Tuple(_, tys) =>
			      if (i < List.length tys) andalso TyU.match (List.nth (tys, i), V.typeOf x)
				then ()
				else err ()
			  | Ty.T_OpenTuple(tys) =>
			      if (i < List.length tys) andalso TyU.match (List.nth (tys, i), V.typeOf x)
				then ()
				else err ()
			  | _ => err ()
			(* end case *);
			addVar (env, x)
                      end
                  | CFG.E_Update (i, y, z) => let
                      fun err () = error[
                             "type mismatch in Update: ",
                             "#", Int.toString i, "(", v2s' y, ") := ", v2s' z, "\n"]
                      in
			chkVar (env, y, "Update");
			chkVar (env, z, "Update");
			case V.typeOf y
			 of Ty.T_Tuple(true, tys) => 
			      if (i < List.length tys) andalso TyU.equal (V.typeOf z, List.nth (tys, i))
				then ()
				else err ()
			  | ty => err ()
			(* end case *);
			env
                      end
                  | CFG.E_AddrOf (x, i, y) => let
                      fun err () = error [
                             "type mismatch in AddrOf: ", v2s' x, " = ",
                             "&(", v2s' y, ")\n"]
                      in
			chkVar (env, y, "AddrOf");
			case V.typeOf y
			 of Ty.T_Tuple(_, tys) =>
			      if (i < List.length tys) andalso TyU.match (Ty.T_Addr(List.nth (tys, i)), V.typeOf x)
				then ()
				else err ()
			  | Ty.T_VProc => 
			    (* allow programs to take offsets from the vproc structure for atomic ops *)
			    ()
			  | _ => err ()
			(* end case *);
			addVar (env, x)
                      end
                  | CFG.E_Alloc(x, ty, ys) => (
		      chkVars (env, ys, "Alloc");
		      case ty
		       of Ty.T_Tuple(isMut, tys) =>
			    if (TyU.match (ty, V.typeOf x))
			      then ()
			      else error[
				 "type mismatch in Alloc: ", v2s' x, " = ",
				 if isMut then "alloc !(" else "alloc (", vl2s' ys, ")\n"
				]
			| _ => error[
			      "type of allocation is ", CFGTyUtil.toString ty, " in ",
			      v2s' x, " = ", "alloc(", vl2s' ys, ")\n"
			    ]
		      (* end case *);
		      addVar (env, x))
                  | CFG.E_GAlloc(x, ty, ys) => (
		      chkVars (env, ys, "GAlloc");
		      case ty
		       of Ty.T_Tuple(isMut, tys) =>
			    if (TyU.match (ty, V.typeOf x))
			      then ()
			      else error[
				 "type mismatch in GAlloc: ", v2s' x, " = ",
				 if isMut then "galloc !(" else "galloc (", vl2s' ys, ")\n"
				]
			| _ => error[
			      "type of allocation is ", CFGTyUtil.toString ty, " in ",
			      v2s' x, " = ", "galloc(", vl2s' ys, ")\n"
			    ]
		      (* end case *);
		      addVar (env, x))
                  | CFG.E_Promote (x, y) => let
                      fun err () = error[
                             "type mismatch in Promote: ", v2s' x, " = ",
                             "promote(", v2s' y, ")\n"]
                      in
			chkVar (env, y, "Promote");
			if TyU.equal (V.typeOf x, V.typeOf y)
			  then ()
			  else err ();
			addVar (env, x)
                      end
                  | CFG.E_Prim0 p => (chkVars (env, PrimUtil.varsOf p, PrimUtil.nameOf p); env)
                  | CFG.E_Prim (x, p) => (
                      chkVars (env, PrimUtil.varsOf p, PrimUtil.nameOf p);
                      addVar (env, x))
                  | CFG.E_CCall (xs, cf, args) => (
                      chkVar (env, cf, "CCall");
                      chkVars (env, args, "CCall args");
		      if (length xs > 1)
			then error["CCall with more than one result\n"]
			else ();
                      addVars (env, xs))
                  | CFG.E_HostVProc x => let
                      fun err () = error[
                             "type mismatch in HostVProc: ", v2s' x, " = ",
                             "host_vproc()\n"]
                      in
			if TyU.match (Ty.T_VProc, V.typeOf x)
			  then ()
			  else err ();
			addVar (env, x)
                      end
                  | CFG.E_VPLoad (x, i, y) => let
                      fun err () = error[
                             "type mismatch in VProcLoad: ", v2s' x, " = ",
                             "load(", v2s' y, "+", IntInf.toString i, ")\n"]
                      in
			chkVar (env, y, "VPLoad");
			if TyU.equal (Ty.T_VProc, V.typeOf y)
			  then ()
			  else err ();
			addVar (env, x)
                      end
                  | CFG.E_VPStore (i, y, z) => let
                      fun err () = error[
                             "type mismatch in VProcStore: ",
                             "store(", v2s' y, "+", IntInf.toString i, ", ", v2s' z, ")\n"]
                      in
			chkVar (env, y, "VPStore");
			chkVar (env, z, "VPStore");
			if TyU.equal (Ty.T_VProc, V.typeOf y)
			  then ()
			  else err ();
			env
                      end
                (* end case *))
          fun chkExit (env, exit) = (case exit
                 of CFG.StdApply{f, clos, args, ret, exh} => (
		      chkVar (env, f, "StdApply");
                      case V.typeOf f
                       of Ty.T_StdFun{clos = closTy, args = argTys, ret = retTy, exh = exhTy} => (
                            chkVar (env, clos, "StdApply clos");
                            chkVars (env, args, "StdApply args");
                            chkVar (env, ret, "StdApply ret");
                            chkVar (env, exh, "StdApply exh");
                            checkArgTypes (TyU.match, concat ["StdApply ", v2s f, " clos"], 
                                           [closTy], typesOf [clos]);
                            checkArgTypes (TyU.match, concat ["StdApply ", v2s f, " args"], 
                                           argTys, typesOf args);
                            checkArgTypes (TyU.match, concat ["StdApply ", v2s f, " ret"], 
                                           [retTy], typesOf [ret]);
                            checkArgTypes (TyU.match, concat ["StdApply ", v2s f, " exh"], 
                                           [exhTy], typesOf [exh]))
                        | ty => error[v2s f, ":", TyU.toString ty, " is not a stdfun\n"]
                      (* end case *))
                  | CFG.StdThrow{k, clos, args} => (
                      chkVar (env, k, "StdThrow");
                      case V.typeOf k
                       of Ty.T_StdCont{clos = closTy, args = argTys} => (
                            chkVar (env, clos, "StdThrow clos");
                            chkVars (env, args, "StdThrow args");
                            checkArgTypes (TyU.match, concat ["StdThrow ", v2s k, " clos"], 
                                           [closTy], typesOf [clos]);
                            checkArgTypes (TyU.match, concat ["StdThrow ", v2s k, " args"], 
                                           argTys, typesOf args))
                        | ty => error[v2s k, ":", TyU.toString ty, " is not a stdcont\n"]
                      (* end case *))
                  | CFG.Apply{f, clos, args} => (
                      chkVar (env, f, "Apply");
                      case V.typeOf f 
                       of Ty.T_KnownFunc {clos = closTy, args = argTys} => (
                            chkVar (env, clos, "Apply clos");
                            chkVars (env, args, "Apply args");
                            checkArgTypes (TyU.match, concat ["Apply ", v2s f, " clos"], 
                                           [closTy], typesOf [clos]);
                            checkArgTypes (TyU.match, concat ["Apply ", v2s f, " args"], 
                                           argTys, typesOf args))
                        | ty => error[v2s f, ":", TyU.toString ty, " is not a known\n"]
                      (* end case *))
                  | CFG.Goto jmp => chkJump (env, jmp, "Goto")
                  | CFG.If(x, j1, j2) => (
                      chkVar (env, x, "If");
                      if TyU.equal (V.typeOf x, Ty.boolTy)
                         then ()
                         else (
(* FIXME: this probably should be an error, but it doesn't get checked upstream yet *)
                           warning ["type mismatch in If(", V.toString x, ")\n"];
                           cerror ["  expected  ", TyU.toString Ty.boolTy, "\n"];
                           cerror ["  but found ", TyU.toString (V.typeOf x), "\n"]);
                      chkJump (env, j1, "If/true");
                      chkJump (env, j2, "If/false"))
                  | CFG.Switch(x, cases, dflt) => (
                      chkVar (env, x, "Switch");
                      case V.typeOf x
                       of Ty.T_Enum wt => let
                            fun chkCase (tag, jmp) = (
			           if (tag <= wt)
                                      then ()
                                      else (
                                        error ["case out of range for Switch(", V.toString x, ")\n"];
                                        cerror ["  expected  ", TyU.toString (V.typeOf x), "\n"];
                                        cerror ["  but found ", Word.toString tag, "\n"]);
                                   chkJump (env, jmp, "Switch/case"))
                            in
                              List.app chkCase cases;
                              Option.app (fn j => chkJump (env, j, "Switch/dflt")) dflt
                            end
                        | Ty.T_Raw rt => let
                            fun chkCase (tag, jmp) = 
                                   chkJump(env, jmp, "Switch/case")
                            fun chk () = (
                                   List.app chkCase cases; 
                                   Option.app (fn j => chkJump (env, j, "Switch/dflt")) dflt)
                            fun bad () = (
                                   error ["type mismatch in Switch argument\n"];
                                   cerror ["  but found ", TyU.toString (V.typeOf x), "\n"])
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
                            cerror ["  but found ", TyU.toString (V.typeOf x), "\n"])
		      (* end case *))
		  | CFG.HeapCheck{hck, szb, nogc} => (
                      chkJump (env, nogc, "HeapCheck"))
                  | CFG.AllocCCall{lhs, f, args, ret = (l,rargs)} => (
                       chkVar (env, f, "AllocCCall");
                       chkVars (env, args, "AllocCCall");
                       case V.typeOf f
                        of Ty.T_CFun (CFunctions.CProto (retTy, argTys, _)) => ()
                         | ty => error[v2s f, ":", TyU.toString ty, " is not a cfun\n"]
                       (* end case *);
                       chkJump (addVars (env, lhs), (l,lhs@rargs), "AllocCCall"))
                (* end case *))
		and chkJump (env, (lab, args), cxt) = (
		      chkLbl (lEnv, lab, cxt);
		      chkVars (env, args, cxt);
                      case L.typeOf lab 
                       of Ty.T_Block {args = argTys} => (
                           checkArgTypes (TyU.equal, cxt, argTys, typesOf args))
                        | ty => error[l2s lab, ":", TyU.toString ty, " is not a block\n"])
          fun chkFunc (CFG.FUNC {lab, entry, body, exit}) = let
                val env = chkEntry (lab, entry)
		val env = List.foldl (fn (exp,env) => chkExp (env, exp)) env body
                val _ = chkExit (env, exit)
                in
                   ()
                end
          in
            List.app chkFunc code;
if !anyErrors
  then (
    print "******************** broken CFG ********************\n";
    PrintCFG.print module;
    print "********************\n";
    raise Fail "broken CFG")
  else ();
	  (* return the error status *)
	    !anyErrors
	  end (* check *)

    val check =
       BasicControl.mkTracePass
       {passName = "cfg-check",
        pass = check,
        verbose = 2}
  end
