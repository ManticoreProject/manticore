(* check-cfg.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Check a CFG module for well-formedness
 *)

structure CheckCFG : sig

    val check : CFG.module -> unit

  end = struct

    structure V = CFG.Var
    structure L = CFG.Label
    structure VSet = CFG.Var.Set
    structure LSet = CFG.Label.Set
    structure LMap = CFG.Label.Map
    structure Ty = CFGTy

    exception CheckCFG
    fun error msg = (
	  TextIO.output(TextIO.stdErr, concat("Error: " :: msg @ ["\n"]));
          raise CheckCFG)

    fun bindVar (env, x) = VSet.add(env, x)

    fun bindVars (env, xs) = VSet.addList(env, xs)

    fun check (m as CFG.MODULE{name, externs, code}) = let
	(* construct a set of the bound labels in the module *)
	  val lSet = List.foldl
		(fn (f as CFG.FUNC{lab, ...}, lset) => LSet.add(lset, lab))
		  LSet.empty code
          val lMap = List.foldl
                (fn (f as CFG.FUNC{lab, entry, ...}, lmap) => LMap.insert(lmap,lab,entry))
                  LMap.empty code
	  fun chk (CFG.FUNC{lab, entry, body, exit}) = let
                fun err msg = error (msg @ [" in ", Atom.toString name, ".", L.toString lab])
		fun chkVar (env, x) = if VSet.member(env, x)
		      then ()
		      else err[
			  "unbound variable ", V.toString x]
		fun chkVars (env, xs) = List.app (fn x => chkVar(env, x)) xs
		fun chkLabel l = (case (L.kindOf l, LSet.member(lSet, l))
		       of (CFG.LK_None, _) => err["label ", L.toString l, "has no kind"]
			| (CFG.LK_Extern _, false) => ()
			| (CFG.LK_Extern _, true) => err["extern local label ", L.toString l]
			| (CFG.LK_Local _, true) => ()
			| (CFG.LK_Local _, false) => err["reference to unbound label ", L.toString l]
		      (* end case *))
		fun chkEntry (CFG.StdFunc{clos, arg, ret, exh}) = (
                      (case L.typeOf lab of
                          Ty.T_StdFun {clos = closTy, arg = argTy, ret = retTy, exh = exhTy} => 
                             ((* FIXME: closure types not set by assignLabels *)
                              (*
                              if Ty.equals (V.typeOf clos, closTy)
                                 then ()
                              else err["variable ", V.toString clos, ":", Ty.toString (V.typeOf clos),
                                       " does not match ",
                                       "closure type ", Ty.toString closTy];
                              *)
                              if Ty.equals (V.typeOf arg, argTy)
                                 then ()
                              else err["variable ", V.toString arg, ":", Ty.toString (V.typeOf arg),
                                       " does not match ",
                                       "argument type ", Ty.toString argTy];
                              if Ty.equals (V.typeOf ret, retTy)
                                 then ()
                              else err["variable ", V.toString ret, ":", Ty.toString (V.typeOf ret),
                                       " does not match ",
                                       "return type ", Ty.toString retTy];
                              if Ty.equals (V.typeOf exh, exhTy)
                                 then ()
                              else err["variable ", V.toString exh, ":", Ty.toString (V.typeOf exh),
                                       " does not match ",
                                       "exh type ", Ty.toString exhTy])
                        | _ => err["label ", L.toString lab, ":", Ty.toString (L.typeOf lab),
                                   " is not stdfun"]);
		      bindVars(VSet.empty, [clos, arg, ret, exh]))
		  | chkEntry (CFG.StdCont{clos, arg}) = (
                      (case L.typeOf lab of
                          Ty.T_StdCont {clos = closTy, arg = argTy} => 
                             ((* FIXME: closure types not set by assignLabels *)
                              (*
                              if Ty.equals (V.typeOf clos, closTy)
                                 then ()
                              else err["variable ", V.toString clos, ":", Ty.toString (V.typeOf clos),
                                       " does not match ",
                                       "closure type ", Ty.toString closTy];
                              *)
                              if Ty.equals (V.typeOf arg, argTy)
                                 then ()
                              else err["variable ", V.toString arg, ":", Ty.toString (V.typeOf arg),
                                       " does not match ",
                                       "argument type ", Ty.toString argTy])
                        | _ => err["label ", L.toString lab, ":", Ty.toString (L.typeOf lab),
                                   " is not stdcont"]);
		      bindVars(VSet.empty, [clos, arg]))
		  | chkEntry (CFG.KnownFunc args) = (
                      (case L.typeOf lab of
                          Ty.T_Code argTys => 
                             ((ListPair.appEq (fn (arg, argTy) =>
                                               if Ty.equals (V.typeOf arg, argTy)
                                                  then ()
                                               else err["variable ", V.toString arg, ":", Ty.toString (V.typeOf arg),
                                                        " does not match ",
                                                        "argument type ", Ty.toString argTy])
                                              (args, argTys))
                              handle ListPair.UnequalLengths =>
                                 err["variables (", String.concatWith "," (List.map V.toString args),
                                     ") do not match ", 
                                     "variables (", String.concatWith "," (List.map Ty.toString argTys),
                                     ")"])
                        | _ => err["label ", L.toString lab, ":", Ty.toString (L.typeOf lab),
                                   " is not code"]);
		      bindVars(VSet.empty, args))
		  | chkEntry (CFG.Block args) = (
                      (case L.typeOf lab of
                          Ty.T_Code argTys => 
                             ((ListPair.appEq (fn (arg, argTy) =>
                                               if Ty.equals (V.typeOf arg, argTy)
                                                  then ()
                                               else err["variable ", V.toString arg, ":", Ty.toString (V.typeOf arg),
                                                        " does not match ",
                                                        "argument type ", Ty.toString argTy])
                                              (args, argTys))
                              handle ListPair.UnequalLengths =>
                                 err["variables (", String.concatWith "," (List.map V.toString args),
                                     ") do not match ", 
                                     "variables (", String.concatWith "," (List.map Ty.toString argTys),
                                     ")"])
                        | _ => err["label ", L.toString lab, ":", Ty.toString (L.typeOf lab),
                                   " is not code"]);
		      bindVars(VSet.empty, args))
		fun chkExp (e, env) = (case e
		       of CFG.E_Var(lhs, rhs) => let
			    fun chk (x, y) = if Ty.equals (V.typeOf x, V.typeOf y) then ()
                                    else err[
					"variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                        " does not match ",
                                        "variable ", V.toString y, ":", Ty.toString (V.typeOf y)
				      ]
			    in
			      chkVars (env, rhs);
                              (ListPair.appEq chk (lhs, rhs))
                                 handle ListPair.UnequalLengths => 
                                          err["variables (", String.concatWith "," (List.map V.toString lhs),
                                              ") do not match ", 
                                              "variables (", String.concatWith "," (List.map V.toString lhs),
                                              ")"];
			      bindVars (env, lhs)
                            end
			| CFG.E_Enum(x, w) => (
                            case V.typeOf x of 
                               CFGTy.T_Enum wt => if Word.<= (w, wt) 
                                                     then () 
                                                  else err["variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                                           " is not ", Ty.toString (CFGTy.T_Enum wt)]
                             | _ => error ["variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                           " is not enum"]
			    (* end case *);
                            bindVar (env, x))
			| CFG.E_Cast(x, ty, y) => (
			    chkVar (env, y);
                            if Ty.isValidCast (V.typeOf y, ty)
			      then ()
			      else err[
				  "variable ", V.toString y, ":", Ty.toString (V.typeOf y),
                                  " cannot be cast to ", "type ", Ty.toString ty
				];
                            if Ty.equals (V.typeOf x, ty)
			      then ()
			      else err[
				  "variable ", V.toString x, ":", Ty.toString (V.typeOf x),
				  " does not match type ", Ty.toString ty
				];
			    bindVar (env, x))
			| CFG.E_Label(x, lab) => (
			    chkLabel lab;
                            case L.kindOf lab
			     of CFG.LK_None => err["label ", L.toString lab, " has kind None"]
			      | CFG.LK_Extern _ => ()
			      | CFG.LK_Local _ => if Ty.equals (V.typeOf x, L.typeOf lab)
				  then ()
                                  else err[
				      "variable ", V.toString x, ":", Ty.toString (V.typeOf x),
				      " does not match label",
				      L.toString lab, ":", Ty.toString (L.typeOf lab)
				    ]
			    (* end case *);
			    bindVar (env, x))
			| CFG.E_Literal(x, _) => bindVar (env, x)
			| CFG.E_Select(x, i, y) => let
			    val ty = CFGTy.selectTy(i, V.typeOf y)
				  handle Fail msg => (
				    error["E_Select(", V.toString x, ", ", Int.toString i, ", ",
					V.toString y, ":", CFGTy.toString(V.typeOf y), ")"
				      ];
				    CFGTy.T_Any)
			    in
			      chkVar (env, y);
(* FIXME: Selecting from a known closure into an T_Any environment pointer fails *)
(*
                              if Ty.equals (V.typeOf x, ty)
                                 then ()
                              else err["variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                       " does not match ",
                                       "type ", Ty.toString ty];
*)
                              bindVar (env, x)
			    end
			| CFG.E_Alloc(x, ys) => (
			    chkVars (env, ys);
                            case V.typeOf x
                             of CFGTy.T_Tuple tys => ()
                              | CFGTy.T_OpenTuple tys => ()
                              | _ => err["variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                         " does not match allocation"]
			    (* end case *);
			    bindVar (env, x))
			| CFG.E_Wrap(x, y) => (
			    chkVar (env, y);
                            let
                               val rty = case V.typeOf y of
                                            CFGTy.T_Raw rty => rty
                                          | _ => err["variable ", V.toString y, ":", Ty.toString (V.typeOf y),
                                                     " is not raw"]
                            in
                               case V.typeOf x of
                                  CFGTy.T_Wrap rtx => if (rtx = rty)
                                         then ()
                                         else err["variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                                  " is not ", Ty.toString (CFGTy.T_Wrap rty)]
                                | _ => err["variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                           " is not wrap"]
                            end;
			    bindVar (env, x))
			| CFG.E_Unwrap(x, y) => (
			    chkVar (env, y);
                            let
                               val rty = case V.typeOf y of
                                            CFGTy.T_Wrap rty => rty
                                          | _=> err["variable ", V.toString y, ":", Ty.toString (V.typeOf y),
                                                    " is not wrap"]
                            in
                               case V.typeOf x of
                                  CFGTy.T_Raw rtx => if (rtx = rty)
                                                        then ()
                                                     else err["variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                                              " is not ", Ty.toString (CFGTy.T_Raw rty)]
                                | _ => err["variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                           " is not raw"]
                            end;
			    bindVar (env, x))
			| CFG.E_Prim(x, p) => (
			    chkVars (env, PrimUtil.varsOf p);
			    bindVar (env, x))
			| CFG.E_CCall(lhs, f, args) => (
			    chkVars (env, f::args);
			    bindVars (env, lhs))
			| CFG.E_HostVProc vp => bindVar (env, vp)
			| CFG.E_VPLoad(x, _, vp) => (
			    chkVar (env, vp);
			    bindVar (env, x))
			| CFG.E_VPStore(_, vp, x) => (
			    chkVar (env, vp);
			    chkVar (env, x);
			    env)
		      (* end case *))
		fun chkExit (env, xfer) = (case xfer
		       of CFG.StdApply{f, clos, arg, ret, exh} => (
			    chkVars (env, [f, clos, arg, ret, exh]);
                            (case V.typeOf f of
                                Ty.T_StdFun {clos = closTy, arg = argTy, ret = retTy, exh = exhTy} =>
                                   (if Ty.isValidCast (V.typeOf clos, closTy)
                                       then ()
                                    else err["variable ", V.toString clos, ":", Ty.toString (V.typeOf clos),
                                             " does not match ",
                                             "closure type ", Ty.toString closTy];
                                    if Ty.isValidCast (V.typeOf arg, argTy)
                                       then ()
                                    else err["variable ", V.toString arg, ":", Ty.toString (V.typeOf arg),
                                             " does not match ",
                                             "argument type ", Ty.toString argTy];
                                    if Ty.isValidCast (V.typeOf ret, retTy)
                                       then ()
                                    else err["variable ", V.toString ret, ":", Ty.toString (V.typeOf ret),
                                             " does not match ",
                                             "return type ", Ty.toString retTy];
                                    if Ty.isValidCast (V.typeOf exh, exhTy)
                                       then ()
                                    else err["variable ", V.toString exh, ":", Ty.toString (V.typeOf exh),
                                             " does not match ",
                                             "exh type ", Ty.toString exhTy])
                              | _ => err["variable ", V.toString f, ":", Ty.toString (V.typeOf f),
                                         " is not stdfun"]))
			| CFG.StdThrow{k, clos, arg} => (
			    chkVars (env, [k, clos, arg]);
                            (case V.typeOf k of
                                Ty.T_StdCont {clos = closTy, arg = argTy} =>
                                   (if Ty.isValidCast (V.typeOf clos, closTy)
                                       then ()
                                    else err["variable ", V.toString clos, ":", Ty.toString (V.typeOf clos),
                                             " does not match ",
                                             "closure type ", Ty.toString closTy];
                                    if Ty.isValidCast (V.typeOf arg, argTy)
                                       then ()
                                    else err["variable ", V.toString arg, ":", Ty.toString (V.typeOf arg),
                                             " does not match ",
                                             "argument type ", Ty.toString argTy])
                              | _ => err["variable ", V.toString k, ":", Ty.toString (V.typeOf k),
                                         " is not stdcont"]))
			| CFG.Apply{f, args} => (
			    chkVars (env, f::args);
                            (case V.typeOf f of
                                Ty.T_Code argTys => 
                                   ((ListPair.appEq (fn (arg, argTy) =>
                                                     if Ty.equals (V.typeOf arg, argTy)
                                                        then ()
                                                     else err["variable ", V.toString arg, ":", Ty.toString (V.typeOf arg),
                                                              " does not match ",
                                                              "argument type ", Ty.toString argTy])
                                                     (args, argTys))
                                    handle ListPair.UnequalLengths =>
                                             err["variables (", String.concatWith "," (List.map V.toString args),
                                                 ") do not match ", 
                                                 "variables (", String.concatWith "," (List.map Ty.toString argTys),
                                                 ")"])
                              | _ => err["variable ", V.toString f, ":", Ty.toString (V.typeOf f),
                                         " is not code"]))
			| CFG.Goto jmp => chkJump (env, jmp)
			| CFG.If(x, j1, j2) => (
			    chkVar (env, x);
                            if Ty.equals (V.typeOf x, CFGTy.boolTy)
                               then ()
                            else err["variable ", V.toString x, ":", Ty.toString (V.typeOf x), 
                                     " is not bool"]
			    chkJump (env, j1);
			    chkJump (env, j2))
			| CFG.Switch(x, cases, dflt) => (
			    chkVar (env, x);
                            let
                               val chkC =
                                  (case V.typeOf x of
                                      CFGTy.T_Enum wt => 
                                         (fn w => if (wt <= w)
                                                     then ()
                                                  else err[
						      "case ", Word.toString w, " is out of range for ",
                                                      "variable ", V.toString x, ":", Ty.toString (V.typeOf x)
						    ])
                                    | CFGTy.T_Raw rty => 
                                         (case rty of
                                             RawTypes.T_Int => (fn _ => ())
                                           | _ => err["variable ", V.toString x, ":", Ty.toString (V.typeOf x), 
                                                      " is not switch"])
                                    | _ => err["variable ", V.toString x, ":", Ty.toString (V.typeOf x), 
                                               " is not valid argument for switch"])
                            in
                              List.app (fn (i, j) => (chkC i; chkJump(env, j))) cases
                            end;
			    Option.app (fn j => chkJump(env, j)) dflt)
			| CFG.HeapCheck{szb, nogc = (lab, args)} => (
                            chkLabel lab;
                            chkVars (env, args);
                            (case L.typeOf lab of
                                Ty.T_StdFun {clos = closTy, arg = argTy, ret = retTy, exh = exhTy} =>
                                   (let val [clos, arg, ret, exh] = args in
                                    if Ty.isValidCast (V.typeOf clos, closTy)
                                       then ()
                                    else err["variable ", V.toString clos, ":", Ty.toString (V.typeOf clos),
                                             " does not match ",
                                             "closure type ", Ty.toString closTy];
                                    if Ty.isValidCast (V.typeOf arg, argTy)
                                       then ()
                                    else err["variable ", V.toString arg, ":", Ty.toString (V.typeOf arg),
                                             " does not match ",
                                             "argument type ", Ty.toString argTy];
                                    if Ty.isValidCast (V.typeOf ret, retTy)
                                       then ()
                                    else err["variable ", V.toString ret, ":", Ty.toString (V.typeOf ret),
                                             " does not match ",
                                             "return type ", Ty.toString retTy];
                                    if Ty.isValidCast (V.typeOf exh, exhTy)
                                       then ()
                                    else err["variable ", V.toString exh, ":", Ty.toString (V.typeOf exh),
                                             " does not match ",
                                             "exh type ", Ty.toString exhTy]
                                    end)
                              | Ty.T_StdCont {clos = closTy, arg = argTy} =>
                                  (let val [clos, arg] = args in
                                    (* FIXME: closure types not set by assignLabels *)
                                    (*
                                    if Ty.isValidCast (V.typeOf clos, closTy)
                                       then ()
                                    else err["variable ", V.toString clos, ":", Ty.toString (V.typeOf clos),
                                             " does not match ",
                                             "closure type ", Ty.toString closTy];
                                    *)
                                    if Ty.isValidCast (V.typeOf arg, argTy)
                                       then ()
                                    else err["variable ", V.toString arg, ":", Ty.toString (V.typeOf arg),
                                             " does not match ",
                                             "argument type ", Ty.toString argTy]
                                    end)
                              | Ty.T_Code argTys => 
                                  ((ListPair.appEq (fn (arg, argTy) =>
                                                    if Ty.equals (V.typeOf arg, argTy)
                                                       then ()
                                                    else err["variable ", V.toString arg, ":", Ty.toString (V.typeOf arg),
                                                             " does not match ",
                                                             "argument type ", Ty.toString argTy])
                                                   (args, argTys))
                                   handle ListPair.UnequalLengths =>
                                      err["variables (", String.concatWith "," (List.map V.toString args),
                                          ") do not match ", 
                                          "variables (", String.concatWith "," (List.map Ty.toString argTys),
                                          ")"])
                              | _ => err["label ", L.toString lab, ":", Ty.toString (L.typeOf lab),
                                         " is not heap-check target"]))
		      (* end case *))
		and chkJump (env, (lab, args)) = (
		      chkLabel lab;
		      chkVars (env, args);
                      case LMap.find (lMap, lab) of
                         SOME (CFG.Block argTys) => 
                           ((ListPair.appEq (fn (arg, argTy) =>
                                             if Ty.equals (V.typeOf arg, argTy)
                                                then ()
                                             else err["variable ", V.toString arg, ":", Ty.toString (V.typeOf arg),
                                                      " does not match ",
                                                      "argument type ", Ty.toString argTy])
                                            (args, List.map V.typeOf argTys))
                            handle ListPair.UnequalLengths =>
                               err["variables (", String.concatWith "," (List.map V.toString args),
                                   ") do not match ", 
                                   "variables (", String.concatWith "," (List.map (Ty.toString o V.typeOf) argTys),
                                   ")"])
                        | _ => err["label ", L.toString lab, " is not block"])
		val env = chkEntry entry
		val env = List.foldl chkExp env body
		in
		  chkExit (env, exit)
		end (* chk *)
	  in
	    List.app chk code
	  end (* check *)

  end
