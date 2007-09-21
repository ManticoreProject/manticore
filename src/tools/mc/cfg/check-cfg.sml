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

    val check : CFG.module -> unit

  end = struct

    structure V = CFG.Var
    structure L = CFG.Label
    structure VSet = CFG.Var.Set
    structure LSet = CFG.Label.Set
    structure LMap = CFG.Label.Map
    structure Ty = CFGTy
    structure Lit = Literal

    fun error msg = TextIO.output(TextIO.stdErr, concat("CFG Error: " :: msg @ ["\n"]))

    fun warning msg = TextIO.output(TextIO.stdErr, concat("CFG Warning: " :: msg @ ["\n"]))

    fun vl2s [] = "[]"
      | vl2s [x] = concat[V.toString x, ":", Ty.toString(V.typeOf x)]
      | vl2s (x::xs) = let
	  fun f (x, l) = "," :: V.toString x ::  ":" ::  Ty.toString(V.typeOf x) :: l
	  in
	    String.concat("[" :: V.toString x ::  ":" ::  Ty.toString(V.typeOf x)
	      :: List.foldr f ["]"] xs)
	  end

    fun tyl2s tys = Ty.toString(Ty.T_Tuple(false, tys))

    fun bindVar (env, x) = VSet.add(env, x)

    fun bindVars (env, xs) = VSet.addList(env, xs)

    fun check (m as CFG.MODULE{name, externs, code}) = let
	  val anyErrors = ref false
	(* construct a set of the bound labels in the module *)
	  val lSet = List.foldl
		(fn (f as CFG.FUNC{lab, ...}, lset) => LSet.add(lset, lab))
		  LSet.empty code
          val lMap = List.foldl
                (fn (f as CFG.FUNC{lab, entry, ...}, lmap) => LMap.insert(lmap,lab,entry))
                  LMap.empty code
	  fun chk (CFG.FUNC{lab, entry, body, exit}) = let
                fun err msg = (
		      anyErrors := true;
		      error (msg @ [" in ", Atom.toString name, ".", L.toString lab]))
                fun warn msg = warning (msg @ [" in ", Atom.toString name, ".", L.toString lab])
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
	      (* check the entry against the declared type of the label;  The declared type is allowed to
	       * be more specific.
	       *)
		fun chkEntry (CFG.StdFunc{clos, args, ret, exh}) = (
                      (case L.typeOf lab of
                          Ty.T_StdFun{clos = closTy, args = argTys, ret = retTy, exh = exhTy} => 
                             ((* FIXME: closure types not set by assignLabels *)
                              (*
                              if Ty.equal (V.typeOf clos, closTy)
                                 then ()
                              else err["variable ", V.toString clos, ":", Ty.toString (V.typeOf clos),
                                       " does not match ",
                                       "closure type ", Ty.toString closTy];
                              *)
                              if ListPair.allEq Ty.match (argTys, List.map V.typeOf args)
                        	then ()
                                else err[
				    "parameters ", vl2s args, " do not match ",
                                       "argument types ", tyl2s argTys
				  ];
                              if Ty.match (retTy, V.typeOf ret)
                                 then ()
                              else err["parameter ", V.toString ret, ":", Ty.toString (V.typeOf ret),
                                       " does not match ",
                                       "return type ", Ty.toString retTy];
                              if Ty.equal (exhTy, V.typeOf exh)
                                 then ()
                              else err["parameter ", V.toString exh, ":", Ty.toString (V.typeOf exh),
                                       " does not match ",
                                       "exh type ", Ty.toString exhTy])
                        | _ => err["label ", L.toString lab, ":", Ty.toString (L.typeOf lab),
                                   " is not stdfun"]);
		      bindVars(VSet.empty, clos::ret::exh::args))
		  | chkEntry (CFG.StdCont{clos, args}) = (
                      case L.typeOf lab
		       of Ty.T_StdCont{clos = closTy, args = argTys} => (
                           (* FIXME: closure types not set by assignLabels *)
                            (*
                            if Ty.equal (V.typeOf clos, closTy)
                               then ()
                            else err["variable ", V.toString clos, ":", Ty.toString (V.typeOf clos),
                                     " does not match ",
                                     "closure type ", Ty.toString closTy];
                            *)
                            if ListPair.allEq Ty.match (argTys, List.map V.typeOf args)
                              then ()
                              else err[
				  "parameters ", vl2s args, " do not match ",
                                     "argument types ", tyl2s argTys
				])
			| _ => err["label ", L.toString lab, ":", Ty.toString (L.typeOf lab),
                                   " is not stdcont"]
		      (* end case *);
		      bindVars(VSet.empty, clos::args))
		  | chkEntry (CFG.KnownFunc args) = (
                      case L.typeOf lab of
                          Ty.T_KnownFunc argTys => 
                             ((ListPair.appEq (fn (arg, argTy) =>
                                               if Ty.match (argTy, V.typeOf arg)
                                                  orelse Ty.match (argTy, Ty.T_Any)
                                                  then ()
                                               else err["parameter ", V.toString arg, ":", Ty.toString (V.typeOf arg),
                                                        " does not match ",
                                                        "argument type ", Ty.toString argTy])
                                              (args, argTys))
                              handle ListPair.UnequalLengths =>
                                 err["parameter (", String.concatWith "," (List.map V.toString args),
                                     ") do not match (", 
                                     String.concatWith "," (List.map Ty.toString argTys),
                                     ")"])
                        | _ => err["label ", L.toString lab, ":", Ty.toString (L.typeOf lab),
                                   " is not code"]
		      (* end case *);
		      bindVars(VSet.empty, args))
		  | chkEntry (CFG.Block args) = (
                      (case L.typeOf lab of
                          Ty.T_Block argTys => 
                             ((ListPair.appEq (fn (arg, argTy) =>
                                               if Ty.match (argTy, V.typeOf arg)
                                                  then ()
                                               else err["parameter ", V.toString arg, ":", Ty.toString (V.typeOf arg),
                                                        " does not match ",
                                                        "argument type ", Ty.toString argTy])
                                              (args, argTys))
                              handle ListPair.UnequalLengths =>
                                 err["parameters (", String.concatWith "," (List.map V.toString args),
                                     ") do not match (", 
                                     String.concatWith "," (List.map Ty.toString argTys),
                                     ")"])
                        | _ => err["label ", L.toString lab, ":", Ty.toString (L.typeOf lab),
                                   " is not code"]);
		      bindVars(VSet.empty, args))
		fun chkExp (e, env) = (case e
		       of CFG.E_Var(lhs, rhs) => let
			    fun chk (x, y) = if Ty.equal (V.typeOf x, V.typeOf y) then ()
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
			| CFG.E_Const(x, lit) => (
			    case (V.typeOf x, lit)
			     of (Ty.T_Enum wt, Lit.Enum w) => if Word.<= (w, wt) 
                        	  then ()
                                  else err[
				      "variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                      " is not ", Ty.toString (Ty.T_Enum wt)
				    ]
			      | _ => ()
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
                            if Ty.equal (V.typeOf x, ty)
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
			      | CFG.LK_Local _ => if Ty.equal (V.typeOf x, L.typeOf lab)
				  then ()
                                  else err[
				      "variable ", V.toString x, ":", Ty.toString (V.typeOf x),
				      " does not match label ",
				      L.toString lab, ":", Ty.toString (L.typeOf lab)
				    ]
			    (* end case *);
			    bindVar (env, x))
			| CFG.E_Select(x, i, y) => let
			    val ty = Ty.selectTy(i, V.typeOf y)
				  handle Fail msg => (
				    err["E_Select(", V.toString x, ", ", Int.toString i, ", ",
					V.toString y, ":", Ty.toString(V.typeOf y), ")"
				      ];
				    Ty.T_Any)
			    in
			      chkVar (env, y);
(* FIXME: Selecting from a known closure into an T_Any environment pointer fails *)
(*
                              if Ty.equal (V.typeOf x, ty)
                                 then ()
                              else err["variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                       " does not match ",
                                       "type ", Ty.toString ty];
*)
                              bindVar (env, x)
			    end
			| CFG.E_Update(i, y, z) => let
			    val ty = Ty.selectTy(i, V.typeOf y)
				  handle Fail msg => (
				    err["E_Update(", Int.toString i, ", ",
					V.toString y, ":", Ty.toString(V.typeOf y), ", ",
					V.toString z, ")"
				      ];
				    Ty.T_Any)
			    in
			      chkVar (env, y);
(* FIXME: check that the tuple is mutable and that z has the right type *)
			      env
			    end
			| CFG.E_AddrOf(x, i, y) => let
			    val ty = Ty.selectTy(i, V.typeOf y)
				  handle Fail msg => (
				    err["E_AddrOf(", V.toString x, ", ", Int.toString i, ", ",
					V.toString y, ":", Ty.toString(V.typeOf y), ")"
				      ];
				    Ty.T_Any)
			    in
			      chkVar (env, y);
			      case V.typeOf x
			       of Ty.T_Addr ty' => if Ty.equal(ty, ty')
				    then ()
				    else err[
					"type mismatch in E_AddrOf: lhs = ", Ty.toString ty',
					", rhs = ", Ty.toString ty, "\n"
				      ]
				| ty' => err[
					"type error in E_AddrOf: lhs = ", Ty.toString ty', "\n"
				      ]
			      (* end case *);
                              bindVar (env, x)
			    end
			| CFG.E_Alloc(x, ys) => (
			    chkVars (env, ys);
                            case V.typeOf x
                             of Ty.T_Tuple tys => ()
                              | Ty.T_OpenTuple tys => ()
			      | Ty.T_Any => ()
                              | _ => err["variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                         " does not match allocation"]
			    (* end case *);
			    bindVar (env, x))
			| CFG.E_Wrap(x, y) => (
			    chkVar (env, y);
			    case V.typeOf y
			     of Ty.T_Raw rty => (case V.typeOf x
				   of Ty.T_Wrap rtx => if (rtx = rty)
                                         then ()
                                         else err["variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                                  " is not ", Ty.toString (Ty.T_Wrap rty)]
				    | _ => err["variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                           " is not wrap"]
				  (* end case *))
			      | _ => err["variable ", V.toString y, ":", Ty.toString (V.typeOf y),
                                                     " is not raw"]
			    (* end case *);
			    bindVar (env, x))
			| CFG.E_Unwrap(x, y) => (
			    chkVar (env, y);
			    case V.typeOf y
			     of Ty.T_Wrap rty => (case V.typeOf x
				   of Ty.T_Raw rtx => if (rtx = rty)
                                        then ()
                                        else err["variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                                 " is not ", Ty.toString (Ty.T_Raw rty)]
				    | _ => err["variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                           " is not raw"]
				  (* end case *))
			      | _ =>  err["variable ", V.toString y, ":", Ty.toString (V.typeOf y),
                                                    " is not wrap"]
			    (* end case *);
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
		       of CFG.StdApply{f, clos, args, ret, exh} => (
			    chkVars (env, f :: clos :: ret :: exh :: args);
                            case V.typeOf f
			     of Ty.T_StdFun{clos = closTy, args = argTys, ret = retTy, exh = exhTy} => (
                                  if Ty.match (V.typeOf clos, closTy)
                                     then ()
                                  else warn["variable ", V.toString clos, ":", Ty.toString (V.typeOf clos),
                                           " does not match ",
                                           "closure type ", Ty.toString closTy];
                        	  if ListPair.allEq Ty.match (List.map V.typeOf args, argTys)
                                    then ()
                                    else err[
					"arguments ", vl2s args, " do not match ",
                                	   "argument types ", tyl2s argTys
				      ];
                                  if Ty.match (V.typeOf ret, retTy)
                                     then ()
                                  else err["variable ", V.toString ret, ":", Ty.toString (V.typeOf ret),
                                           " does not match ",
                                           "return type ", Ty.toString retTy];
                                  if Ty.match (V.typeOf exh, exhTy)
                                     then ()
                                  else err["variable ", V.toString exh, ":", Ty.toString (V.typeOf exh),
                                           " does not match ",
                                           "exh type ", Ty.toString exhTy])
                              | _ => err["variable ", V.toString f, ":", Ty.toString (V.typeOf f),
                                         " is not stdfun"]
			    (* end case *))
			| CFG.StdThrow{k, clos, args} => (
			    chkVars (env, k :: clos :: args);
                            case V.typeOf k
			     of Ty.T_StdCont{clos = closTy, args = argTys} => (
                                  if Ty.match (V.typeOf clos, closTy)
                                     then ()
                                  else warn["variable ", V.toString clos, ":", Ty.toString (V.typeOf clos),
                                           " does not match ",
                                           "closure type ", Ty.toString closTy];
                        	if ListPair.allEq Ty.match (List.map V.typeOf args, argTys)
                        	  then ()
                        	  else err[
				      "arguments ", vl2s args, " do not match ",
                                	 "argument types ", tyl2s argTys
				    ])
                              | _ => err["variable ", V.toString k, ":", Ty.toString (V.typeOf k),
                                         " is not stdcont"]
			    (* end case *))
			| CFG.Apply{f, args} => (
			    chkVars (env, f::args);
                            (case V.typeOf f of
                                Ty.T_KnownFunc argTys => 
                                   ((ListPair.appEq (fn (arg, argTy) =>
                                                     if Ty.match (V.typeOf arg, argTy)
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
                            if Ty.equal (V.typeOf x, Ty.boolTy)
                              then ()
                              else err["variable ", V.toString x, ":", Ty.toString (V.typeOf x), 
                                     " is not bool"];
			    chkJump (env, j1);
			    chkJump (env, j2))
			| CFG.Switch(x, cases, dflt) => (
			    chkVar (env, x);
			    case V.typeOf x
			     of Ty.T_Enum wt => let
				  fun chkCase (tag, jmp) = (
					if (tag <= wt)
                                	  then ()
                                	  else err[
					      "case ", Word.toString tag, " is out of range for ",
                                	      V.toString x, ":", Ty.toString (V.typeOf x)
					    ];
					chkJump(env, jmp))
				  in
				    List.app chkCase cases;
				    Option.app (fn j => chkJump(env, j)) dflt
				  end
			      | _ => err["variable ", V.toString x, ":", Ty.toString (V.typeOf x), 
                                               " is not valid argument for switch"]
			    (* end case *))
			| CFG.HeapCheck{szb, nogc = (lab, args)} => (
                            chkLabel lab;
                            chkVars (env, args);
                            case L.typeOf lab
			     of Ty.T_StdFun _ => err["noGC target is standard fun"]
                              | Ty.T_StdCont _ => err["noGC target is standard cont"]
                              | Ty.T_Block argTys => 
                                  ((ListPair.appEq (fn (arg, argTy) =>
                                                    if Ty.equal (V.typeOf arg, argTy)
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
                                         " is not heap-check target"]
			    (* end case *))
		      (* end case *))
		and chkJump (env, (lab, args)) = (
		      chkLabel lab;
		      chkVars (env, args);
                      case LMap.find (lMap, lab) of
                         SOME (CFG.Block argTys) => 
                           ((ListPair.appEq (fn (arg, argTy) =>
                                             if Ty.equal (V.typeOf arg, argTy)
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
	    List.app chk code;
	    if !anyErrors then raise Fail "broken CFG" else ()
	  end (* check *)

    val check =
       BasicControl.mkTracePass
       {passName = "cfg-check",
        pass = check,
        verbose = 2}
  end
