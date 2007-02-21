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
    fun error msg = (TextIO.output(TextIO.stdErr, concat("Error: " :: msg @ ["\n"]))
                     ; raise CheckCFG)

    fun bindVar (env, x) = VSet.add(env, x)

    fun bindVars (env, xs) = VSet.addList(env, xs)

    fun check (m as CFG.MODULE{name, code}) = let
	(* construct a set of the bound labels in the module *)
	  val lSet = List.foldl
		(fn (f as CFG.FUNC{lab, ...}, lset) => LSet.add(lset, lab))
		  LSet.empty code
          val lMap = List.foldl
                (fn (f as CFG.FUNC{lab, entry, ...}, lmap) => LMap.insert(lmap,lab,entry))
                  LMap.empty code
	  fun chk (CFG.FUNC{lab, entry, body, exit}) = let
                fun err msg = error (msg @ [" in ", L.toString lab, ".", Atom.toString name])
		fun chkVar (env, x) = if VSet.member(env, x)
		      then ()
		      else err[
			  "unbound variable ", V.toString x]
		fun chkVars (env, xs) = List.app (fn x => chkVar(env, x)) xs
		fun chkLabel l = (case (L.kindOf l, LSet.member(lSet, l))
		       of (CFG.LK_None, _) => err[
			      "label ", L.toString l, "has no kind"]
			| (CFG.LK_Extern _, false) => ()
			| (CFG.LK_Extern _, true) => err[
			      "exported local label ", L.toString l]
			| (CFG.LK_Local _, true) => ()
			| (CFG.LK_Local _, false) => err[
			      "reference to unbound label ", L.toString l]
		      (* end case *))
		fun chkEntry (CFG.StdFunc{clos, arg, ret, exh}) =
		      bindVars(VSet.empty, [clos, arg, ret, exh])
		  | chkEntry (CFG.StdCont{clos, arg}) =
		      bindVars(VSet.empty, [clos, arg])
		  | chkEntry (CFG.KnownFunc params) =
		      bindVars(VSet.empty, params)
		  | chkEntry (CFG.Block params) =
		      bindVars(VSet.empty, params)
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
				handle ListPair.UnequalLengths => err["E_Var binding of unequal lengths"];
			      bindVars (env, lhs)
                            end
			| CFG.E_Enum(x, w) => (
                            case V.typeOf x
                             of CFGTy.T_Enum wt => if Word.<= (w, wt) then () else error [""]
                             | _ => error [""]
			    (* end case *);
                            bindVar (env, x))
			| CFG.E_Cast(x, ty, y) => (
			    chkVar (env, y);
(* FIXME: check that x and y have same kinds *)
(*
                            case (V.kindOf x, V.kindOf y) of
                               (CFG.VK_None, CFG.VK_None) => ()
                             | (CFG.VK_Let _, CFG.VK_Let _) => ()
                             | (CFG.VK_Param _, CFG.VK_Param _) => ()
                             | _ => err["variable ", V.toString x, "@", CFG.varKindToString (V.kindOf x),
                                        " does not match ",
                                        "variable ", V.toString y, "@", CFG.varKindToString (V.kindOf y)];
*)
                            if Ty.equals (V.typeOf x, ty)
                               then ()
                            else err["variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                     " does not match ",
                                     "type ", Ty.toString ty];
			    bindVar (env, x))
			| CFG.E_Label(x, lab) => (
			    chkLabel lab;
                            case L.kindOf lab of
                               CFG.LK_Extern _ => ()
                             | CFG.LK_Local _ => 
                                  if Ty.equals (V.typeOf x, L.typeOf lab)
                                     then ()
                                  else err["variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                           " does not match ",
                                           "label", L.toString lab, ":", Ty.toString (L.typeOf lab)];
			    bindVar (env, x))
			| CFG.E_Literal(x, _) => bindVar (env, x)
			| CFG.E_Select(x, i, y) => let
			    val ty = CFGTy.selectTy(i, V.typeOf y)
				  handle Fail msg => (error [msg]; CFGTy.T_Any)
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
(* FIXME: check the type of x *)
                            (case V.typeOf x of
                                CFGTy.T_Tuple tys => ()
                              | CFGTy.T_OpenTuple tys => ()
                              | _ => err["variable ", V.toString x, ":", Ty.toString (V.typeOf x),
                                         " does not match allocation"]);
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
		      (* end case *))
		fun chkExit (env, xfer) = (case xfer
		       of CFG.StdApply{f, clos, arg, ret, exh} => (
(* FIXME: check type of f *)
			    chkVars (env, [f, clos, arg, ret, exh]))
			| CFG.StdThrow{k, clos, arg} => (
(* FIXME: check type of k *)
			    chkVars (env, [k, clos, arg]))
			| CFG.Apply{f, args} => (
(* FIXME: check type of f *)
			    chkVars (env, f::args))
			| CFG.Goto jmp => chkJump (env, jmp)
			| CFG.If(x, j1, j2) => (
(* FIXME: check type of x *)
			    chkVar (env, x);
			    chkJump (env, j1);
			    chkJump (env, j2))
			| CFG.Switch(x, cases, dflt) => (
(* FIXME: check type of x *)
			    chkVar (env, x);
			    List.app (fn (_, j) => chkJump(env, j)) cases;
			    Option.app (fn j => chkJump(env, j)) dflt)
			| CFG.HeapCheck{szb, gc, nogc} => (
			    chkJump (env, gc);
			    chkJump (env, nogc))
		      (* end case *))
		and chkJump (env, (lab, args)) = (
		      chkLabel lab;
(* FIXME: gcCall in HeapCheck are Extern, not Block *)
(*
                      case LMap.find (lMap, lab) of
                         SOME (CFG.Block ys) => ()
                        | _ => err["label ", L.toString lab, " is not Block"];
*)
		      chkVars (env, args))
		val env = chkEntry entry
		val env = List.foldl chkExp env body
		in
		  chkExit (env, exit)
		end (* chk *)
	  in
	    List.app chk code
	  end (* check *)

  end
