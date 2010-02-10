(* classify-conts.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure ClassifyConts : sig

    val analyze : CPS.module -> unit

  (* the different kinds of continuations *)
    datatype cont_kind
      = JoinCont	(* a join-point; all uses are throws that are in the
			 * same function as the binding site of the
			 * continuation
			 *)
      | ReturnCont	(* passed as return/exception-handler argument to
			 * function.
			 *)
      | ParamCont	(* bound as parameter *)
      | OtherCont	(* continuation that escapes in some-other way (i.e.,
			 * a first-class continuation)
			 *)

    val kindToString : cont_kind -> string

  (* return the kind of  a continuation *)
    val kindOfCont : CPS.var -> cont_kind

  (* is k a join continuation?  This returns false when classification is
   * disabled.
   *)
    val isJoinCont : CPS.var -> bool

  end = struct

    structure C = CPS
    structure CV = CPS.Var
    structure ST = Stats

  (* controls *)
    val enableFlg = ref true

    val () = List.app (fn ctl => ControlRegistry.register ClosureControls.registry {
	      ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
	      envName = NONE
	    }) [
	      Controls.control {
		  ctl = enableFlg,
		  name = "enable-join-opt",
		  pri = [0, 1],
		  obscurity = 0,
		  help = "enable optimization of join continuations"
		}
	    ]


    type context = C.var	(* function or continuation that encloses
				 * the expression in question.
				 *)

  (* the different kinds of continuations *)
    datatype cont_kind
      = JoinCont	(* a join-point; all uses are throws that are in the
			 * same function as the binding site of the
			 * continuation
			 *)
      | ReturnCont	(* passed as return/exception-handler argument to
			 * function.
			 *)
      | ParamCont	(* bound as parameter *)
      | OtherCont	(* continuation that escapes in some-other way (i.e.,
			 * a first-class continuation)
			 *)

    fun kindToString JoinCont = "JoinCont"
      | kindToString ReturnCont = "ReturnCont"
      | kindToString ParamCont = "ParamCont"
      | kindToString OtherCont = "OtherCont"

  (********** Counters for statistics **********)
    val cntJoinCont	= ST.newCounter "clos:join-cont"

  (* the outer context of a letcont-bound variable *)
    local
      val {peekFn, setFn : C.var * context -> unit, ...} =
	    CV.newProp (fn _ => raise Fail "nesting")
    in
    val getOuter = peekFn
    val setOuter = setFn
    end

  (* track the use sites of a possible join-point continuation *)
    local
      val {peekFn, setFn : C.var * context list ref -> unit, clrFn, ...} =
	    CV.newProp (fn _ => ref[])
    in
    fun initUse k = setFn(k, ref[])
    fun addUse (outer, k) = (case peekFn k
	   of NONE => ()
	    | SOME r => r := outer :: !r
	  (* end case *))
    fun clrUses k = clrFn k
    fun usesOf k = (case peekFn k
	   of SOME xs => !xs
	    | NONE => []
	  (* end case *))
    end

  (* track the kind of a bound continuation *)
    local
      val {peekFn, setFn : C.var * cont_kind -> unit, ...} =
	    CV.newProp (fn _ => raise Fail "cont kind")
    in
  (* return the kind of  a continuation *)
    fun kindOf k = (case peekFn k
	   of NONE => OtherCont
	    | SOME kind => kind
	  (* end case *))
    fun markAsJoin k = (setFn(k, JoinCont); initUse k)
    fun markAsReturn k = setFn(k, ReturnCont)
    fun markAsOther k = (case peekFn k
	   of SOME OtherCont => ()
	    | _ => (setFn(k, OtherCont); clrUses k)
	  (* end case *))
    end

    fun doArg x = (case CV.kindOf x
	   of C.VK_Cont _ => markAsOther x
	    | _ => ()
	  (* end case *))

  (* given a binding context for a continuation, check uses to see
   * if they are in the same environment.
   *)
    fun checkUse outer = let
	  fun chk k = CV.same(outer, k)
		orelse (case kindOf k
		   of JoinCont => (case getOuter k
			 of NONE => false
			  | SOME k' => chk k'
			(* end case *))
		    | _ => false
		  (* end case *))
	  in
	    chk
	  end

    fun analExp (outer, C.Exp(_, t)) = (case t
	   of C.Let (_, rhs, e) => (
		CPSUtil.appRHS doArg rhs;
		analExp (outer, e))
	    | C.Fun(fbs, e) => let
		fun doFB (C.FB{f, body, ...}) = analExp (f, body)
		in
		  List.app doFB fbs;
		  analExp (outer, e)
		end
	    | C.Cont(C.FB{f, body, ...}, e) => (
	      (* initialize properties for this continuation *)
		setOuter (f, outer);
		if (CV.useCount f = CV.appCntOf f)
		  then markAsJoin f
		  else markAsReturn f;
	      (* analyse its body *)
		analExp (f, body);
		if List.null (usesOf f)
		  then ()
		  else (* we don't support recursive join conts *)
		    markAsOther f;
		analExp (outer, e);
		if Controls.get ClosureControls.debug
		  then print(concat[
		      "ClassifyConts: kindOf(", CV.toString f, ") = ",
		      kindToString(kindOf f), "\n"
		    ])
		  else ();
		case kindOf f
		 of JoinCont => (
		      if List.all (checkUse outer) (usesOf f)
			then ST.tick cntJoinCont (* it remains a join continuation *)
			else markAsOther f;
		      clrUses f)
		  | _ => ())
	    | C.If(_, e1, e2) => (analExp(outer, e1); analExp(outer, e2))
	    | C.Switch(_, cases, dflt) => (
		List.app (fn (_, e) => analExp(outer, e)) cases;
		Option.app (fn e => analExp(outer, e)) dflt)
	    | C.Apply(_, args, _) => List.app doArg args
	    | C.Throw(k, args) => (
		addUse (outer, k);
		List.app doArg args)
	  (* end case *))

    fun analyze (C.MODULE{body=C.FB{f, body, ...}, ...}) =
	  if !enableFlg then analExp (f, body) else ()

  (* return the kind of a continuation *)
    fun kindOfCont k = (case CV.kindOf k
	   of C.VK_Cont _ => kindOf k
	    | C.VK_Param _ => ParamCont
	    | _ => OtherCont
	  (* end case *))

  (* is k a join continuation? *)
    fun isJoinCont k = !enableFlg
	  andalso (case kindOfCont k
	     of JoinCont => true
	      | _ => false
	    (* end case *))

  end

