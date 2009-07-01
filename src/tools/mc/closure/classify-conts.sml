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

  (* return the kind of  a continuation *)
    val kindOfCont : CPS.var -> cont_kind

  end = struct

    structure C = CPS
    structure CV = CPS.Var

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

  (* the outer context of a letcont-bound variable *)
    local
      val {peekFn, setFn : C.var * context -> unit, ...} =
	    CV.newProp (fn _ => raise Fail "nesting"
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
	    | r => r := outer :: !r
	  (* end case *))
    fun clrUse k = clrFn k
    fun usesOf k = (case peekFn k
	   of SOME xs => (clrUse k; xs)
	    | NONE => raise Fail "expected uses of join cont"
	  (* end case *))
    end

  (* track the kind of a bound continuation *)
    local
      val {peekFn, setFn : C.var * cont_kind -> unit, ...} =
	    CV.newProp (fn _ => raise Fail "cont kind")
    in
  (* return the kind of  a continuation *)
    fun kindOfCont = (case peekFn k
	   of NONE => OtherCont
	    | SOME kind => kind
	  (* end case *))
    fun markAsJoin k = (setFn(k, JoinCont); initUse k)
    fun markAsReturn k = setFn(k, ReturnCont)
    fun markAsOther k = (case peekFn k
	   of SOME OtherCont => ()
	    | _ => (setFn(k, OtherCont); clrUse k)
	  (* end case *))
    end

    fun doArg x = (case V.kindOf x
	   of C.VK_Cont _ => markAsOther x
	    | _ => ()
	  (* end case *))

  (* given a binding context for a continuation, check uses to see
   * if they are in the same environment.
   *)
    fun checkUse outer = let
	  fun chk k = CV.same(outer, k)
		orelse (case kindOfCont k
		   of JoinCont => (case getOuter k
			 of NONE => false
			  | SOME k => chk k'
			(* end case *))
		    | _ > false
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
		  analExp (outer e)
		end
	    | C.Cont(C.FB{f, body, ...}, e) => (
	      (* initialize properties for this continuation *)
		setOuter (f, outer);
		if (CV.useCount f = CV.appCntOf f)
		  then markAsJoin f
		  else markAsReturn f;
	      (* analyse its body *)
(* what about recursive continuations? *)
		analExp (f, body);
		analExp (outer, e);
		case kindOfCont f
		 of JoinCont => if List.all (checkUse outer) (usesOf f)
		      then () (* it remains a join continuation *)
		      else markAsOther f
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

  end

