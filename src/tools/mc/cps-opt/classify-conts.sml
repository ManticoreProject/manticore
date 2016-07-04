(* classify-conts.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure ClassifyConts : sig

    val analyze : CPS.module -> unit

  (* the different kinds of continuations *)
    datatype cont_kind
      = JoinCont        (* a join-point; all uses are throws that are in the
                         * same function as the binding site of the
                         * continuation
                         *)
      | ReturnCont      (* passed as return/exception-handler argument to
                         * function.
                         *)
      | ParamCont       (* bound as parameter *)
      | OtherCont       (* continuation that escapes in some-other way (i.e.,
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

    val () = List.app (fn ctl => ControlRegistry.register CPSOptControls.registry {
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


    type context = C.var        (* function or continuation that encloses
                                 * the expression in question.
                                 *)

  (* the different kinds of continuations *)
    datatype cont_kind
      = JoinCont        (* a join-point; all uses are throws that are in the
                         * same function as the binding site of the
                         * continuation
                         *)
      | ReturnCont      (* passed as return/exception-handler argument to
                         * function.
                         *)
      | ParamCont       (* bound as parameter *)
      | OtherCont       (* continuation that escapes in some-other way (i.e.,
                         * a first-class continuation)
                         *)

    fun kindToString JoinCont = "JoinCont"
      | kindToString ReturnCont = "ReturnCont"
      | kindToString ParamCont = "ParamCont"
      | kindToString OtherCont = "OtherCont"

  (********** Counters for statistics **********)
    val cntJoinCont         = ST.newCounter "classify-conts:join-cont"
    val cntReturnCont   = ST.newCounter "classify-conts:return-cont"
    (*val cntParamCont  = ST.newCounter "classify-conts:param-cont"*)
    val cntOtherCont    = ST.newCounter "classify-conts:other-cont"
    val cntTotalCont    = ST.newCounter "classify-conts:total-conts"

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
    
    (* used when determining whether a continuation escapes as an
       argument to a throw/apply, or used in a RHS of a let. *)
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
               
               (* FIXME right now this would mark a cont that is simply renamed as Other. *)
                CPSUtil.appRHS doArg rhs; 
                
                analExp (outer, e))
                
            | C.Fun(fbs, e) => let
                fun doFB (C.FB{f, body, rets as [retk], ...}) = 
                        analExp ({lambdaV = f, retk = retk, exn = #exn(outer)}, body)
                  
                  | doFB (C.FB{f, body, rets as [retk, exn], ...}) =
                        analExp ({lambdaV = f, retk = retk, exn = exn}, body)
                        
                in
                  List.app doFB fbs;
                  analExp (outer, e)
                end
                
            | C.Cont(C.FB{f, body, ...}, e) => (
              (* initialize properties for this continuation *)
                setOuter (f, #lambdaV(outer));
        
                if (CV.useCount f = CV.appCntOf f)
                  then markAsJoin f
                  else markAsReturn f;
          
              (* analyse its body, preserving the retk & exn, however *)
                analExp ({lambdaV = f, 
                          retk = #retk(outer),
                          exn = #exn(outer)}, body);
        
                if List.null (usesOf f)
                  then ()
                  else (* we don't support recursive join conts *)
                    markAsOther f;
            
                analExp (outer, e);
          
                (case kindOf f
                 of JoinCont => (
                        if List.all (checkUse (#lambdaV(outer))) (usesOf f)
                                then () (* it remains a join continuation *)
                                else (markAsOther f; clrUses f)
                        )
          | _ => ());
          
        if Controls.get CPSOptControls.debug
                  then print(concat[
                      "ClassifyConts: kindOf(", CV.toString f, ") = ",
                      kindToString(kindOf f), "\n"
                    ])
                  else ();
          
          (* collect statistics in one place *)
          ST.tick cntTotalCont;
          
          (case kindOf f
            of JoinCont => ST.tick cntJoinCont
             | ReturnCont => ST.tick cntReturnCont
             | OtherCont => ST.tick cntOtherCont
          (* esac *))
        )
          
            | C.If(_, e1, e2) => (analExp(outer, e1); analExp(outer, e2))
            | C.Switch(_, cases, dflt) => (
                List.app (fn (_, e) => analExp(outer, e)) cases;
                Option.app (fn e => analExp(outer, e)) dflt)
            
            | C.Apply(_, args, rets as (retk :: _)) => let
                val currentRetk = #retk(outer)
                
                (* TODO: mark the ret and exn continuations as such if they're passed
                         in this Apply *)
                
                        (* a better debug message might be nice :) *)
                val _ = if CV.same(retk, currentRetk)
                        then print "tail call\n"
                        else print ("found a non-tail call. " 
                                ^ (CV.toString currentRetk) ^ " ~> " 
                                ^ (CV.toString retk) ^ "\n")
            in
                List.app doArg args
            end
            
            | C.Throw(k, args) => (
                (* TODO now that we have the current retk & exn in the outer context,
                        we can classify this as throwing an exception, a return, or something else. *)
                addUse (#lambdaV(outer), k);
                List.app doArg args)
          (* end case *))

    fun analyze (C.MODULE{body=C.FB{f, body, rets as [retk, exn], ...}, ...}) = 
        analExp ({lambdaV = f, retk = retk, exn = exn} , body)

  (* return the kind of a continuation *)
    fun kindOfCont k = (case CV.kindOf k
           of C.VK_Cont _ => kindOf k
            | C.VK_Param _ => ParamCont
            | _ => OtherCont
          (* end case *))

  (* is k a join continuation? if the optimization is disabled, we always say no *)
    fun isJoinCont k = !enableFlg
          andalso (case kindOfCont k
             of JoinCont => true
              | _ => false
            (* end case *))

  end
