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
                         * continuation. can be mapped as a basic block.
                         *)
                         
      | GotoCont        (* similar to a join-point; throw sites are all known, but
                         * at least one site occurs in within another function.
                         * this cont is mapped as a function instead of a block.
                         *)
      
      | ReturnCont      (* passed as a return continuation to another function
                         *)
                         
      | ExnCont         (* passed as an exception handler to another function
                         *)
                         
      | ParamCont       (* bound as parameter (other than return or exception) *)
      
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
    
    val isTailApply : CPS.exp -> bool

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
                         * continuation. can be mapped as a basic block.
                         *)
                         
      | GotoCont        (* similar to a join-point; throw sites are all known, but
                         * at least one site occurs in within another function.
                         * this cont is mapped as a function instead of a block.
                         *)
      
      | ReturnCont      (* passed as a return continuation to another function
                         *)
                         
      | ExnCont         (* passed as an exception handler to another function
                         *)
                         
      | ParamCont       (* bound as parameter to to a lambda (excluding returns or exceptions) *)
      
      | OtherCont       (* continuation that escapes in some-other way (i.e.,
                         * a first-class continuation)
                         *)

    fun kindToString JoinCont = "JoinCont"
      | kindToString ReturnCont = "ReturnCont"
      | kindToString ExnCont = "ExnCont"
      | kindToString ParamCont = "ParamCont"
      | kindToString GotoCont = "GotoCont"
      | kindToString OtherCont = "OtherCont"

  (********** Counters for statistics **********)
    val cntJoinCont     = ST.newCounter "classify-conts:join-cont"
    val cntReturnCont   = ST.newCounter "classify-conts:return-cont"
    val cntExnCont      = ST.newCounter "classify-conts:exn-cont"
    val cntOtherCont    = ST.newCounter "classify-conts:other-cont"
    val cntGotoCont     = ST.newCounter "classify-conts:goto-cont"
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
    fun markAsExn k = setFn(k, ExnCont)
    fun markAsGoto k = (setFn(k, GotoCont); clrUses k)
    fun markAsOther k = (case peekFn k
           of SOME OtherCont => ()
            | _ => (setFn(k, OtherCont); clrUses k)
          (* end case *))
    end
    
    (* check/mark whether an Apply is in a tail position *)
    local
        val {getFn : ProgPt.ppt -> bool, setFn : ProgPt.ppt * bool -> unit }
            = ProgPt.newFlag()
    in
        val checkTail = getFn
        val markTail = setFn
    end
    
    (* check/mark the kind of Throw *)
    local
        
    in
    end
    
    (* used when determining whether a continuation escapes as an
       argument to a throw/apply, or used in a RHS of a let. *)
    fun doArg x = (case CV.kindOf x
           of C.VK_Cont _ => markAsOther x
            | _ => ()
          (* end case *))

  (* given a binding context for a continuation, check uses to see
   * if they are in the same function environment.
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
          
    (* climbs the context, if necessary, to find the immediately enclosing function. *)
    fun enclosingFun outer = (case CV.kindOf outer
        of C.VK_Fun fb => SOME fb
         | C.VK_Cont(C.FB{ f , ...}) => (case getOuter f
             of SOME newOuter => enclosingFun newOuter
              | _ => NONE
             (* esac *))
        | _ => NONE
    (* esac *))
    
    fun getRetK (C.FB{ rets as retk :: _, ...}) = SOME retk
      | getRetK _ = NONE
    
    and getExnK (C.FB{ rets as [_, exnk], ...}) = SOME exnk
      | getExnK _ = NONE
      

    fun analExp (outer, C.Exp(ppt, t)) = (case t
           of C.Let (_, rhs, e) => (
               
               (* FIXME right now this would mark a cont that is simply renamed or
                  casted as Other. 
                  
                  Commenting out the below nearly fixes it since we're now
                  using CFA escaping information earlier, but now arity raising
                  will break things since it introduces a type coercion of a
                  continuation, which is still not escaping, but now closure conversion
                  barfs, probably due to environment stuff changing.
                   *)
                   
                (*CPSUtil.appRHS doArg rhs; *)
                
                analExp (outer, e))
                
            | C.Fun(fbs, e) => let
                fun doFB (C.FB{f, body, ...}) = analExp (f, body)
                in
                  List.app doFB fbs;
                  analExp (outer, e)
                end
                
            | C.Cont(C.FB{f, body, ...}, e) => let
                val notEscaping = (not o CFACPS.isEscaping) f
                
                (* JoinCont iff
                   not escaping (currently we use the app = use metric)
                   AND not thrown to recursively (in its own body)
                   AND all uses occur within the same enclosing function
                   
                   GotoCont iff
                   not escaping (currently we use the app = use metric)
                   AND not a return or exception cont
                   
                   Return/Exception Cont iff passed as such to an Apply
                   
                   OtherCont anything else
                 *)
                
            in (
                (* initialize properties for this continuation *)
                setOuter (f, outer);
                
                if notEscaping
                    then markAsJoin f
                    else markAsOther f;
                    
                (* analyse its body *)
                analExp (f, body);
                
                if (not o List.null) (usesOf f) andalso notEscaping
                        then (* after analyzing the body, we found out
                                that it's a recursive Join, so we turn it into a Goto *)
                            markAsGoto f
                        else ();
                
                analExp (outer, e);
          
                (case kindOf f
                 of JoinCont => (
                        if List.all (checkUse outer) (usesOf f)
                                then () (* it remains a join continuation *)
                                (*else (markAsOther f; clrUses f)*)
                                else markAsGoto f
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
                 | ExnCont => ST.tick cntExnCont
                 | GotoCont => ST.tick cntGotoCont
              (* esac *))
              )
            end
          
            | C.If(_, e1, e2) => (analExp(outer, e1); analExp(outer, e2))
            | C.Switch(_, cases, dflt) => (
                List.app (fn (_, e) => analExp(outer, e)) cases;
                Option.app (fn e => analExp(outer, e)) dflt)
            
            | C.Apply(_, args, rets) => let
            
                val _ = (case rets
                         of [_, exnk] => markAsExn exnk
                          | _ => ()
                        (* esac *))
            
                val retk = List.hd rets
                val _ = markAsReturn retk
                
                (* an Apply is a tail call iff the enclosing ret cont is the same
                   as the one passed to the function *)
                val SOME encl = enclosingFun outer
                val SOME enclRet = getRetK encl
            in
                (markTail(ppt, CV.same(enclRet, retk));
                 List.app doArg args)
            end
                
                (* TODO evaluate whether we even need doArg in Throw/Apply now that we use
                   CFA information to determine whether the continuation escapes. *)
                
            | C.Throw(k, args) => 
                (addUse (outer, k);
                 List.app doArg args)
          (* end case *))

    fun analyze (C.MODULE{body=C.FB{f, body, ...}, ...}) = analExp (f, body)

  (* return the kind of a continuation *)
    fun kindOfCont k = (case CV.kindOf k
           of C.VK_Cont _ => kindOf k
            | C.VK_Param (C.FB{ rets ,...}) => checkRets (k, rets)
            | _ => OtherCont
          (* end case *))
    
    and checkRets (k, [retk]) = if CV.same(k, retk) then ReturnCont else ParamCont
      | checkRets (k, [retk, exnk]) = if CV.same(k, retk) then ReturnCont else
                                      if CV.same(k, exnk) then ExnCont else ParamCont
      | checkRets _ = ParamCont

  (* is k a join continuation? if the optimization is disabled, we always say no *)
    fun isJoinCont k = !enableFlg
          andalso (case kindOfCont k
             of JoinCont => true
              | _ => false
            (* end case *))

    fun isTailApply (C.Exp(ppt, C.Apply _)) = checkTail ppt

  end
