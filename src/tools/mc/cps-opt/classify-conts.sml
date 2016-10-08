(* classify-conts.sml
 *
 * COPYRIGHT (c) 2016 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure ClassifyConts : sig

    (* bool indicates whether the analysis should assume a direct-style translation. *)
    val analyze : bool -> CPS.module -> unit

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
    
  (* Given a throw of some var k, returns the immediately enclosing function at that site. *)
    val contextOfThrow : CPS.var -> CPS.lambda
    
    (* helper for contextOfThrow. will check the rets of the lambda to see if
       the given var matches one of those. *)
    val checkRets : (CPS.var * CPS.lambda) -> cont_kind option

  (* is k a join continuation?  This returns false when classification is
   * disabled.
   *)
    val isJoinCont : CPS.var -> bool
    
    val isReturnCont : CPS.var -> bool
    
    val isTailApply : CPS.exp -> bool

  end = struct

    structure C = CPS
    structure CV = CPS.Var
    structure ST = Stats
    structure CFA = CFACPS

    (* controls *)
    val enableFlg = ref true
    val usingDS = ref false

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
                         
      | ParamCont       (* bound as parameter to a lambda. *)
      
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
    fun trackUses k = (case peekFn k
        of NONE => initUse k
         | _ => ()
         (* end case *))
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
    fun markAsReturn k = (setFn(k, ReturnCont); trackUses k)
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
    
    (* Mark throws with their immediately enclosing function. This will allow closure
       conversion to determine whether a throw to a Return continuation is a function
       return or just a jump. Example:
       
        fun x () =
            cont foo () =
                ...
            (* end foo *)
            if ...
                then apply bar {retk = foo, ...}
                else throw foo ()
            
       Here, foo is marked as a return continuation, so foo will be a basic block within x,
       but foo also has a throw to it that within x, but this is _not_ a return throw, but merely
       a jump to a continuation that happens to marked as a return continuation.
    *)
    local
        val {getFn, setFn : C.var * C.lambda -> unit, ...} =
              CV.newProp (fn _ => raise Fail "throw kind")
    in
        val contextOfThrow = getFn
        val markThrowContext = setFn
    end

  (* given a binding context for a continuation, check uses to see
   * if they are in the same function environment. 
   *)
    fun checkUse outer = let
          fun chkCPS k = CV.same(outer, k)
                orelse (case kindOf k
                   of JoinCont => (case getOuter k
                         of NONE => false
                          | SOME k' => chkCPS k'
                        (* end case *))
                    | _ => false
                  (* end case *))
                  
          fun chkDS k = CV.same(outer, k)
                orelse (case kindOf k
                   of (JoinCont | ReturnCont) => (case getOuter k
                         of NONE => false
                          | SOME k' => chkDS k'
                        (* end case *))
                    | _ => false
                  (* end case *))
          in
            if !usingDS then chkDS else chkCPS
          end

    
    (* consult CFA to determine the actual continuation, if its known *)
    fun actualCont k = (case CFA.valueOf k
        of CFA.LAMBDAS gs => let
                val gs = CPS.Var.Set.filter (not o CFA.isProxy) gs
            in 
                if CPS.Var.Set.numItems gs = 1 
                    then Option.valOf (CPS.Var.Set.find (fn _ => true) gs)
                    else k 
            end
         | _ => k
         (* esac *))
    
    (* climbs the context, if necessary, to find the immediately enclosing function. *)
    fun enclosingFun outer = (case CV.kindOf outer
        of C.VK_Fun fb => SOME fb
         | C.VK_Cont(C.FB{ f , ...}) => (case getOuter f
             of SOME newOuter => enclosingFun newOuter
              | _ => NONE
             (* esac *))
        | _ => NONE
    (* esac *))

    fun analExp (outer, C.Exp(ppt, t)) = (case t
            of C.Let (_, _, e) => analExp (outer, e)
                
            | C.Fun(fbs, e) => let
                fun doFB (C.FB{f, body, ...}) = analExp (f, body)
                in
                  List.app doFB fbs;
                  analExp (outer, e)
                end
                
            | C.Cont(C.FB{f, body, ...}, e) => let
                val notEscaping = (not o CFA.isEscaping) f
                
                (* JoinCont iff
                     not escaping
                     AND not thrown to recursively (in its own body)
                     AND all uses occur within the same enclosing function
                   
                   Return/Exception Cont iff 
                     not escaping
                     AND passed as such a continuation in at least one Apply
                     AND all uses occur within the same enclosing function
                   
                   GotoCont iff
                     not escaping
                     AND not a return or exception cont
                   
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
          
                (case (kindOf f, !usingDS)
                 of ((JoinCont, _) | (ReturnCont, true))  => (
                        (* do all uses of f occur in the current environment? *)
                        if List.all (checkUse outer) (usesOf f)
                                then () (* it remains as is *)
                                else markAsGoto f (* should be its own function *)
                        )
                  | _ => ());
          
                (*if Controls.get CPSOptControls.debug then*)
                print(concat[
                      "ClassifyConts: kindOf(", CV.toString f, ") = ",
                      kindToString(kindOf f), "\n"
                    ])
                  (*else ()*)
                  ;
                  
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
                
                (* we mark the actual cont *)
                val cfaRets = List.map actualCont rets
                val _ = (case cfaRets
                            of [retk, exnk] => (markAsExn exnk ; markAsReturn retk)
                             | [retk] => (markAsReturn retk)
                             | _ => raise Fail "an apply with unexpected rets"
                        (* esac *))
                        
                (* now we check whether it's a tail call by comparing the retk param
                   with what is passed in this apply. *)
                val retk :: _ = rets
                val SOME encl = enclosingFun outer
                val SOME enclRet = CPSUtil.getRetK encl
                
                (* we're looking to see if v originates from a constant, modulo casts.
                   this is a very specific pattern generated by an optimization.
                   
                   TODO Ideally, we'd be able to ask CFA if it is a constant value to
                   potentially get a more robust answer for this.
                *) 
                fun isConst v = (case CV.kindOf v
                    of C.VK_Let(C.Cast(_, v)) => isConst v
                     | C.VK_Let(C.Const _) => true
                     | _ => false
                    (* esac *))
            in
                (* an Apply is a tail call if
                
                1. the enclosing ret cont is the same as the one passed to the function
                2. the return continuation being passed is "unit", aka, the function does
                   not return normally (some CPS optimization will do this).
                   
                 *)
                markTail(ppt, CV.same(enclRet, retk) orelse isConst retk)
            end
                
            | C.Throw(k, args) => let
                (* we check CFA information to determine if k is an alias for some Cont,
                   and add the use to the actual Cont and not the alias.
                   
                   this may happen if a Cont is casted/rebound, which can be introduced
                   after arity raising. *)
                     val SOME encl = enclosingFun outer
                   in
                    addUse (outer, actualCont k) ; markThrowContext (k, encl)
                   end
                
                
                
          (* end case *))

    fun analyze usingDirectStyle (C.MODULE{body=C.FB{f, body, ...}, ...}) = 
        (usingDS := usingDirectStyle ; analExp (f, body))

  (* return the kind of a continuation *)
    fun kindOfCont k = (case CV.kindOf k
           of C.VK_Cont _ => kindOf k
            | C.VK_Param _ => ParamCont
            | _ => OtherCont
          (* end case *))
    
    and checkRets (k, C.FB{ rets = [retk] , ...}) = 
            if CV.same(k, retk) then SOME ReturnCont else NONE
      | checkRets (k, C.FB{ rets = [retk, exnk] , ...}) = 
            if CV.same(k, retk) then SOME ReturnCont
            else if CV.same(k, exnk) then SOME ExnCont 
            else NONE
      | checkRets _ = NONE

  (* is k a join continuation? if the optimization is disabled, we always say no *)
    fun isJoinCont k = !enableFlg
          andalso (case (kindOfCont o actualCont) k
             of JoinCont => true
              | _ => false
            (* end case *))

  (* is k a return continuation? *)
    fun isReturnCont k = 
        (case (kindOfCont o actualCont) k
            of ReturnCont => true
             | _ => false
            (* esac *))
    

    fun isTailApply (C.Exp(ppt, C.Apply _)) = checkTail ppt

  end
