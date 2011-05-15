(* eta-expand.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Selectively eta-expand functions that have both known application sites and
 * unknown uses.  Roughly, the transformation is
 *
 *	fun f x = e in e'
 *
 *		===>
 *
 *	fun f' x = e[f'/f] and f x = f' x in e'[f'/f]
 *
 * although we only substitute f' for f at call sites.
 *)

structure EtaExpand : sig

    val transform : CPS.module -> CPS.module

  end = struct

    structure C = CPS
    structure CV = C.Var
    structure VMap = CV.Map
    structure ST = Stats
    structure Census = CPSCensus


  (***** controls ******)
    val expandFlg = ref true
    val etaDebug = ref false

    val () = List.app (fn ctl => ControlRegistry.register CPSOptControls.registry {
              ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
              envName = NONE
            }) [
              Controls.control {
                  ctl = expandFlg,
                  name = "eta-expand",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "enable eta-expansion"
                },
              Controls.control {
                  ctl = etaDebug,
                  name = "eta-expand-debug",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "debug eta-expansion"
                }
            ]


  (********** Counters for statistics **********)

    val cntExpand	= ST.newCounter "eta:expand"


  (***** var to var substitution ******)

    type env = C.var VMap.map

    fun subst (env, x) = (case VMap.find(env, x)
	   of SOME y => y
	    | NONE => x
	  (* end case *))


  (***** transform ******)

    fun doExp (env, C.Exp(ppt, t)) = (case t
	   of C.Let(lhs, rhs, e) => C.mkLet(lhs, rhs, doExp(env, e))
	    | C.Fun(fbs, e) => let
		fun doFB' (fb, (env, fbs)) = let
		      val (env, fbs') = doFB (env, fb, false)
		      in
			(env, fbs' @ fbs)
		      end
		val (env, fbs) = List.foldr doFB' (env, []) fbs
		in
		  C.mkFun(fbs, doExp(env, e))
		end
	    | C.Cont(fb, e) => (
              if isRecursive fb
              then C.mkCont (fb, doExp (env, e))
              else let
                      val (env, fbs) = doFB (env, fb, true)
                  in
                      case fbs
                       of [fb] => C.mkCont (fb, doExp (env, e))
                        | stub::newBody::[] => C.mkCont (newBody,
                                                         C.mkCont (stub,
                                                                   doExp (env, e)))
                        | _ => raise Fail "Unexpected return from doFB"
                  end)
	    | C.If(x, e1, e2) => C.mkIf(x, doExp(env, e1), doExp(env, e2))
	    | C.Switch(x, cases, dflt) => C.mkSwitch(
		x,
		List.map (fn (tag, e) => (tag, doExp(env, e))) cases,
		Option.map (fn e => doExp (env, e)) dflt)
	    | C.Apply(f, args, rets) => C.mkApply(subst(env, f), args, rets)
	    | C.Throw(k, args) => C.mkThrow(subst(env, k), args)
	  (* end case *))
    (* Since we do not have mutually recursive continuations, we cannot eta-expand any
     * continuation that contains a recursive escaping usage of itself.
     *)
    and isRecursive (C.FB{f, params, rets, body}) = let
        fun isSame v = CV.same (f, v)
        fun scanExp (C.Exp (_, t)) = (
            case t
             of C.Let (_, rhs, exp) => List.exists isSame
                                                  (CPSUtil.varsOfRHS rhs) orelse scanExp exp
              | C.Fun (fbs, exp) => List.exists scanFB fbs orelse scanExp exp
              | C.Cont (fb, exp) => scanFB fb orelse scanExp exp
              | C.If (cond, exp1, exp2) => List.exists isSame (CondUtil.varsOf cond) orelse
                                          scanExp exp1 orelse scanExp exp2
              | C.Switch (_, cases, default) => (
                List.exists (fn (_, e) => scanExp e) cases orelse
                case default
                 of NONE => false
                  | SOME e => scanExp e)
        (* Ignore apply/throw target because those are non-escaping uses *)
              | C.Apply (_, args, rets) => List.exists isSame args orelse List.exists isSame rets
              | C.Throw (_, args) => List.exists isSame args
        (* end case *))
        and scanFB (C.FB {body,...}) = scanExp body
    in
        scanExp body
    end 
  (* we expand functions that have known application sites and more use-sites than
   * applications.
   *)
    and doFB (env, C.FB{f, params, rets, body}, isCont) = let
	  val appCnt = CV.appCntOf f
	  val useCnt = CV.useCount f
	  in
	    if (appCnt > 0) andalso (useCnt > appCnt)
	      then let
		val f' = CV.copy f
                val _ = if !etaDebug
                        then print (concat ["Expanding: ", CV.toString f, " NewBody: ", CV.toString f', "\n"])
                        else ()
		val params' = List.map CV.copy params
		val rets' = List.map CV.copy rets
		val env = VMap.insert(env, f, f')
		val fbs = [
			C.FB{
			    f=f, params=params', rets=rets',
			    body= if isCont
			      then C.mkThrow(f', params')
			      else C.mkApply(f', params', rets')
			  },
			C.FB{f=f', params=params, rets=rets, body=doExp(env, body)}
		      ]
		in
		(* update stats *)
		  ST.tick cntExpand;
		  (env, fbs)
		end
	      else (env, [C.FB{f=f, params=params, rets=rets, body=doExp(env, body)}])
	  end

    fun transform (m as C.MODULE{name, externs, body}) =
	  if !expandFlg
	    then let
	      val (_, [body]) = doFB (VMap.empty, body, false)
              val m' = C.MODULE{name=name, externs=externs, body=C.mkLambda (body, false)}
              val _ = Census.census m'
	      in
                  m'
	      end
	    else m

  end

