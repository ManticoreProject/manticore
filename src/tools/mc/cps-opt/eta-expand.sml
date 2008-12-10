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


  (***** controls ******)
    val expandFlg = ref false

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
                }
            ]


  (********** Counters for statistics **********)

    val cntExpand	= ST.newCounter "eta:expand"
    val cntRename	= ST.newCounter "eta:rename"


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
		      val (env, fbs') = doFB (env, fb)
		      in
			(env, fbs' @ fbs)
		      end
		val (env, fbs) = List.foldr doFB' (env, []) fbs
		in
		  C.mkFun(fbs, doExp(env, e))
		end
(* FIXME: we should generalize the representation to allow mutually recursive continuations! *)
	    | C.Cont(C.FB{f, params, rets, body}, e) =>
		C.mkCont(
		  C.FB{f=f, params=params, rets=rets, body=doExp(env, body)}, doExp(env, e))
	    | C.If(x, e1, e2) => C.mkIf(x, doExp(env, e1), doExp(env, e2))
	    | C.Switch(x, cases, dflt) => C.mkSwitch(
		x,
		List.map (fn (tag, e) => (tag, doExp(env, e))) cases,
		Option.map (fn e => doExp (env, e)) dflt)
	    | C.Apply(f, args, rets) => C.mkApply(subst(env, f), args, rets)
	    | C.Throw(k, args) => C.mkThrow(subst(env, k), args)
	  (* end case *))

  (* we expand functions that have known application sites and more use-sites than
   * applications.
   *)
    and doFB (env, C.FB{f, params, rets, body}) = let
	  val appCnt = CV.appCntOf f
	  val useCnt = CV.useCount f
	  in
	    if (appCnt > 0) andalso (useCnt > appCnt)
	      then let
		val f' = CV.copy f
		val params' = List.map CV.copy params
		val rets' = List.map CV.copy rets
		val env = VMap.insert(env, f, f')
		val fbs = [
			C.FB{
			    f=f, params=params', rets=rets',
			    body= if List.null rets'
			      then C.mkThrow(f', params')
			      else C.mkApply(f', params', rets')
			  },
			C.FB{f=f', params=params, rets=rets, body=doExp(env, body)}
		      ]
		in
		(* adjust census counts *)
		  CV.setCount (f', appCnt+1);
		  CV.appCntRef f' := appCnt+1;
		  CV.setCount (f, useCnt - appCnt);
		(* update stats *)
		  ST.tick cntExpand;
		  ST.bump (cntRename, CV.appCntOf f);
		  (env, fbs)
		end
	      else (env, [C.FB{f=f, params=params, rets=rets, body=doExp(env, body)}])
	  end

    fun transform (m as C.MODULE{name, externs, body}) =
	  if !expandFlg
	    then let
	      val (_, [body]) = doFB (VMap.empty, body)
	      in
		C.MODULE{name=name, externs=externs, body=body}
	      end
	    else m

  end

