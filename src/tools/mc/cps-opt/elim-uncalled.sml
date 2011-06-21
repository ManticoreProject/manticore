(* elim-uncalled.sml
 *
 * COPYRIGHT (c) The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Eliminate all known uncalled functions. 
 *)

structure ElimUncalled : sig

    val transform : CPS.module -> CPS.module

  end = struct

    structure C = CPS
    structure CV = C.Var
    structure VMap = CV.Map
    structure ST = Stats
    structure Census = CPSCensus


  (***** controls ******)
    val elimFlg = ref true
    val elimDebug = ref false

    val () = List.app (fn ctl => ControlRegistry.register CPSOptControls.registry {
              ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
              envName = NONE
            }) [
              Controls.control {
                  ctl = elimFlg,
                  name = "elim",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "enable eliminate uncalled"
                },
              Controls.control {
                  ctl = elimDebug,
                  name = "elim-debug",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "debug eliminate uncalled"
                }
            ]


  (********** Counters for statistics **********)

    val cntElim	= ST.newCounter "elim:eliminated"


  (***** transform ******)

    (*
     * For each function binding, if it is uncalled:
     * - and unused, remove
     * - but used, convert to a constant
     *)
    fun partitionFBs (fbs) = let
        fun partition ([], needsConst, keep) = (needsConst, List.rev keep)
          | partition ((fb as C.FB{f,...})::fbs, needsConst, keep) = (
            case (CFACPS.isUncalled f, CV.useCount f)
             of (true, 0) => (
                if !elimDebug
                then print(concat["Elim w/o constant: ", CV.toString f, "\n"])
                else ();
                ST.tick cntElim;
                partition(fbs, needsConst, keep))
              | (true, _) => (
                if !elimDebug
                then print(concat["Elim w/ constant: ", CV.toString f, "\n"])
                else ();
                ST.tick cntElim;
                partition(fbs, fb::needsConst, keep))
              | (_, _) => partition(fbs, needsConst, fb::keep))
    in
        partition(fbs, [], [])
    end

    fun convertToConst(C.FB{f,...}, g) = let
        val t = CV.new ((CV.nameOf f) ^ "Elim", CPSTy.T_Any)
    in
        C.mkLet([t], C.Const(Literal.Enum 0w0, CPSTy.T_Any),
                C.mkLet([f], C.Cast (CV.typeOf f, t), g))
    end

    fun doExp (C.Exp(ppt, t)) = (case t
	   of C.Let(lhs, rhs, e) => C.mkLet(lhs, rhs, doExp e)
	    | C.Fun(fbs, e) => let
                  val (needsConst, keep) = partitionFBs fbs
                  val keep = List.map doFB keep
                  val newFun = if (length keep = 0)
                               then doExp e
                               else C.mkFun (keep, doExp e)
              in
                  List.foldr (fn (l, r) => convertToConst (l, r)) newFun needsConst
              end
	    | C.Cont(fb, e) => let
                  val (needsConst, keep) = partitionFBs [fb]
                  val keep = List.map doFB keep
                  val newFun = (case (length keep)
                                 of 0 => doExp e
                                  | 1 => C.mkCont (hd keep, doExp e)
                                  | _ => raise Fail "Continuations cannot have more than one fb.")
              in
                  List.foldr (fn (l, r) => convertToConst (l, r)) newFun needsConst
              end
	    | C.If(x, e1, e2) => C.mkIf(x, doExp e1, doExp e2)
	    | C.Switch(x, cases, dflt) => C.mkSwitch(
		x,
		List.map (fn (tag, e) => (tag, doExp e)) cases,
		Option.map doExp dflt)
	    | C.Apply(f, args, rets) => C.mkApply(f, args, rets)
	    | C.Throw(k, args) => C.mkThrow(k, args)
	  (* end case *))
    and doFB (C.FB{f, params, rets, body}) =
        C.FB{f=f, params=params, rets=rets, body=doExp body}

    fun transform (m as C.MODULE{name, externs, body}) =
	  if !elimFlg
	    then let
              val m' = C.MODULE{name=name, externs=externs, body=doFB body}
              val _ = Census.census m'
	      in
                  m'
	      end
	    else m

  end

