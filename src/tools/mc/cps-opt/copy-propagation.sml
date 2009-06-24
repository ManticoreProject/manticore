(* copy-propagation.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This transformation replaces any variables that are not a function definition
 * but which CFA tells us can only be bound to a single function with that function's
 * literal name. Note that we need to respect lexical scoping (which the CFA analysis
 * does not require to be true).
 *)

structure CopyPropagation : sig

    val transform : CPS.module -> CPS.module

  end = struct

    structure PPt = ProgPt
    structure C = CPS
    structure CV = C.Var
    structure VSet = CV.Set
    structure U = CPSUtil
    structure CTy = CPSTy
    structure CFA = CFACPS
    structure ST = Stats

  (***** controls ******)
    val enableCopyPropagation = ref true
    val propagationDebug = ref false

    val () = List.app (fn ctl => ControlRegistry.register CPSOptControls.registry {
              ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
              envName = NONE
            }) [
              Controls.control {
                  ctl = enableCopyPropagation,
                  name = "copy-propagation",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "enable copy propagation"
                },
              Controls.control {
                  ctl = propagationDebug,
                  name = "copy-propagation-debug",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "debug copy propagation"
                  }
            ]


    (***** Statistics *****)
    val cntPropagatedFunctions  = ST.newCounter "cps-copy-propagation:propagated-functions"

    structure VarTree = RedBlackSetFn (struct
		                       type ord_key = CV.var
		                       val compare = CV.compare
		                       end)

    fun copyPropagate (C.MODULE{name,externs,body=(C.FB{f=main,params=modParams,rets=modRets,body=modBody})}) = let
        fun findCopy (f, vt) = (
            case CFA.valueOf f
             of CFA.LAMBDAS (l) => (
                case CV.Set.listItems l
                 of [f'] =>
                    (if CV.compare (f, f') = EQUAL
                     then NONE
                     else
                         (if VarTree.member (vt, f')
                          then (if !propagationDebug
                                then print (concat [CV.toString f', " is being propagated.\n"])
                                else ();
                                SOME (f'))
                          else (if !propagationDebug
                                then print (concat [CV.toString f', " was not in scope for copy-prop.\n"])
                                else ();
                                NONE)))
                  | _ => NONE
                (* end case *))
              | _ => NONE
        (* end case *))
        fun copyPropagateExp (exp as C.Exp(ppt, e), vt) = (
            case e
             of C.Let (vars, rhs, e) => let
                    val (body, vt') = copyPropagateExp (e, vt)
                in (C.mkLet (vars, rhs, body), vt') end
              | C.Fun (lambdas, body) => let
                    val (lambdas, vt') = List.foldr (fn (l, (ls, vt)) => let
                                                                val (l', vt') = copyPropagateLambda (l, vt)
                                                            in (l'::ls, vt') end)
                                                    ([], vt) lambdas
                    val (body, _) = copyPropagateExp (body, vt')
                in
                    (C.mkFun(lambdas, body), vt')
                end
              | C.Cont (f, body) => let
                    val (lambda, vt') = copyPropagateLambda (f, vt)
                    val (body, _) = copyPropagateExp (body, vt')
                in
                    (C.mkCont (lambda, body), vt')
                end
              | C.If (v, e1, e2) => let
                    val (e1', _) = copyPropagateExp (e1, vt)
                    val (e2', _) = copyPropagateExp (e2, vt)
                in
                    (C.mkIf (v, e1', e2'), vt)
                end
              | C.Switch (v, cases, body) => let
                    val switches = List.map (fn (tag,e) => let
                                                    val (body, _) = copyPropagateExp (e, vt)
                                                in (tag, body) end) cases
                    val default = Option.map (fn (x) => let
                                                     val (body, _) = copyPropagateExp (x, vt)
                                                 in body end) body
                in
                    (C.mkSwitch(v, switches, default), vt)
                end
              | C.Apply (f, args, retArgs) => (
                case findCopy (f, vt)
                 of SOME (f') => (ST.tick cntPropagatedFunctions;
                                  Census.decAppCnt f;
                                  Census.incAppCnt f';
                                  (C.mkApply (f', args, retArgs), vt))
                  | NONE => (C.mkApply (f, args, retArgs), vt)
                (* end case *))
              | C.Throw (k, args) => (
                case findCopy (k, vt)
                 of SOME (k') => (ST.tick cntPropagatedFunctions;
                                  Census.decAppCnt k;
                                  Census.incAppCnt k';
                                  (C.mkThrow (k', args), vt))
                  | NONE => (C.mkThrow (k, args), vt)
                (* end case *))
        (* end case *))
        and copyPropagateLambda (lambda as C.FB{f, params, rets, body}, vt) = let
            val vt' = VarTree.add (vt, f)
            val (body, _) = copyPropagateExp (body, vt')
        in
            (C.mkLambda(C.FB{f=f,params=params,rets=rets,body=body}), vt')
        end
        val (body', _) = copyPropagateExp (modBody, VarTree.empty)
    in
        C.MODULE{
	name=name, externs=externs,
	body = C.mkLambda(C.FB{
                          f=main,params=modParams,rets=modRets,
                          body=body'
		         })
	}
    end

    fun transform m =
        if !enableCopyPropagation
	then copyPropagate m
        else m

  end
