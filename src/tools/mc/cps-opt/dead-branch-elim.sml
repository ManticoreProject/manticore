(* dead-branch-elim.sml
 * COPYRIGHT (c) The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Eliminate if branches that CFA tells us will never be used.
 *)

structure BranchElim : sig

    val transform : CPS.module -> CPS.module
   
  end = struct

    structure C = CPS
    structure CFA = CFACPS
    structure P = Prim
    structure COND = CondUtil
    structure ST = Stats (*Keep track of how many branches you eliminate, and how many branches there are total *)
    structure Census = CPSCensus
    (* Will I need more of these? Probably! Who knows?!*)

  (***** controls ******)
    (* turn optimization on and off with boolFlg *)

    val branchDebug = ref false
   
    val () = List.app (fn ctl => ControlRegistry.register CPSOptControls.registry { 
	     ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
	     envName = NONE
	}) [
	    Controls.control { 
	        ctl = branchDebug,
		name = "branch-debug",
		pri = [0, 1],
		obscurity = 0,
		help = "debug dead branch elimination"
	    }
	   ]



  (************ Counters for statistics **********)
	    
    val cntBranch = ST.newCounter "branch:total"
    val cntElim = ST.newCounter "branch:eliminate"


  (****** transform ******)

	    fun doExp (C.Exp(ppt, t)) = (case t
		of C.Let(lhs, rhs, e) => (C.mkLet(lhs, rhs, doExp e))
		 | C.Fun(fbs, e) => (C.mkFun ((List.map doFB fbs), doExp e))
		 | C.Cont(fb, e) => (C.mkCont ((doFB fb), doExp e))
		 | C.If(cond, e1, e2) => (C.mkIf(cond, doExp e1, doExp e2))
		 | C.Switch(x, cases, dflt) => (let val xVal = CFA.valueOf x
						    fun findCase((tag, e)::xs, value) =
							if tag = value
							then e
							else findCase(xs, value)
						    val _ = if !branchDebug
							    then print(concat[CFA.valueToString xVal, "\n"])
							    else ()
						    val _ = ST.tick cntBranch
						in
						    (case xVal
						      of CFA.BOOL(true) => (ST.tick cntElim; doExp (findCase(cases, Word.fromInt 1)))
						       | CFA.BOOL(false) => (ST.tick cntElim; doExp (findCase(cases, Word.fromInt 0)))
						       | _ => C.mkSwitch(
							      x,
							      List.map (fn (tag, e) => (tag, doExp e)) cases,
							      Option.map doExp dflt)
						    (*end case*))
						end
					       )
		 | C.Apply(f, args, rets) => (C.mkApply(f, args, rets))
		 | C.Throw(k, args) => (C.mkThrow(k, args))
		(*end case*))
	    and doFB (C.FB{f, params, rets, body}) = 
		C.FB{f=f, params=params, rets=rets, body=doExp body}
				       
				       

	    fun transform (m as C.MODULE{name, externs, body}) =
		if !CFA.boolFlg
		   then let
			fun debug _ = if !branchDebug
				      then print "Starting branch elimination\n"
				      else ()
			val _ = debug()
			val m' = C.MODULE{name=name, externs=externs, body=doFB body}
			val _ = Census.census m'
		    in
			m'
		    end
		else m

end
