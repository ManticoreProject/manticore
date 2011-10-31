(* common-subexpression-elimination.sml
 *
 * COPYRIGHT (c) The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure CommonSubexpressionElimination : sig

    val transform : CPS.module -> CPS.module

  end = struct

    structure C = CPS
    structure CV = C.Var
    structure VMap = CV.Map
    structure ST = Stats
    structure Census = CPSCensus


  (***** controls ******)
    val cseFlg = ref true
    val cseDebug = ref false

    val () = List.app (fn ctl => ControlRegistry.register CPSOptControls.registry {
              ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
              envName = NONE
            }) [
              Controls.control {
                  ctl = cseFlg,
                  name = "cse",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "enable common-subexpression-elimination"
                },
              Controls.control {
                  ctl = cseDebug,
                  name = "cse-debug",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "debug common-subexpression-elimination"
                }
            ]


  (********** Counters for statistics **********)

    val cntElim	= ST.newCounter "cse:elim"


  (***** var to var substitution ******)

    fun subst (env, x) = (case VMap.find(env, x)
	   of SOME y => y
	    | NONE => x
	  (* end case *))


  (* apply a substitution to a list of variables *)
    fun subst' (env, []) = []
      | subst' (env, x::xs) = subst(env, x) :: subst'(env, xs)


  (***** transform ******)

    (*
     * This transformation tracks selections, prims,
     * and allocs. For any tuple variable x, it will record a list
     * of i*y where each i is an index and y is the name of
     * another variable bound to that index.
     * When there is a hit in that map, switch the uses of
     * x[i] for y in the rest of the program. When there is
     * not, add an entry to the cseMapSelect.
     * The behavior for prims and allocs is similar.
     * We only track pure prims and allocs all of whose
     * elements are constants.
     *)

    fun doExp (env, cseMapSelect, cseListPrim, cseListAlloc, C.Exp(ppt, t)) = 
        (case t
	  of C.Let(lhs as [l], rhs as C.Select(i, x), e) => let
                 val x = subst(env, x)
             in
                 case VMap.find (cseMapSelect, x)
                  of SOME x' =>
                     (case List.find (fn (x,_) => x=i) x'
                       of SOME (_, l') => 
                          (case CV.typeOf l'
                            of CPSTy.T_Tuple(_,_) => (
                               ST.tick cntElim;
                               doExp(VMap.insert(env, l, l'), cseMapSelect, cseListPrim, cseListAlloc, e))
                             | _ => (* This error occurs when we have added a variable that is selected
                                      * into elsewhere, but that variable does not actually have a tuple
                                      * type.*)
                               C.mkLet(lhs, C.Select(i, x), doExp(env, cseMapSelect, cseListPrim, cseListAlloc, e)))
                        | NONE => let
                              val cseMapSelect = VMap.insert (cseMapSelect, x, (i, l)::x')
                          in
                              C.mkLet(lhs, C.Select(i, x), doExp(env, cseMapSelect, cseListPrim, cseListAlloc, e))
                          end)
                   | NONE => let
                         val cseMapSelect = VMap.insert (cseMapSelect, x, [(i, l)])
                     in
                         C.mkLet(lhs, C.Select(i, x), doExp(env, cseMapSelect, cseListPrim, cseListAlloc, e))
                     end
             end
           | C.Let(lhs as [l], rhs as C.Prim p, e) => let
		 val p = PrimUtil.map (fn (x) => subst(env, x)) p
	       in
                 if PrimUtil.isPure p  (* only do cse on pure (no side-effects) prim operations *)
		   then
                     case List.find (fn (x,_) => (PrimUtil.nameOf x = PrimUtil.nameOf p) andalso (ListPair.allEq CV.same (PrimUtil.varsOf x, PrimUtil.varsOf p))) cseListPrim
		      of SOME (_, l') => (
			 ST.tick cntElim;
			 doExp(VMap.insert(env, l, l'), cseMapSelect, cseListPrim, cseListAlloc, e))
		       | NONE => C.mkLet(lhs, C.Prim p, doExp(env, cseMapSelect, (p, l)::cseListPrim, cseListAlloc, e))
		   else
                     C.mkLet(lhs, C.Prim p, doExp(env, cseMapSelect, cseListPrim, cseListAlloc, e))
               end
	   | C.Let(lhs as [l], rhs as C.Alloc(CPSTy.T_Tuple(false, tys), vars), e) => let
		 val vars = subst'(env, vars)
	     in
		 (* we need to make sure the elements of the alloc are constant literals *)
		 if List.all (fn v => (case v of (VarRep.V{kind = ref(C.VK_Let(C.Const _)), ...}) => true | _ => false)) vars
		   then
		     case List.find (fn (vars', _) => 
                                        ListPair.allEq (fn (v, v') => 
                                                           (case (v, v') 
                                                              of (VarRep.V{kind = ref(C.VK_Let(C.Const(lit,ty))), ...}, VarRep.V{kind = ref(C.VK_Let(C.Const(lit',ty'))), ...}) =>
							              CPSTyUtil.equal (ty, ty') andalso  Literal.same (lit, lit')
                                                               | _ => false)) (vars, vars')) cseListAlloc
		      of SOME (_, l') => (
			 ST.tick cntElim;
			 doExp(VMap.insert(env, l, l'), cseMapSelect, cseListPrim, cseListAlloc, e))
		       | NONE => C.mkLet(lhs, C.Alloc(CPSTy.T_Tuple(false, tys), vars), doExp(env, cseMapSelect, cseListPrim, (vars, l)::cseListAlloc, e))
		   else
		     C.mkLet(lhs, C.Alloc(CPSTy.T_Tuple(false, tys), vars), doExp(env, cseMapSelect, cseListPrim, cseListAlloc, e))
             end
           | C.Let(lhs, rhs, e) => C.mkLet(lhs, CPSUtil.mapRHS (fn (x) => subst (env, x)) rhs, doExp(env, cseMapSelect, cseListPrim, cseListAlloc, e))
	   | C.Fun(fbs, e) =>
             C.mkFun(List.map (fn (x) => doFB (env, cseMapSelect, cseListPrim, cseListAlloc, x)) fbs, doExp(env, cseMapSelect, cseListPrim, cseListAlloc, e))
	   | C.Cont(fb, e) => 
             C.mkCont(doFB (env, cseMapSelect, cseListPrim, cseListAlloc, fb), doExp(env, cseMapSelect, cseListPrim, cseListAlloc, e))
	   | C.If(x, e1, e2) => C.mkIf(CondUtil.map (fn (v) => subst (env, v)) x, doExp(env, cseMapSelect, cseListPrim, cseListAlloc, e1), doExp(env, cseMapSelect, cseListPrim, cseListAlloc, e2))
	   | C.Switch(x, cases, dflt) =>
             C.mkSwitch(
	     subst(env, x),
	     List.map (fn (tag, e) => (tag, doExp(env, cseMapSelect, cseListPrim, cseListAlloc, e))) cases,
	     Option.map (fn e => doExp (env, cseMapSelect, cseListPrim, cseListAlloc, e)) dflt)
	   | C.Apply(f, args, rets) => C.mkApply(subst(env, f), subst'(env, args), subst'(env, rets))
	   | C.Throw(k, args) => C.mkThrow(subst(env, k), subst'(env, args)))
    and doFB (env, cseMapSelect, cseListPrim, cseListAlloc, C.FB{f, params, rets, body}) =
        C.FB{f=f, params=params, rets=rets, body=doExp (env, cseMapSelect, cseListPrim, cseListAlloc, body)}

    fun transform (m as C.MODULE{name, externs, body}) =
	  if !cseFlg
	    then let
	      val body = doFB (VMap.empty, VMap.empty, [], [], body)
              val m' = C.MODULE{name=name, externs=externs, body=C.mkLambda (body, false)}
              val _ = Census.census m'
	      in
                  m'
	      end
	    else m

  end

