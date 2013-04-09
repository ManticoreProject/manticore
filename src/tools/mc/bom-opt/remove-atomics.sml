(* case-simplify.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure RemoveAtomics : sig

    val transform : BOM.module -> BOM.module

  end = struct

    structure B = BOM
    structure BV = BOM.Var
    structure BTy = BOMTy
    structure CFA = CFABOM
    structure BTU = BOMTyUtil
    structure Lit = Literal
    structure BU = BOMUtil
    structure VSet = B.Var.Set

  (* controls *)
    val removeFlg = ref false
    val removeDebug = ref true

    val () = List.app (fn ctl => ControlRegistry.register BOMOptControls.registry {
	      ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
	      envName = NONE
	    }) [
	      Controls.control {
		  ctl = removeFlg,
		  name = "enable-remove",
		  pri = [0, 1],
		  obscurity = 0,
		  help = "enable atomic removal"
		},
	      Controls.control {
		  ctl = removeDebug,
		  name = "remove-debug",
		  pri = [0, 1],
		  obscurity = 0,
		  help = "debug atomic removal"
		}
	    ]

    (* Effects *)
    datatype value
      = PURE
      | ATOMIC
      | MUTATE
      | TOP

    (* property to track the effects of a program point *)
    val {getFn=getEffect, setFn=setEffect,...} =
        ProgPt.newProp (fn x => PURE)

    (* map from BVs corresponding to funs to their effect *)
    val {getFn=getFunEffect, setFn=setFunEffect,...} =
        BV.newProp (fn x => PURE)

    fun analyze (module as B.MODULE{name, externs, hlops, rewrites, body}) = let
	val changed = ref false
        fun merge (e1, e2) =
            case (e1,e2)
             of (PURE, PURE) => (PURE, false)
              | (PURE, x) => (x, true)
              | (x, PURE) => (x, false)
              | (ATOMIC, MUTATE) => (MUTATE, true)
              | (ATOMIC, TOP) => (TOP, true)
              | (MUTATE, TOP) => (TOP, true)
              | (MUTATE, ATOMIC) => (MUTATE, false)
              | (TOP, _) => (TOP, false)

        fun updateEffect (ppt, eff) =
            let
                val (combined, new) = merge (getEffect ppt, eff)
            in
                if new
                then (changed := true;
                      setEffect (ppt, combined);
                      combined)
                else combined
            end

        fun updateFunEffect (f, eff) =
            let
                val (combined, new) = merge (getFunEffect f, eff)
            in
                if new
                then (changed := true;
                      setFunEffect (f, combined);
                      combined)
                else combined
            end

        fun effectOfVar v =
            case CFABOM.valueOf v
             of CFABOM.TOP => TOP
              | CFABOM.BOT => PURE
              | CFABOM.HLOPC _ => PURE
              | CFABOM.TUPLE _ => raise Fail "Cannot apply a tuple"
              | CFABOM.LAMBDAS lams => (VSet.foldr (fn (v, e) => 
                                                          #1(merge (e, getFunEffect v)))
                                                   PURE lams)


	fun doExp (e as B.E_Pt(ppt, t)) : value = (case t
		 of B.E_Let(lhs, e1, e2) => (
                    let
                        val eff1 = doExp e1
                        val eff2 = doExp e2
                    in
                        updateEffect (ppt, eff1);
                        updateEffect (ppt, eff2)
                    end)
		  | B.E_Stmt(lhs, rhs, e) => updateEffect (ppt, doExp e)
		  | B.E_Fun(fbs, e) => (
                    List.app doLambda fbs;
                    updateEffect (ppt, doExp e))
		  | B.E_Cont(fb, e) => (doLambda fb; updateEffect (ppt, doExp e))
		  | B.E_If(x, e1, e2) => (
                    let
                        val eff1 = doExp e1
                        val eff2 = doExp e2
                    in
                        updateEffect (ppt, eff1);
                        updateEffect (ppt, eff2)
                    end)
		  | B.E_Case(x, cases, dflt) => (
                    let
                        val caseEffects = List.map (fn (_, e) => doExp e) cases
                        val merged = List.foldr (fn (eff, prior) => #1(merge (prior, eff))) PURE caseEffects
                        val final = case dflt
                                     of NONE => merged
                                      | SOME e => #1(merge(merged,doExp e))
                    in
                        updateEffect (ppt, final)
                    end)
		  | B.E_Apply (v, _, _) => (
                    let
                        val VarRep.V{name,...} = v
                        (* TODO:
                         * The right way to handle this is, instead of using literal names,
                         * to extend either the type system or properties of identifiers so
                         * that we track their mutability status.
                         * Right now, we are not identity-aware (e.g., another "Array.array"
                         * would confuse us).
                         *)
                        val effect = (
                            case name
                             of "Array.array" => MUTATE
                              | "Array.sub" => MUTATE
                              | "Array.update" => MUTATE
                              | "Ref.new" => MUTATE
                              | "Ref.get" => MUTATE
                              | "Ref.set" => MUTATE
                              | "TicketLock.lock" => ATOMIC
(*                              | "TicketLock.unlock" => ATOMIC *)
                              | "TicketLock.lockWithTicket" => ATOMIC
                              | _ => effectOfVar v)
                    in
                        updateEffect (ppt, effect)
                    end)
		  | B.E_Throw (v, _)  => updateEffect (ppt, effectOfVar v)
		  | B.E_Ret _ => PURE
		  | B.E_HLOp(hlOp, args, rets) => PURE)
	  and doLambda (B.FB{f, params, exh, body}) =
              (ignore (updateFunEffect (f, doExp body)))
	  val body = doLambda body
	  in
	    if !changed
	    then analyze module
	    else ()
	  end

    fun remove (B.MODULE{name, externs, hlops, rewrites, body}) = let
	  fun doExp (e as B.E_Pt(ppt, t)) = (case t
		 of B.E_Let(lhs, e1 as B.E_Pt(ppt1, _), e2 as B.E_Pt(ppt2, _)) => (
                    case (getEffect ppt1, getEffect ppt2)
                     of (ATOMIC, PURE) => (
                        if !removeDebug then (print "Removed expression\n") else ();
                        doExp e2)
                      | _ => B.mkLet(lhs, doExp e1, doExp e2))
		  | B.E_Stmt(lhs, rhs, e) => B.mkStmt(lhs, rhs, doExp e)
		  | B.E_Fun(fbs, e) => B.mkFun(List.map cvtLambda fbs, doExp e)
		  | B.E_Cont(fb, e) => B.mkCont(cvtLambda fb, doExp e)
		  | B.E_If(x, e1, e2) => B.mkIf(x, doExp e1, doExp e2)
		  | B.E_Case(x, cases, dflt) => B.mkCase(x,
		      List.map (fn (p, e) => (p, doExp e)) cases,
		      Option.map doExp dflt)
		  | B.E_Apply _ => e
		  | B.E_Throw _ => e
		  | B.E_Ret _ => e
		  | B.E_HLOp(hlOp, args, rets) => e)
	  and cvtLambda (B.FB{f, params, exh, body}) =
                  B.FB{f=f, params=params, exh=exh, body=doExp body}
	  val body = cvtLambda body
    in
        B.mkModule(name, externs, hlops, rewrites, body)
    end

    fun transform module =
        if !removeFlg
        then (let
                  val _ = analyze module
              in
                  remove module
              end)
        else module
             
  end
