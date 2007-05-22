(* expand-hlops.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure ExpandHLOps : sig

  (* replace the high-level operators in the module with their definitions; returns
   * NONE if there was no change to the module.
   *)
    val expand : BOM.module -> BOM.module option

  end = struct

    structure B = BOM
    structure BTy = BOMTy
    structure BU = BOMUtil
    structure H = HLOp
    structure ATbl = AtomTable
    structure VTbl = B.Var.Tbl

    fun expand (module as B.MODULE{name, externs, body}) = let
	  val changed = ref false
	(* a table of the lambdas that we are adding to the module.  The domain is the
	 * lambda names from the HLOp environment, while the range are fresh copies of
	 * the lambdas.
	 *)
	  val lambdas = VTbl.mkTable (16, Fail "lambda table")
	  fun applyHLOp (lambda as B.FB{f, ...}, args, rets) = let
		val f = (case VTbl.find lambdas f
		       of SOME(B.FB{f, ...}) => f
			| NONE => let
			    val lambda as B.FB{f=f', ...} = BU.copyLambda lambda
			    in
			      VTbl.insert lambdas (f, lambda);
			      f'
			    end
		      (* end case *))
		in
		  B.mkApply(f, args, rets)
		end
	(* initialize the import environment with the current list of external
	 * C functions.
	 *)
	  val importEnv = let
		val importEnv = ATbl.mkTable (32, Fail "importEnv")
		fun ins (cf as CFunctions.CFun{name, ...}) =
		      ATbl.insert importEnv (Atom.atom name, cf)
		in
		  List.app ins externs;
		  importEnv
		end
	(* the original number of externs *)
	  val nExterns = ATbl.numItems importEnv
	(* get the list of externs from the importEnv *)
	  fun getExterns () = if (nExterns = ATbl.numItems importEnv)
		then externs (* no change *)
		else ATbl.listItems importEnv
	  fun cvtExp (e as B.E_Pt(_, t)) = (case t
		 of B.E_Let(lhs, e1, e2) => B.mkLet(lhs, cvtExp e1, cvtExp e2)
		  | B.E_Stmt(lhs, rhs, e) => B.mkStmt(lhs, rhs, cvtExp e)
		  | B.E_Fun(fbs, e) => B.mkFun(List.map cvtLambda fbs, cvtExp e)
		  | B.E_Cont(fb, e) => B.mkCont(cvtLambda fb, cvtExp e)
		  | B.E_If(x, e1, e2) => B.mkIf(x, cvtExp e1, cvtExp e2)
		  | B.E_Case(x, cases, dflt) => B.mkCase(x,
		      List.map (fn (p, e) => (p, cvtExp e)) cases,
		      Option.map cvtExp dflt)
		  | B.E_Apply _ => e
		  | B.E_Throw _ => e
		  | B.E_Ret _ => e
		  | B.E_HLOp(hlOp, args, rets) => let
		      val {inline, defn} = HLOpDefLoader.load(importEnv, hlOp)
		      in
			changed := true;
			if inline
			  then BU.applyLambda(defn, args, rets)
			  else applyHLOp(defn, args, rets)
		      end
		(* end case *))
	  and cvtLambda (B.FB{f, params, exh, body}) =
		B.FB{f=f, params=params, exh=exh, body=cvtExp body}
	  val body = cvtLambda body
	  val body = (case VTbl.listItems lambdas
		 of [] => body
		  | fbs => let
		      val B.FB{f, params, exh, body} = body
		      in
			B.FB{f=f, params=params, exh=exh, body=B.mkFun(fbs, body)}
		      end
		(* end case *))																			
	  in
	    if !changed
	      then SOME(B.mkModule(name, getExterns(), body))
	      else NONE
	  end

  end
