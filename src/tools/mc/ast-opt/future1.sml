(* future1.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * 
 * Compiler hooks for the Future1 library.
 *)

structure Future1 : sig

    val futureTyc : unit -> Types.tycon

    val futureTy : AST.ty -> AST.ty

    val mkTouch  : AST.exp -> AST.exp
    val mkFuture : AST.exp -> AST.exp
    val mkCancel : AST.exp -> AST.exp

  (* Determines whether a particular expression should be made a future or not. *)
    val isFutureCand : AST.exp -> bool

  end = struct

    structure A = AST
    structure T = Types

    local 

      val futureModuleName = "MultilispFuture"

      fun getTyc id = BasisEnv.getTyConFromBasis [futureModuleName, id]
      fun getVar id = BasisEnv.getVarFromBasis [futureModuleName, id]

      val memoFutureTyc : Types.tycon Memo.memo = Memo.new (fn _ =>
        getTyc "future")

      val memoFuture : AST.var Memo.memo = Memo.new (fn _ =>
        getVar "future")
  
      val memoTouch1 : AST.var Memo.memo = Memo.new (fn _ =>
        getVar "touch")

      val memoCancel1 : AST.var Memo.memo = Memo.new (fn _ =>
        getVar "cancel")

    in

      fun futureTyc () = Memo.get memoFutureTyc
      fun future1   () = Memo.get memoFuture
      fun touch1    () = Memo.get memoTouch1
      fun cancel1   () = Memo.get memoCancel1

    end (* local *)
  
    fun futureTy t = T.ConTy ([t], futureTyc())

    (* mkThunk : A.exp -> A.exp *)
    (* Consumes e; produces (fn u => e) (for fresh u : unit). *)
    fun mkThunk e = A.FunExp (Var.new ("u", Basis.unitTy), e, TypeOf.exp e)		    

    (* isFuture : A.exp -> bool *)
    fun isFuture e = (case TypeOf.exp e
			of T.ConTy (_, c) => TyCon.same (c, futureTyc())
			 | _ => false)

    (* typeOfFuture : A.exp -> T.ty *)
    (* Precondition: The argument must be a future. *)
    (* The function raises Fail if the precondition is not met. *)
    (* ex: typeOfFuture (future (fn () => 8))     ==> int *)
    (* ex: typeOfFuture (future (fn () => 8 > 8)) ==> bool *)
    fun typeOfFuture e =
	let val t = TypeOf.exp e
	    fun mkMsg t = ("typeOfFuture: expected future type, got "
			   ^ (PrintTypes.toString t))
	in
	    case t
	      of T.ConTy ([t'], c) => if TyCon.same (c, futureTyc()) 
				      then t'
				      else raise Fail (mkMsg t')
	       | _ => raise Fail (mkMsg t)
	end

    (* mkTch : var -> A.exp -> A.exp *)
    fun mkTch touchvar e =
	if (isFuture e) then
	    let val t = typeOfFuture e
		val touch = A.VarExp (touchvar(), [t])
	    in
		A.ApplyExp (touch, e, t)
	    end
	else
	    let val ts = Var.toString (touchvar())
	    in 
		raise Fail (ts ^ ": argument is not a future")
	    end

    (* mkCan : var -> A.exp -> A.exp *)
    fun mkCan cancelvar e =
	if (isFuture e) then
	    let val cancel = A.VarExp (cancelvar(), [typeOfFuture e])
	    in
		A.ApplyExp (cancel, e, Basis.unitTy)
	    end
	else
	    let val cs = Var.toString (cancelvar())
	    in
		raise Fail (cs ^ ": argument is not a future")
	    end

    (* mkFut : var -> A.exp -> A.exp *)
    (* Consumes futvar -> q and e; produces futvar (q, fn u => e). *)
    fun mkFut futvar e = let 
      val te = TypeOf.exp e
      val falseExp = AST.ConstExp (AST.DConst (Basis.boolFalse, []))
      val arg = AST.TupleExp [mkThunk e, falseExp]
      in
	A.ApplyExp (A.VarExp (futvar(), [te]), arg, futureTy te)
      end

    (* Precondition: The argument must be a future. *)
    (* The function raises Fail if the precondition is not met. *)
    val mkTouch = mkTch touch1

    (* Precondition: The argument e1 must be a future. *)
    (* The function raises Fail if the precondition is not met. *)
    val mkCancel = mkCan cancel1

    val mkFuture = mkFut future1

    (* isFutureCand : A.exp -> bool *)
    (* Determines whether a particular expression should be made a future or not. *)
    (* TODO: This predicate is minimally sophisticated ATM, but can be made arbitrarily so. *)
    fun isFutureCand e =
	  let fun exp (A.LetExp (b, e)) = binding b orelse exp e
		| exp (A.IfExp (e1, e2, e3, _)) = List.exists exp [e1, e2, e3]
		| exp (A.CaseExp (e, ms, _)) = exp e orelse List.exists match ms
		| exp (A.PCaseExp (es, pms, _)) = exp e orelse List.exists pmatch pms
		| exp (A.HandleExp (e, ms, _)) = exp e orelse List.exists match ms
		| exp (A.RaiseExp (e, _)) = exp e
		| exp (A.FunExp (x, e, _)) = false
		| exp (A.ApplyExp (e1, e2, _)) = true
		| exp (A.VarArityOpExp _) = false
		| exp (A.TupleExp es) = List.exists exp es
		| exp (A.RangeExp (e1, e2, oe3, _)) = 
		    exp e1 orelse exp e2 orelse optExp oe3
		| exp (A.PTupleExp es) = List.exists exp es
		| exp (A.PArrayExp (es, _)) = List.exists exp es
		| exp (A.PCompExp (e, pes, oe)) = 
		    exp e orelse List.exists (fn (p,e) => exp e) pes orelse optExp oe
		| exp (A.PChoiceExp (es, _)) = List.exists exp es
		| exp (A.SpawnExp e) = exp e
		| exp (A.ConstExp _) = false
		| exp (A.VarExp (x, _)) = false
		| exp (A.SeqExp (e1, e2)) = exp e1 orelse exp e2
		| exp (A.OverloadExp _) = false
		| exp (A.ExpansionOptsExp(_, e)) = exp e
		| exp (A.FTupleExp es) = List.exists exp es
		| exp (A.FArrayExp (es, n, _)) = List.exists exp es orelse ntree n
		| exp (A.FlOp oper) = false
		| exp (A.PArrayOp oper) = false
	      and ntree (A.Lf (e1, e2)) = exp e1 orelse exp e2
		| ntree (A.Nd ns) = List.exists ntree ns
	      and optExp NONE = false
		| optExp (SOME e) = exp e
	      and binding (A.ValBind (_, e)) = exp e
		| binding (A.PValBind (_, e)) = exp e
		| binding (A.FunBind lams) = List.exists lambda lams
		| binding (A.PrimVBind _) = raise Fail "todo: PrimVBind"
		| binding (A.PrimCodeBind _) = raise Fail "todo: PrimCodeBind"
	      and lambda (A.FB (_, _, e)) = exp e
	      and match (A.PatMatch (_, e)) = exp e
		| match (A.CondMatch (_, e1, e2)) = exp e1 orelse exp e2
	      and pmatch (A.PMatch (_, e)) = exp e
		| pmatch (A.Otherwise (ts, e)) = exp e
	  in
	      exp e
	  end

  end (* Future1 *)
