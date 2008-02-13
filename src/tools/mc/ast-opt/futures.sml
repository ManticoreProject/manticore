(* futures.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This file includes definitions of future, touch, and cancel, for use
 * in transformations of intermediate languages.
 *)

structure Futures : sig

    val futureTyc : Types.tycon
    val futureTy  : Types.ty -> Types.ty

    val future : Var.var
    val touch  : Var.var
    val cancel : Var.var

    val future1Spawn  : Var.var
    val future1Touch  : Var.var
    val future1Cancel : Var.var

    val mkFuture  : AST.exp -> AST.exp 
    val mkTouch   : AST.exp -> AST.exp
    val mkCancel  : AST.exp -> AST.exp

    val mkFuture1Spawn  : AST.exp -> AST.exp
    val mkFuture1Touch  : AST.exp -> AST.exp
    val mkFuture1Cancel : AST.exp -> AST.exp

    val isFutureCand : AST.exp -> bool

  end =

  struct

    structure A = AST
    structure B = Basis
    structure T = Types
    structure U = UnseenBasis
    
    (* fail : string -> 'a *)
    fun fail msg = raise Fail msg

    (* todo : string -> 'a *)
    fun todo thing = fail ("todo: " ^ thing)
			 
    (* futureTyc : T.tycon *)
    val futureTyc = TyCon.newAbsTyc (Atom.atom "future", 1, false)

    (* futureTy : T.ty -> T.ty *)
    fun futureTy t = T.ConTy ([t], futureTyc)

    fun forall mkTy =
	let val tv = TyVar.new (Atom.atom "'a")
	in
	    T.TyScheme ([tv], mkTy (A.VarTy tv))
	end

    fun monoVar (name, ty) = Var.new (name, ty)

    fun polyVar (name, mkTy) = Var.newPoly (name, forall mkTy)
		
    val --> = T.FunTy
    infixr 8 -->

    fun ** (t1, t2) = T.TupleTy [t1, t2]
    infixr 8 **

    val future = polyVar ("future",
 		          fn tv => (Basis.unitTy --> tv) --> futureTy tv)

    val touch = polyVar ("touch",
		         fn tv => futureTy tv --> tv)

    val cancel = polyVar ("cancel",
			  fn tv => futureTy tv --> Basis.unitTy)

    val future1Spawn = polyVar ("future1-spawn",
			         fn tv => (B.unitTy --> tv) --> futureTy tv)

    val future1Touch = polyVar ("future1-touch",
			         fn tv => (futureTy tv) --> tv)

    val future1Cancel = polyVar ("future1-cancel",
			          fn tv => (futureTy tv) --> Basis.unitTy)

    (* mkThunk : A.exp -> A.exp *)
    (* Consumes e; produces (fn u => e) (for fresh u : unit). *)
    fun mkThunk e = A.FunExp (Var.new ("_", Basis.unitTy), e, TypeOf.exp e)

    (* mkFut : var -> A.exp -> A.exp *)
    (* Consumes futvar -> q and e; produces futvar (q, fn u => e). *)
    fun mkFut futvar e = 
	let val te = TypeOf.exp e
	in
	    A.ApplyExp (A.VarExp (futvar, [te]),
			mkThunk e,
			futureTy te)
	end

    (* mkFuture : A.exp * A.exp -> A.exp *)
    val mkFuture = mkFut future
 
    (* mkFuture1 : A.exp * A.exp -> A.exp *)
    val mkFuture1Spawn = mkFut future1Spawn 

    local

	(* isFuture : A.exp -> bool *)
	fun isFuture e = (case TypeOf.exp e
			    of T.ConTy (_, c) => TyCon.same (c, futureTyc)
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
		  of T.ConTy ([t'], c) => if TyCon.same (c, futureTyc) 
					  then t'
					  else raise Fail (mkMsg t')
		   | _ => raise Fail (mkMsg t)
	    end

        (* mkTch : var -> A.exp -> A.exp *)
	fun mkTch touchvar e =
	    if (isFuture e) then
		let val t = typeOfFuture e
		    val touch = A.VarExp (touchvar, [t])
		in
		    A.ApplyExp (touch, e, t)
		end
	    else
		let val ts = Var.toString touchvar
		in 
		    raise Fail (ts ^ ": argument is not a future")
		end

	(* mkCan : var -> A.exp -> A.exp *)
	fun mkCan cancelvar e =
	    if (isFuture e) then
		let val cancel = A.VarExp (cancelvar, [typeOfFuture e])
		in
		    A.ApplyExp (cancel, e, Basis.unitTy)
		end
	    else
		let val cs = Var.toString cancelvar
		in
		    raise Fail (cs ^ ": argument is not a future")
		end

    in

    (* mkTouch : A.exp -> A.exp * A.exp *)
    (* Precondition: The argument must be a future. *)
    (* The function raises Fail if the precondition is not met. *)
    val mkTouch = mkTch touch 

    (* mkTouch1 : A.exp -> A.exp * A.exp *)
    (* Precondition: The argument must be a future. *)
    (* The function raises Fail if the precondition is not met. *)
    val mkFuture1Touch = mkTch future1Touch

    (* mkCancel : A.exp -> A.exp * A.exp *)
    (* Precondition: The argument e1 must be a future. *)
    (* The function raises Fail if the precondition is not met. *)
    val mkCancel = mkCan cancel

    (* mkCancel1 : A.exp -> A.exp * A.exp *)
    (* Precondition: The argument e1 must be a future. *)
    (* The function raises Fail if the precondition is not met. *)
    val mkFuture1Cancel = mkCan future1Cancel

    end (* local *)

    (* isFutureCand : A.exp -> bool *)
    (* Determines whether a particular expression should be made a future or not. *)
    (* TODO: This predicate is minimally sophisticated ATM, but can be made arbitrarily so. *)
    fun isFutureCand e =
	  let fun exp (A.LetExp (b, e)) = binding b orelse exp e
		| exp (A.IfExp (e1, e2, e3, _)) = List.exists exp [e1, e2, e3]
		| exp (A.CaseExp (e, ms, _)) = exp e orelse List.exists match ms
		| exp (A.HandleExp (e, ms, _)) = exp e orelse List.exists match ms
		| exp (A.RaiseExp (e, _)) = exp e
		| exp (A.FunExp (x, e, _)) = true
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
	      and optExp NONE = false
		| optExp (SOME e) = exp e
	      and binding (A.ValBind (_, e)) = exp e
		| binding (A.PValBind (_, e)) = exp e
		| binding (A.FunBind lams) = List.exists lambda lams
	      and lambda (A.FB (_, _, e)) = exp e
	      and match (A.PatMatch (_, e)) = exp e
		| match (A.CondMatch (_, e1, e2)) = exp e1 orelse exp e2
	  in
	      exp e
	  end

  end
