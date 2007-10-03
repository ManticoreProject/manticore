(* futures.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This file includes definitions of future, touch, and cancel, for use
 * in transformations of intermediate languages.
 *)

structure Futures (* : sig

    val futureTyc : Types.tycon
    val futureTy  : Types.ty -> Types.ty

    val newWorkQueue : Var.var

    val future : Var.var
    val touch  : Var.var
    val cancel : Var.var

    val future1 : Var.var
    val touch1  : Var.var
    val cancel1 : Var.var

    val mkNewWorkQueue : unit -> AST.exp

    val mkFuture  : AST.exp * AST.exp -> AST.exp 
    val mkTouch   : AST.exp * AST.exp -> AST.exp
    val mkCancel  : AST.exp * AST.exp -> AST.exp

    val mkFuture1 : AST.exp * AST.exp -> AST.exp
    val mkTouch1  : AST.exp * AST.exp -> AST.exp
    val mkCancel1 : AST.exp * AST.exp -> AST.exp

  end *) =

  struct
    structure A = AST
    structure T = Types
    structure B = Basis
    
    (* fail : string -> 'a *)
    fun fail msg = raise Fail msg

    (* todo : string -> 'a *)
    fun todo thing = fail ("todo: " ^ thing)
			 
    (* futureTyc : T.tycon *)
    val futureTyc = TyCon.newAbsTyc (Atom.atom "future", 1, false)

    (* futureTy : T.ty -> T.ty *)
    fun futureTy t = T.ConTy ([t], futureTyc)
		    
    (* forall : (T.ty -> T.ty) -> T.ty_scheme *)
    fun forall mkTy =
	let val tv = TyVar.new (Atom.atom "'a")
	in
	    T.TyScheme ([tv], mkTy (A.VarTy tv))
	end

    (* monoVar : string * T.ty -> Var.var *)
    fun monoVar (name, ty) = Var.new (name, ty)

    (* polyVar : string * (T.ty -> T.ty) -> Var.var *)
    fun polyVar (name, mkTy) = Var.newPoly (name, forall mkTy)

    val --> = T.FunTy
    infixr 8 -->

    fun ** (t1, t2) = T.TupleTy [t1, t2]
    infixr 8 **

    (* predefined functions *)
    val newWorkQueue = monoVar ("newWorkQueue",
				B.unitTy --> B.workQueueTy)

    val future = polyVar ("future",
 		          fn tv => (Basis.unitTy --> tv) --> futureTy tv)

    val touch = polyVar ("touch",
		         fn tv => futureTy tv --> tv)

    val cancel = polyVar ("cancel",
			  fn tv => futureTy tv --> Basis.unitTy)

    val future1 = polyVar ("future1",
			   fn tv => (B.workQueueTy ** (B.unitTy --> tv)) --> futureTy tv)

    val touch1 = polyVar ("touch1",
			  fn tv => (B.workQueueTy ** (futureTy tv)) --> tv)

    val cancel1 = polyVar ("cancel1",
			   fn tv => (B.workQueueTy ** (futureTy tv)) --> Basis.unitTy)

    (* mkNewWorkQueue : unit -> A.exp *)
    (* Produces an AST expression which is a call to the newWorkQueue hlop. *)
    fun mkNewWorkQueue () = A.ApplyExp (A.VarExp (newWorkQueue, []),
					A.TupleExp [],
					Basis.workQueueTy)

    (* mkThunk : A.exp -> A.exp *)
    (* Consumes e; produces (fn u => e) (for fresh u : unit). *)
    fun mkThunk e = A.FunExp (Var.new ("u", Basis.unitTy), e, TypeOf.exp e)

    (* mkFut : var -> A.exp * A.exp -> A.exp *)
    (* Consumes futvar -> q and e; produces futvar (q, fn u => e). *)
    fun mkFut futvar (qVarExp, e) = 
	let val te = TypeOf.exp e
	in
	    A.ApplyExp (A.VarExp (futvar, []), (* [te]),  *)
			A.TupleExp [qVarExp, mkThunk e], 
			futureTy te)
	end

    (* mkFuture : A.exp * A.exp -> A.exp *)
    val mkFuture = mkFut future
 
    (* mkFuture1 : A.exp * A.exp -> A.exp *)
    val mkFuture1 = mkFut future1 

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

        (* mkTch : var -> A.exp * A.exp -> A.exp *)
	fun mkTch touchvar (qvar, e) =
	    if (isFuture e) then
		let val t = typeOfFuture e
		    val touch = A.VarExp (touchvar, [t])
		in
		    A.ApplyExp (touch, A.TupleExp [qvar, e], t)
		end
	    else
		let val ts = Var.toString touchvar
		in 
		    raise Fail (ts ^ ": argument is not a future")
		end

	(* mkCan : var -> A.exp * A.exp -> A.exp *)
	fun mkCan cancelvar (qvar, e) =
	    if (isFuture e) then
		let val cancel = A.VarExp (cancelvar, [typeOfFuture e])
		in
		    A.ApplyExp (cancel, A.TupleExp [qvar, e], Basis.unitTy)
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
    val mkTouch1 = mkTch touch1

    (* mkCancel : A.exp -> A.exp * A.exp *)
    (* Precondition: The argument e1 must be a future. *)
    (* The function raises Fail if the precondition is not met. *)
    val mkCancel = mkCan cancel

    (* mkCancel1 : A.exp -> A.exp * A.exp *)
    (* Precondition: The argument e1 must be a future. *)
    (* The function raises Fail if the precondition is not met. *)
    val mkCancel1 = mkCan cancel1

    end (* local *)

  end
