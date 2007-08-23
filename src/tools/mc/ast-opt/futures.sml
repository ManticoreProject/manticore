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

    val mkFuture  : AST.exp -> AST.exp 
    val mkTouch   : AST.exp -> AST.exp
    val mkCancel  : AST.exp -> AST.exp

    val mkFuture1 : AST.exp -> AST.exp
    val mkTouch1  : AST.exp -> AST.exp
    val mkCancel1 : AST.exp -> AST.exp

  end *) =

  struct
  
    structure A = AST
    structure T = Types
    
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

    (* polyVar : Atom.atom * (T.ty -> T.ty) -> Var.var *)
    fun polyVar (name, mkTy) = Var.newPoly (Atom.toString name, forall mkTy)

    val --> = T.FunTy
    infixr 8 -->

    (* predefined functions *)
    val future = polyVar (Atom.atom "future",
 		          fn tv => (Basis.unitTy --> tv) --> futureTy tv)

    val touch = polyVar (Atom.atom "touch",
		         fn tv => futureTy tv --> tv)

    val cancel = polyVar (Atom.atom "cancel",
			  fn tv => futureTy tv --> Basis.unitTy)

    val future1 = polyVar (Atom.atom "future1",
			   fn tv => (Basis.unitTy --> tv) --> futureTy tv)

    val touch1 = polyVar (Atom.atom "touch1",
			  fn tv => futureTy tv --> tv)

    val cancel1 = polyVar (Atom.atom "cancel1",
			   fn tv => futureTy tv --> Basis.unitTy)

    (* mkFut : var -> A.exp -> A.exp *)
    (* Note: The given expression will be thunkified. *)
    fun mkFut futvar e = 
	let val t = TypeOf.exp e
	    val unitTy = Basis.unitTy
	    val thunkVar = Var.new ("thunk", T.FunTy (unitTy, t))
	    val thunk = A.FB (thunkVar, Var.new ("u", unitTy), e)
	in
	    A.LetExp (A.FunBind [thunk],
		      A.ApplyExp (A.VarExp (futvar, [t]),
				  A.VarExp (thunkVar, []),
				  futureTy t))		      
	end

    (* mkFuture : A.exp -> A.exp *)
    val mkFuture = mkFut future
 
    (* mkFuture1 : A.exp -> A.exp *)
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

        (* mkTch : var -> A.exp -> A.exp *)
	fun mkTch touchvar e =
	    if (isFuture e) then
		let val t = typeOfFuture e
		    val touch = A.VarExp (touchvar, [t])
		in
		    A.ApplyExp (touch, e, t)
		end
	    else
		raise Fail "touch: argument is not a future"

	(* mkCan : var -> A.exp -> A.exp *)
	fun mkCan cancelvar e =
	    if (isFuture e) then
		let val cancel = A.VarExp (cancelvar, [typeOfFuture e])
		in
		    A.ApplyExp (cancel, e, Basis.unitTy)
		end
	    else
		raise Fail "cancel: argument is not a future"
    in

    (* mkTouch : A.exp -> A.exp *)
    (* Precondition: The argument must be a future. *)
    (* The function raises Fail if the precondition is not met. *)
    val mkTouch = mkTch touch 

    (* mkTouch1 : A.exp -> A.exp *)
    (* Precondition: The argument must be a future. *)
    (* The function raises Fail if the precondition is not met. *)
    val mkTouch1 = mkTch touch1

    (* mkCancel : A.exp -> A.exp *)
    (* Precondition: The argument e1 must be a future. *)
    (* The function raises Fail if the precondition is not met. *)
    val mkCancel = mkCan cancel

    (* mkCancel1 : A.exp -> A.exp *)
    (* Precondition: The argument e1 must be a future. *)
    (* The function raises Fail if the precondition is not met. *)
    val mkCancel1 = mkCan cancel1

    end (* local *)

  end
