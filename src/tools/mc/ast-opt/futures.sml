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
    val future    : AST.exp -> AST.exp 
    val touch     : AST.exp -> AST.exp
    val cancel    : AST.exp * AST.exp -> AST.exp

  end *) =

  struct
  
    structure A = AST
    structure T = Types
    
    val futureTyc = TyCon.newAbsTyc (Atom.atom "future", 1, false)

    (* futureTy : T.ty -> T.ty *)
    fun futureTy t = T.ConTy ([t], futureTyc)
		    
    (* future : A.exp -> A.exp *)
    fun future e = 
	let val t = TypeOf.exp e
	    val futureVar = Var.new ("future", T.FunTy (t, futureTy t))
	    val unit = A.TupleExp []
	    val unitTy = Basis.unitTy
	    val thunkVar = Var.new ("thunk", T.FunTy (unitTy, t))
	    val thunk = A.FB (thunkVar, Var.new ("u", unitTy), e)
	in
	    A.LetExp (A.FunBind [thunk],
		      A.ApplyExp (A.VarExp (futureVar, [t]),
				  A.VarExp (thunkVar, []),
				  futureTy t))		      
	end

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

    in

    (* touch : A.exp -> A.exp *)
    (* Precondition: The argument must be a future. *)
    (* The function raises Fail if the precondition is not met. *)
    fun touch e = 
	let val t = typeOfFuture e
	    val touchVar = Var.new ("touch", T.FunTy (TypeOf.exp e, t))
	    val touch = A.VarExp (touchVar, [t])
	in
	    A.ApplyExp (touch, e, t)
	end

    (* cancel : A.exp * A.exp -> A.exp *)
    (* Precondition: The argument e1 must be a future. *)
    (* The function raises Fail if the precondition is not met. *)
    fun cancel (e1, e2) =
	let val u = Basis.unitTy
	in
	    if (isFuture e1) then
		let val cancelVar = Var.new ("cancel", 
					     T.FunTy (TypeOf.exp e1, u))
		    val cancel = A.VarExp (cancelVar, 
					   [typeOfFuture e1])
		in
		    A.SeqExp (A.ApplyExp (cancel, e1, u), e2)
		end
	    else
		raise Fail "cancel: first argument is not a future"
	end

    end (* local *)
  
  end
