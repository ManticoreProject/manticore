(* fut-par-tup.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * 
 * This module rewrites parallel tuples in terms of futures and touches.
 *)

structure FutParTup (* : sig

    val futurize : A.module -> A.module

  end *) = 

  struct

    structure A = AST
    structure T = Types

    val futureTycon : T.tycon =
	let val a = T.TVar {stamp = Stamp.new (),
			    name = Atom.atom "a",
			    class = NONE}
	    val fcons = ref (nil : T.dcon list)
	    val fty = T.DataTyc {stamp = Stamp.new (),
				 name = Atom.atom "future",
				 params = [a],
				 cons = fcons}
	    val fdcon = T.DCon {stamp = Stamp.new (),
				name = Atom.atom "Future",
				owner = fty,
				argTy = SOME (T.VarTy a)}
	in
	    fty before fcons := [fdcon]
	end

    (* futureTy : T.ty -> T.ty *)
    fun futureTy t = T.ConTy ([t], futureTycon)

    (* future : A.exp -> A.exp *)
    (* to turn an expression e into "future e" *)
    fun future e =
	let val te = TypeOf.exp e
	    val tf = T.FunTy (te, futureTy te)	     
	    val fvar = VarRep.V {name = "future",
				 id = Stamp.new (),
				 kind = ref A.VK_Fun,
				 useCnt = ref 0,
				 ty = ref (T.TyScheme ([], tf)),
				 props = PropList.newHolder ()}
	    val f = A.VarExp (fvar, []) 
	in
	    A.ApplyExp (f, e, futureTy te)
	end

    (* stampFromTycon : T.tycon -> Stamp.stamp *)
    fun stampFromTycon (T.AbsTyc data) = #stamp data
      | stampFromTycon (T.DataTyc data) = #stamp data 
	
    (* isFutureTy : T.ty -> bool *)
    fun isFutureTy (T.ConTy (t, c)) =
	  let val futureStamp = stampFromTycon futureTycon
	      val cStamp = stampFromTycon c
	  in
	      Stamp.same (futureStamp, cStamp)
	  end
      | isFutureTy _ = false

    (* typeFromFutureTy : T.ty -> T.ty *)
    fun typeFromFutureTy ty =
	  (case ty
             of T.ConTy (t::[], _) => if isFutureTy ty 
				      then t 
				      else raise Fail "not a future ty"
	      | T.ConTy (_, _) => raise Fail "expected arity 1 for future ty"
	      | _ => raise Fail "not even a ConTy, let alone a future ty")

    (* touch : A.exp -> A.exp *)
    (* pre: the argument is a future *)
    fun touch e = 
  	  let val te = TypeOf.exp e
	      val innerTy = typeFromFutureTy te 
	                    (* will throw exception if e is not a future *)
	      val tt = T.FunTy (te, innerTy)
	      val tvar = VarRep.V {name = "touch",
				   id = Stamp.new (),
				   kind = ref A.VK_Fun,
				   useCnt = ref 0,
				   ty = ref (T.TyScheme ([], tt)),
				   props = PropList.newHolder ()}
	      val t = A.VarExp (tvar, [])
	  in
	      A.ApplyExp (t, e, innerTy)
	  end

    infixr 5 :>: (* same precedence as :: *)
    (* (:>:) : ('a * 'b) * ('a list * 'b list) -> 'a list * 'b list *)
    fun (x,y) :>: (xs,ys) = (x::xs, y::ys)

    (* exp : A.exp -> A.exp *)
    fun exp (A.PTupleExp es) = 
  	  let (* mkFutBinds : A.exp list -> A.binding list * A.var list *)
	      fun mkFutBinds ([], n) = ([],[])
		| mkFutBinds (e::es, n) =
		    let val te = TypeOf.exp e
			val f_n =  
			      VarRep.V {name = "f" ^ Int.toString n,
					id = Stamp.new (),
					kind = ref A.VK_Pat,
					useCnt = ref 0,
					ty = ref (T.TyScheme ([], te)),
					props = PropList.newHolder ()}
			val p = A.VarPat f_n
			val b = A.ValBind (p, future e)
		    in
			(b, f_n) :>: mkFutBinds (es, n+1)
		    end
	      (* letMany : A.binding list * A.exp -> A.exp *)
	      (* pre: there is at least one binding *)
	      fun letMany (b::[], e) = A.LetExp (b, e)
		| letMany (b::bs, e) = A.LetExp (b, letMany (bs, e))
		| letMany ([], _) = raise Fail "argument must have at least one binding"
	      val (bs, vs) = mkFutBinds (es, 0)
	      val touches = map (fn v => touch (A.VarExp (v, []))) vs
	  in
	      letMany (bs, A.TupleExp touches)
	  end
      | exp _ = raise Fail "todo"

    (* module : A.module -> A.module *)
    fun module m = exp m

    (* futurize : A,module -> A.module *)
    fun futurize m = module m

  end
