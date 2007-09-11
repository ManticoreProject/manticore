(* flat-par-tup.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure FlatParTup : sig

    val flattenModule : AST.module -> AST.module
    val test : int -> unit

  end = 

  struct

    structure A = AST
    structure T = Types

    (* fail : string -> 'a *)
    fun fail msg = raise Fail msg

    (* todo : string -> 'a *)
    fun todo thing = fail ("todo: " ^ thing)

    (* id : 'a -> 'a *)
    val id = (fn x => x)

    (* (**) : ((a -> b) * (c -> d)) -> ((a * c) -> (b * d)) *)
    (* This is a combinator for  "pairing" functions together. *)
    infix 3 ** (* same precedence level as o *)
    fun f ** g = (fn (a, b) => (f a, g b))

    (* isFlattenCand : A.exp -> bool *)
    (* Determines whether the given expression is suitable for flattening. *)
    fun isFlattenCand e =
	  let (* tup : A.exp -> bool *)
	      fun tup (A.PTupleExp _) = true
		| tup (A.TupleExp _)  = true		
		| tup _ = false
	      (* dcon : A.exp -> bool *)
	      fun dcon (A.ApplyExp (A.ConstExp (A.DConst _), _, _)) = true
		| dcon _ = false
	      (* pred : A.exp -> bool *)
	      fun pred e = tup e orelse dcon e
	  in
	      case e
	        of (A.PTupleExp es) => List.exists pred es
		 | _ => false
	  end

    (* flattenExps : A.exp list -> A.exp list *)
    (* Removes all "leading" data constructors and inner parens from given exps. *)
    (* ex: ((1, 2), SOME 3)                ==> (1,2,3) *)
    (* ex: (SOME (1, 2), (SOME 3, SOME 4)) ==> (1,2,3,4) *)
    fun flattenExps es =
	let (* isDCon : A.exp -> bool *)
	    fun isDCon (A.ConstExp (A.DConst _)) = true
	      | isDCon _ = false
	    (* exp : A.exp -> A.exp list *)
	    fun exp (A.TupleExp es)  = exps es
	      | exp (A.PTupleExp es) = exps es
	      | exp (app as (A.ApplyExp (e1, e2, t))) = 
		  if (isDCon e1) then exp e2 else [app]
	      | exp e = [e]
	    (* exps : A.exp list -> A.exp list *)
	    and exps [] = []
	      | exps (e::es) = exp e @ exps es
	in
	    exps es
	end

    (* flattenTup : A.exp -> A.exp *)
    (* Pre: The argument is either a PTupleExp or a TupleExp. *)
    fun flattenTup (A.PTupleExp es) = A.PTupleExp (flattenExps es)
      | flattenTup (A.TupleExp es)  = A.TupleExp  (flattenExps es)
      | flattenTup _ = fail "flattenTup: expected a tuple"

    (* getNester : A.exp -> (A.var * A.lambda) option *)	
    fun getNester (A.LetExp (A.FunBind [nlam],
			     A.ApplyExp (nvar, e, t))) = SOME (nvar, nlam)
      | getNester _ = NONE

    (* firstValOf : ('a -> 'b option) * (unit -> 'b) -> 'a list -> 'b *)
    fun firstValOf (f, default) =
	let fun g [] = default ()
	      | g (x::xs) = (case f x
			       of SOME x' => x'
			        | NONE => g xs)
	in
	    g
	end

    (* valsOf : ('a -> 'b option) -> 'a list -> 'b list *)
    fun valsOf f =
	let (* v : 'a list -> 'b list *)
	    fun v [] = []
	      | v (x::xs) = (case f x
			      of SOME x' => x' :: v xs
			       | NONE => v xs)
	in
	    v
	end

    (* allSame : A.lambda list -> bool *)
    (* Consumes a list of nesters, returns true if they're all the same. *)
    fun allSame [] = true
      | allSame (n::[]) = true
      | allSame (n::(t as n'::ns)) = Nester.same (n, n') andalso allSame t

    (* omap : ('a -> 'b) -> 'a option -> 'b option *)
    val omap = Option.map

    (* exp : A.exp -> A.exp *)
    (* n.b. Type-preserving. *) 
   fun exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
      | exp (A.IfExp (e1, e2, e3, t)) = ifExp (e1, e2, e3, t)
      | exp (A.CaseExp (e, pes, t)) = caseExp (e, pes, t)
      | exp (A.FunExp (x, e, t)) = A.FunExp (x, exp e, t)
      | exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, t)
      | exp (A.TupleExp es) = A.TupleExp (List.map exp es)
      | exp (A.RangeExp (e1, e2, oe3, t)) = A.RangeExp (exp e1, 
							exp e2, 
							omap exp oe3, 
							t)
      | exp (p as A.PTupleExp es) =
	  if isFlattenCand p then	      
	      let val t = TypeOf.exp p
		  val (f, lam) = Nester.fromExp p 
	      in
		  A.LetExp (A.FunBind [lam],
			    A.ApplyExp (A.VarExp (f, []), 
					A.PTupleExp (flattenExps es), 
					t))
	      end
	  else 
	      A.PTupleExp (map exp es)
      | exp (A.PArrayExp (es, t)) = A.PArrayExp (List.map exp es, t)
      | exp (A.PCompExp (e, pes, oe)) = A.PCompExp (exp e, 
						    List.map (id ** exp) pes, 
						    omap exp oe)
      | exp (A.PChoiceExp (es, t)) = A.PChoiceExp (List.map exp es, t)
      | exp (A.SpawnExp e) = A.SpawnExp (exp e)
      | exp (k as (A.ConstExp _)) = k
      | exp (v as (A.VarExp _)) = v
      | exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
      | exp (v as (A.OverloadExp ovr)) = v

    (* ifExp : A.exp * A.exp * A.exp * T.ty -> A.exp *)
    and ifExp (e1, e2, e3, t) = 
  	(* Optimization: the nester is factored out of the if expression if poss. *)
	(* However, we need to know if both nesters (if there are two) are the same. *)
        (* While they won't differ in types, they might differ in data constructors. *)
	let (* f : A.exp -> A.exp *)
	    (* f either pulls the "applicand" out of an application, *)
	    (* or flattens the given tuple. If the argument is neither *)
	    (* an application nor a tuple, flattenTup will raise Fail. *)
	    fun f (A.LetExp (_, A.ApplyExp (_, e, _))) = e
	      | f e = flattenTup e
	    (* i : A.exp * A.exp -> A.exp *)
	    fun i (e2, e3) =
		case valsOf getNester [e2, e3]
		 of [] => fail "ifExp: no nesters found"
		  | nesters as (nVar, nLam) :: _ =>
		      if allSame (map #2 nesters) then			   
			  let val e2' = f e2
			      val e3' = f e3
			      val flatTy = TypeOf.exp e2' 
                                           (* e3' would do as well *)
			  in
			      A.LetExp (A.FunBind [nLam],
					A.ApplyExp (nVar,
						    A.IfExp (exp e1, 
							     e2', 
							     e3', 
							     flatTy),
						    t))
			  end
		      else (* we cannot factor *)
			  A.IfExp (exp e1, e2, e3, t)
	in
	    if isFlattenCand e2 orelse isFlattenCand e3 then
		i (exp e2, exp e3)
	    else
		A.IfExp (exp e1, exp e2, exp e3, t)
	end

    (* caseExp : A.exp * (A.pat * A.exp) list * T.ty -> A.exp *)
    and caseExp (e, pes, t) =
	(* Optimization: the nester is factored out of the case expression if poss. *)
	(* However, we need to know if all nesters (if there are > 1) are the same. *)
	(* While they won't differ in types, they might differ in data constructors. *)
	let (* f : A.exp -> A.exp *)
	    (* f either pulls the "applicand" out of an application, *)
	    (* or flattens the given tuple. If the argument is neither *)
	    (* an application nor a tuple, flattenTup will raise Fail. *)
	    fun f (A.LetExp (_, A.ApplyExp (_, e, _))) = e
	      | f e = flattenTup e
	    (* c : A.pat list * A.exp list -> A.exp *)
	    fun c (ps, es) = 
		(case valsOf getNester es
		   of [] => fail "caseExp: no nesters found"
		    | nesters as (nVar, nLam) :: _ =>
 		        if allSame (map #2 nesters) then
			    let val es' = map f es
				val flatTy = TypeOf.exp (hd es')
				(* any member of es' would do *)
				val pes' = ListPair.zip (ps, es')
			    in
				A.LetExp (A.FunBind [nLam],
					  A.ApplyExp (nVar,
						      A.CaseExp (exp e,
								 pes',
								 flatTy),
						      t))
			    end
			else (* we cannot factor *)
			    A.CaseExp (exp e, ListPair.zip (ps, es), t)
  		  (* end case *))
	    val (ps, es) = ListPair.unzip pes
	    val es' = map exp es
	in
            if List.exists isFlattenCand es then
		c (ps, es')
	    else
		A.CaseExp (exp e, ListPair.zip (ps, es'), t)
	end

    (* binding : A.binding -> A.binding *)
    and binding (A.ValBind (p, e)) = A.ValBind (p, exp e)
      | binding (A.PValBind (p, e)) = A.PValBind (p, exp e)
      | binding (A.FunBind ls) = A.FunBind (List.map lambda ls)

    (* lambda : A.lambda -> A.lambda *)
    and lambda (A.FB (v1, v2, e)) = A.FB (v1, v2, exp e)

    (* flatten : A.module -> A.module *)
    fun flattenModule m = exp m

    (**** tests ****)

    local

	structure P = PrintAST
	structure U = TestUtils

	val (tup, ptup, int, fact, some) = (U.tup, U.ptup, U.int, U.fact, U.some)
	val (intPat, varPat) = (U.intPat, U.varPat)

	val t0 = ptup [int 0, 
		       ptup [int 1, int 2],
		       int 3,
		       ptup [int 4,
			     int 5,
			     ptup [int 6, int 7]]]

	val t1 =  ptup [int 0, 
		        tup [int 1, int 2],
		        int 3,
		        tup [int 4,
			     int 5,
			     tup [int 6, int 7]]]

	(* t2 = (| SOME (fact 10), (fact 11, fact 12) |) *)
	val t2 = ptup [some (fact 10), tup [fact 11, fact 12]]

	(* t3 = (| (| SOME (SOME 1), SOME (2, 3) |), (SOME 4, SOME 5) |) *)
	val t3 = ptup [ptup [some (some (int 1)),
			     some (tup [int 2, int 3])],
		       tup [some (int 4),
			    some (int 5)]]

	(* t4 = (| SOME(1,2), (SOME 3, SOME 4) |) *)
	val t4 = ptup [some (tup [int 1, int 2]),
		       tup [some (int 3), some (int 4)]]


	(* t5 = (| 1, (2, 3) |) *)
	val t5 = ptup [int 1, tup (map int [2, 3])]

	(* t6 = if (isZero 0) then (| 1, (2, 3) |) else (| 4, (5, 6) |)  *)
	val t6 = U.ifexp (U.isZero 0,
			  ptup [int 1, tup [int 2, int 3]],
			  ptup [int 4, tup [int 5, int 6]])


	(* t7 = case n of 0 => (| 1, (2, SOME 3) |) 
                        | 1 => (| 4, (5, SOME 6) |) 
                        | k => (| 0, (0, NONE) |) *)
	(* Note: the nester cannot be factored out of this case. *)
	val t7 = 
	    let val k = Var.new ("k", Basis.intTy)
	    in
		U.caseExp (A.VarExp (Var.new ("n", Basis.intTy), []),
			   [(intPat 0, ptup [int 1, tup [int 2, some (int 3)]]),
			    (intPat 1, ptup [int 4, tup [int 5, some (int 6)]]),
			    (varPat k, ptup [int 0, tup [int 0, U.none Basis.intTy]])])
	    end

	(* t8 = if (isZero 0) then (| SOME 1, (2, 3) |) else (| NONE, (5, 6) |)  *)
	(* Note: the nester cannot be factored out of this if. *)
	val t8 = U.ifexp (U.isZero 0,
			  ptup [some (int 1), tup [int 2, int 3]],
			  ptup [U.none Basis.intTy, tup [int 5, int 6]])

	(* t9 = case n of 0 => (| 1, (2, SOME 3) |) 
                        | 1 => (| 4, (5, SOME 6) |) 
                        | k => (| 0, (0, SOME 0) |) *)
	(* Note: the nester can be factored out of this case. *)
	val t9 = 
	    let val k = Var.new ("k", Basis.intTy)
	    in
		U.caseExp (A.VarExp (Var.new ("n", Basis.intTy), []),
			   [(intPat 0, ptup [int 1, tup [int 2, some (int 3)]]),
			    (intPat 1, ptup [int 4, tup [int 5, some (int 6)]]),
			    (varPat k, ptup [int 0, tup [int 0, some (int 0)]])])
	    end

	fun testExp e = (P.print e;
			 P.printComment "-->";
			 P.print (flattenModule e))

    in

        (* test : int -> unit *)
        val test = 
	    let val testCases = [t0,t1,t2,t3,t4,t5,t6,t7,t8,t9]
	    in
		U.mkTest testExp testCases
	    end

    end (* local *)
		    
  end
