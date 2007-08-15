(* flat-par-tup.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure FlatParTup (* : sig

    val flattenModule : AST.module -> AST.module

  end *) = 

  struct

    structure A = AST
    structure T = Types

    (* id : a -> a *)
    val id = (fn x => x)

    (* (**) : ((a -> b) * (c -> d)) -> ((a * c) -> (b * d)) *)
    (* This is a combinator for  "pairing" functions together. *)
    infix 3 ** (* same precedence level as o *)
    fun f ** g = (fn (a, b) => (f a, g b))

    (* flattenCand : A.exp -> bool *)
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
    (* Removes all "leading" data constructors and inner parens *)
    (*   from given list of expressions. *)
    (* ex: ((1,2),SOME 3) ==> (1,2,3) *)
    (* ex: (SOME(1,2), (SOME 3, SOME 4)) ==> (1,2,3,4) *)
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

    (**** main traversal of the AST ****)
	
    (* exp : A.exp -> A.exp *)
    fun exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
      | exp (A.IfExp (e1, e2, e3, t)) =
          (* TODO: IfExp -- *)
          (* possible optimization...the nester can be "factored out" of the branches *)
          (* of an if expression *)
	  A.IfExp (exp e1, exp e2, exp e3, t) 
      | exp (A.CaseExp (e, pes, t)) = A.CaseExp (exp e, List.map (id ** exp) pes, t)
      | exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, t)
      | exp (A.TupleExp es) = A.TupleExp (List.map exp es)
      | exp (A.RangeExp (e1, e2, oe3, t)) = 
	  A.RangeExp (exp e1, exp e2, Option.map exp oe3, t)
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
      | exp (A.PCompExp (e, pes, eo)) = A.PCompExp (exp e, 
						    List.map (id ** exp) pes, 
						    Option.map exp eo)
      | exp (A.PChoiceExp (es, t)) = A.PChoiceExp (List.map exp es, t)
      | exp (A.SpawnExp e) = A.SpawnExp (exp e)
      | exp (k as (A.ConstExp _)) = k
      | exp (v as (A.VarExp _)) = v
      | exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
      | exp (v as (A.OverloadExp ovr)) = v

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


	(* t5 = if (isZero 0) then (| 1, (2, 3) |) else (| 4, (5, 6) |)  *)
	val t5 = U.ifexp (U.isZero 0,
			  ptup [int 1, tup [int 2, int 3]],
			  ptup [int 4, tup [int 5, int 6]])

	fun testExp e = (P.print e;
			 P.printComment "-->";
			 P.print (flattenModule e))

    in

        (* test : int -> unit *)
        fun test n =
	    let val tests = [t0,t1,t2,t3,t4,t5]
	    in
		testExp (List.nth (tests, n))
		handle Subscript =>
		       let val msg = "FlatParTup.test: Please choose a test value \
                                     \between 0 and "
				     ^ Int.toString ((List.length tests) - 1)
				     ^ ".\n"
		       in
			   print msg
		       end
	    end

    end (* local *)
		    
  end
