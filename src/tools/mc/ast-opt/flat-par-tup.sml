(* flat-par-tup.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure FlatParTup (* : sig

    val flatten : AST.module -> AST.module

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
	
    (**** main traversal of the AST ****)
	
    (* exp : A.exp -> A.exp *)
    fun exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
      | exp (A.IfExp (e1, e2, e3, t)) = A.IfExp (exp e1, exp e2, exp e3, t)
      | exp (A.CaseExp (e, pes, t)) = A.CaseExp (exp e, List.map (id ** exp) pes, t)
      | exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, t)
      | exp (A.TupleExp es) = A.TupleExp (List.map exp es)
      | exp (A.RangeExp (e1, e2, oe3, t)) = A.RangeExp (exp e1, exp e2, Option.map exp oe3, t)
      | exp (p as A.PTupleExp es) =
	  if isFlattenCand p then	      
	      let val t = TypeOf.exp p
		  val (f, lam) = Nester.fromExp p 
	      in
		  A.LetExp (A.FunBind [lam],
			    A.ApplyExp (A.VarExp (f, []), 
					Nester.flatten p, 
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
    fun flatten m = exp m

    (**** tests ****)

    local
	structure P = PrintAST
	fun tup es  = A.TupleExp es
	fun ptup es = A.PTupleExp es
	fun int n = A.ConstExp (A.LConst (Literal.Int n, Basis.intTy))
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
	fun test e = (P.print e;
		      P.printComment "-->";
		      P.print (flatten e))
    in
        fun test0 () = test t0
        fun test1 () = test t1
    end

  end
