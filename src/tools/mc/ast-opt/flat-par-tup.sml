(* flat-par-tup.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Adam Shaw
 *)

(* Q1 What is the ty list for in a VarExp? *)

structure FlatParTup (* : sig

    val fpt : AST.module -> AST.module

  end *) = 

  struct

    structure A = AST
    structure T = Types

    (* (**) : ((a -> b) * (c -> d)) -> ((a * c) -> (b * d)) *)
    infixr **
    fun f ** g = (fn (a, b) => (f a, g b))

    datatype shape = ShapeGroup of shapes | Dot
    and shapes = ShapeSeq of shape * shapes | ShapeSing of shape

    (* shapeOf : A.exp -> shape *)
    fun shapeOf (A.TupleExp es) = ShapeGroup (shapesOf es)
      | shapeOf (A.PTupleExp es) = ShapeGroup (shapesOf es)
      | shapeOf _ = Dot

    (* shapesOf : A.exp list -> shapes *)
    and shapesOf ([]) = raise Fail "empty tuple"
      | shapesOf ([e]) = ShapeSing (shapeOf e)
      | shapesOf (e::es) = ShapeSeq (shapeOf e, shapesOf es)

    (* removeParens : A.exp list -> A.exp list *)
    (* pre: argument must not be empty *)
    fun removeParens es =
	let (* exp : exp -> exp list *)
	    fun exp (A.TupleExp es) = exps es
	      | exp (A.PTupleExp es) = exps es
	      | exp e = [e]
	    (* exps : exp list -> exp list *)
	    and exps ([e]) = exp e
	      | exps (e::es) = exp e @ exps es
	      | exps [] = raise Fail "empty"
	in
	    exps es
	end

    (* flatten : A.exp -> A.exp *)
    fun flatten (A.TupleExp es) = A.TupleExp (removeParens es)
      | flatten (A.PTupleExp es) = A.PTupleExp (removeParens es)
      | flatten e = e

    (* letterSeed : int ref *)
    val letterSeed = ref 0

    (* letters : string vector *)
    val letters = 
	let val alphabet = "abcdefghijklmnopqrstuvwxyz"
	in
	    Vector.fromList (List.map Char.toString (explode alphabet))
	end

    (* tupleType : A.exp list -> A.ty *)
    fun tupleType es =
	let (* ty : A.exp -> A.ty *)
	    fun ty (A.LetExp (_, e)) = ty e
	      | ty (A.IfExp (_, _, _, t)) = t
	      | ty (A.CaseExp (_, _, t)) = t
	      | ty (A.ApplyExp (_, _, t)) = t
	      | ty (A.TupleExp es) = tupleType es
	      | ty (A.RangeExp (_, _, _, t)) = t
	      | ty (A.PTupleExp es) = tupleType es
	      | ty (A.PArrayExp (_, t)) = t
	      | ty (A.ComprehendExp (e, _, _)) = raise Fail "todo"
	      | ty (A.PChoiceExp (_, t)) = t
	      | ty (A.SpawnExp e) = ty e
	      | ty (A.ConstExp k) = 
		  (case k
		    of A.DConst (d, ts) => raise Fail "todo"
		     | A.LConst (_, t) => t)
	      | ty (A.VarExp (_, ts)) = raise Fail "todo"
	      | ty (A.SeqExp (_, e)) = ty e
	      | ty (A.OverloadExp ovr) = raise Fail "todo"
	in
	    T.TupleTy (List.map ty es)
	end

    (* makeNester : shape -> A.lambda * A.var *)
    (* returns a function along with the variable to which it is bound *)
    fun makeNester s =
	let fun gensym () =
		let val s = Stamp.new ()
		    val n = !letterSeed before (letterSeed := (!letterSeed + 1) mod 26)
		    val a = Vector.sub (letters, n)
		    val t = T.VarTy (T.TVar {stamp=s, 
					     name=Atom.atom a,  
					     class=NONE})
		in
		    A.VarExp (* Q1 *) (Var.new (a, t), [])
		end
	    (* mkNestedTup : shape -> A.exp *)
	    fun mkNestedTup s =
		let (* shape : shape -> A.exp *)
		    fun shape (ShapeGroup sbar) = A.TupleExp (shapes sbar)
		      | shape Dot = gensym ()
		    (* shapes : shapes -> A.exp list *)
		    and shapes (ShapeSeq (s, sbar)) = (shape s) :: (shapes sbar)
		      | shapes (ShapeSing s) = [shape s]
		in
		    shape s
		end
	    val nestedVarTup = mkNestedTup s
	    val flatVarTup = flatten nestedVarTup
	    val flatVarPat = 
		let fun pat (A.VarExp (v, _)) = A.VarPat v
		      | pat _ = raise Fail "expected a VarExp"
		in
		    case flatVarTup
		     of A.TupleExp es => A.TuplePat (List.map pat es)
		      | _ => raise Fail "expected a flat tuple of variables"
		end
	    fun varToTyvar (VarRep.V {name, id, ...}) =
		  Types.TVar {stamp=id, name=Atom.atom name, class=NONE}
	    fun extractTyvar (A.VarExp (v, _)) = varToTyvar v
	      | extractTyvar _ = raise Fail "not a VarExp"
	    val dty = (case flatVarTup
			of A.TupleExp es => tupleType es
			 | _ => raise Fail "expected TupleExp")
	    val rty = (case nestedVarTup	    
			of A.TupleExp es => tupleType es
			 | _ => raise Fail "expected TupleExp")
	    val tvs = (case flatVarTup
			of A.TupleExp es => List.map extractTyvar es
			 | _ => raise Fail "expected TupleExp")
	    val funScheme = T.TyScheme (tvs, T.FunTy (dty, rty))
	    val funVar = VarRep.V {name = "f",
				   id = Stamp.new (),
				   kind = ref A.VK_Fun,
				   useCnt = ref 0, (* ??? *)
				   ty = ref funScheme,
				   props = PropList.newHolder ()}
	    val argVar = VarRep.V {name = "t",
				   id = Stamp.new (),
				   kind = ref A.VK_Pat,
				   useCnt = ref 0,
				   ty = ref (T.TyScheme (tvs, dty)),
				   props = PropList.newHolder ()}
	    val body = A.CaseExp (A.VarExp (argVar, []) (* Q1 *),
				  [(flatVarPat, nestedVarTup)],
				  rty)
	in
	    (A.FB (funVar, argVar, body), funVar) 
	end

    (* exp : A.exp -> A.exp *)
    fun exp (A.LetExp (b, e)) = A.LetExp (binding b, exp e)
      | exp (A.IfExp (e1, e2, e3, t)) = A.IfExp (exp e1, exp e2, exp e3, t)
      | exp (A.CaseExp (e, pes, t)) = A.CaseExp (exp e, List.map (pat ** exp) pes, t)
      | exp (A.ApplyExp (e1, e2, t)) = A.ApplyExp (exp e1, exp e2, t)
      | exp (A.TupleExp es) = A.TupleExp (List.map exp es)
      | exp (A.RangeExp (e1, e2, oe3, t)) = A.RangeExp (exp e1, exp e2, Option.map exp oe3, t)
      | exp (p as A.PTupleExp es) = 
	  let val (fdef, fname) = makeNester (shapeOf p)
	      val ftup = flatten p
	      val ty = tupleType es
	  in
	      A.LetExp (A.FunBind [fdef], 
			A.ApplyExp (A.VarExp (fname, []) (* Q1 *), ftup, ty))
	  end
      | exp (A.PArrayExp (es, t)) = A.PArrayExp (List.map exp es, t)
      | exp (A.ComprehendExp (e, pes, eo)) = A.ComprehendExp (exp e, 
							      List.map (pat ** exp) pes, 
							      Option.map exp eo)
      | exp (A.PChoiceExp (es, t)) = A.PChoiceExp (List.map exp es, t)
      | exp (A.SpawnExp e) = A.SpawnExp (exp e)
      | exp (A.ConstExp k) = A.ConstExp (const k)
      | exp (A.VarExp (v, ts)) = A.VarExp (var v, ts)
      | exp (A.SeqExp (e1, e2)) = A.SeqExp (exp e1, exp e2)
      | exp (A.OverloadExp ovr) = raise Fail "todo"

    (* binding : A.binding -> A.binding *)
    and binding (A.ValBind (p, e)) = A.ValBind (pat p, exp e)
      | binding (A.PValBind (p, e)) = A.PValBind (pat p, exp e)
      | binding (A.FunBind ls) = A.FunBind (List.map lambda ls)

    (* lambda : A.lambda -> A.lambda *)
    and lambda (A.FB (v1, v2, e)) = A.FB (var v1, var v2, exp e)

    (* pat : A.pat -> A.pat *)
    and pat (A.ConPat (d, ts, p)) = A.ConPat (d, ts, pat p)
      | pat (A.TuplePat ps) = A.TuplePat (List.map pat ps)
      | pat (A.VarPat v) = A.VarPat (var v)
      | pat (A.ConstPat k) = A.ConstPat (const k)

    (* const: A.const -> A.const *)
    and const (A.DConst (d, ts)) = A.DConst (d, ts)
      | const (A.LConst (l, t)) = A.LConst (l, t)

    (* overload_var : A.overload_var -> A.overload_var *)
    and overload_var (A.Unknown (t, vs)) = A.Unknown (t, List.map var vs)
      | overload_var (A.Instance v) = A.Instance (var v)

    (* var_kind : A.var_kind -> A.var_kind *)
    and var_kind k = k

    (* var : A.var -> A.var *)
    and var v = v

    (* fpt : A.module -> A.module *)
    fun fpt m = exp m

  end
