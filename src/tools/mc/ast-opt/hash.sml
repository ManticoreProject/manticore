(* hash.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * n.b. This is hash in the sense of ML's #, not a hash function, table, etc.
 *
 * Given an integer [1,n] and a list of types of length at least 1 and <= n,
 * return a tuple selector function (a la ML's #) of appropriate type.
 *
 * Ex: mkHash (2, [int, bool, char]) --> fn (i:int, b:bool, c:char) => b
 *)

structure Hash (* : sig

    val mkHash : int * Types.ty list -> AST.exp

  end *) = 

  struct

    structure A = AST
    structure T = Types
    structure F = Futures

    (* fail : string -> 'a *)
    fun fail msg = raise Fail msg

    (* todo : string -> 'a *)
    fun todo thing = fail ("todo: " ^ thing)

    (* patAndVar : T.ty list * int -> A.pat * Var.var *)
    (* Given a list of types and an index (1-based) into that list, *)
    (* return a tuple pattern, appropriately typed, and the var to select *)
    (* out of that pattern. *)
    fun patAndVar (ts, i) =
	let (* build : T.ty list * int * A.pat list * Var.var option
                       -> A.pat * Var.var *)
	    fun build ([], _, acc, ov) = 
		  let val x = case ov
			        of SOME x => x
				 | NONE => fail ("patAndVar: n is outside the interval\
                                                \ [1," ^ Int.toString (length ts) ^ "]")
		  in
		      (A.TuplePat (rev acc), x)
		  end
	      | build (t::ts, n, acc, ov) =
		  let val xn = Var.new ("x" ^ Int.toString n, t)
		      val ov' = if (n=i) then SOME xn else ov
		  in
		      build (ts, n+1, A.VarPat xn :: acc, ov')
		  end
	in
	    build (ts, 1, [], NONE)
	end

    (* containsTyvar : T.ty -> bool *)
    fun containsTyvar t =
	let val any = List.exists
	    (* ty : T.ty -> bool *)
	    fun ty T.ErrorTy = false
	      | ty (T.MetaTy m) = meta m
	      | ty (T.VarTy _) = true
	      | ty (T.ConTy (ts, _)) = any ty ts
	      | ty (T.FunTy (t1, t2)) = ty t1 orelse ty t2
	      | ty (T.TupleTy ts) = any ty ts
	    (* meta : T.meta -> bool *)
	    and meta (T.MVar {info, ...}) =
		(case !info
		   of T.INSTANCE t => ty t
		    | T.CLASS _ => false
		    | T.UNIV _ => false)
	in
	    ty t
	end

    (* mkHash : int * T.ty list -> A.exp *)
    (* n.b. This is hash in the sense of ML's #, not a hash function, table, etc. *)
    (* Pre: n is on [1, length ts]. *)
    (* Pre: There are no free type variables in the list of types. *)
    (* Given a list of types (assumed to be members of a tuple), return #n. *) 
    (* n.b. n is 1-based, in keeping with #. *)
    fun mkHash (n, ts) =
	let val resultTy = List.nth (ts, n-1)
	    val (tuplePat, xn) = patAndVar (ts, n)
	    val arg = Var.new ("t", T.TupleTy ts)
	    val loneBranch = A.PatMatch (tuplePat, A.VarExp (xn, []))
	    val caise = A.CaseExp (A.VarExp (arg, []), [loneBranch], resultTy)
	    val hashFn = A.FunExp (arg, caise, resultTy)
	in
	    if List.exists containsTyvar ts then
		fail "mkHash: passed unresolved polymorphic type"
	    else
		hashFn
	end

    (**** tests ****)

    local

	structure U = TestUtils

	(* tst : int * T.ty list -> unit *)
	fun tst (n, ts) = 
	    let val h = mkHash (n, ts)
	    in
		PrintAST.print h;
		PrintAST.printComment (TypeUtil.toString (TypeOf.exp h))
	    end

        (* t0 *)
	val t0 = (1, [Basis.intTy, Basis.boolTy]) 

        (* t1 *)
	val t1 = (2, [Basis.intTy, Basis.boolTy])

        (* t2 *)
	val t2 = (3, [Basis.intTy, Basis.boolTy, F.futureTy Basis.floatTy])

    in
        (* test : int -> unit *)
        val test = U.mkTest tst [t0,t1,t2]

    end

  end (* structure Hash *)
