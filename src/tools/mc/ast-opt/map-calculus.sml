(* map-calculus.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Utilities to generate polymorphic parallel map functions of various arities.
 *)

structure MapCalculus : sig

    val mkMapP : int -> AST.var
val test : unit -> unit
  end = struct

    structure A = AST
    structure B = Basis

    val polyVarMulti = BasisUtils.polyVarMulti

    val qTy = UnseenBasis.workQueueTy

    (* split : 'a list -> ('a list) * 'a *)
    (* Splits a list into all but its last element and its last element. *)
    (* Pre: argument is nonempty. *)
    (* ex: split [1,2,3] => ([1,2], 3) *)
    (* ex: split [1] => ([], 1) *)
    fun split xs =
	let fun s ([], bf) = raise Fail "BUG"
	      | s ([x], bf) = (rev bf, x)
	      | s (x::xs, bf) = s (xs, x::bf)
	in
	    case xs
	      of [] => raise Fail "split given empty list"
	       | _ => s (xs, [])
	end

    (* mkMapP : int -> A.var *)
    (* The arity should be the number of arguments to the function to be mapped. *)
    (* e.g., mkMapP 2 will generate a function of type *)
    (*  ('a * 'b -> 'c) * ('a parray) * ('b parray) -> ('c parray) *)
    fun mkMapP arity =
	let val arityString = Int.toString arity
	    fun mkTy tys =
		  if (length tys) <> (arity+1) then
		      raise Fail ("BUG: bad instantiation for mapP of arity " 
				  ^ arityString)
		  else
		      let val (butfirstTys, lastTy) = split tys
			  val fnTy = A.FunTy (A.TupleTy butfirstTys, lastTy)
			  val argTy = A.TupleTy (fnTy :: (map B.parrayTy butfirstTys))
		      in
			  A.FunTy (qTy, A.FunTy (argTy, B.parrayTy lastTy))
		      end
	in
	    polyVarMulti (concat ["map", arityString, "PQ"], arity+1, mkTy)
	end

    fun test () =
	let val m3 = mkMapP 3
	in
	    print (Var.toString m3);
	    print "\n";
	    print (TypeUtil.schemeToString (Var.typeOf m3));
	    print "\n"
	end

  end
