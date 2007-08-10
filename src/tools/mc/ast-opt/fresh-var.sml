(* fresh-var.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure FreshVar (* : sig

    val resetVarNames : unit -> unit
    val nextVarName   : unit -> string
    val fresh         : string option * Types.ty -> AST.var

  end *) = 

  struct
  
    local
	(* letterSeed : int ref *)
	val letterSeed = ref 0		 
	(* letters : string vector *)
	val letters = 
	    let val alphabet = "abcdefghijklmnopqrstuvwxyz"
	    in
		Vector.fromList (List.map Char.toString (explode alphabet))
	    end
    in
    
    (* resetVarNames : unit -> unit *)
    fun resetVarNames () = (letterSeed := 0)
    
    (* nextVarName : unit -> string *)
    fun nextVarName () =
	let val s = !letterSeed
	    val a = Vector.sub (letters, s mod 26)
	    val n = s div 26
	    val x = if (n > 0) 
		    then a ^ (Int.toString n)
		    else a
	(* This function should provide something like 26 * MAX_INT
         * fresh variable names, but might run into trouble 
         * thereafter. TODO: provide proof that won't happen at call sites.
         *)
	in
	    letterSeed := s + 1;
	    x
	end

    (* fresh: (string option * T.ty) -> A.var *)
    fun fresh (os, t) = 
	let val x = case os of SOME s => s 
			     | NONE => nextVarName ()
	in
	    Var.new (x, t)
	end
    
    end (* local *)

  end
