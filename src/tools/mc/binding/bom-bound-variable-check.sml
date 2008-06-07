(* bom-bound-variable-check.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Check BOM code for unbound variables.
 *)

structure BOMBoundVariableCheck :> sig

  (* check for unbound variables *)
    val check : (Error.err_stream * ProgramParseTree.BOM1.code) -> ProgramParseTree.BOM2.code

  end = struct

    structure BOM1 = ProgramParseTree.BOM1
    structure BOM2 = ProgramParseTree.BOM2

    fun chkList loc (chkX, xs, env) = let
	   fun f (x, (xs, env)) = let
	          val (x, env) = chkX loc (x, env)
                  in
	            (x :: xs, env)
	          end
	   val (xs', env) = List.foldl f ([], env) xs
           in
	      (List.rev xs', env)
           end

    fun check (loc, defs) = raise Fail "todo"

  end (* BOMBoundVariableCheck *)