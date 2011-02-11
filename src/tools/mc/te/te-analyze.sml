(* te-analyze.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Type-and-effect analysis.
 *)

structure TEAnalyze = struct

  structure A = AST
  structure E = EAST
  structure F = Effects
  
  fun exp (e: A.exp) : E.exp = raise Fail "todo"

end
