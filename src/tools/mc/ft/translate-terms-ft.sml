(* flatten-terms.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Translate AST terms to FLAST terms. 
 * Flattening comes later.
 * docs in /path/to/manti-papers/papers/notes/amsft/
 *)

structure TranslateTermsFT = struct

  structure A = AST
  structure F = FLAST
  structure T = FTTypes

  val tty = TranslateTypesFT.translate

  fun exp (A.ConstExp k) = F.ConstExp (const k)
    | exp _ = raise Fail "todo"

  and const (A.DConst (c, ts)) = F.DConst (c, List.map (T.I o tty) ts)
    | const (A.LConst (k, t)) = F.LConst (k, (T.I o tty) t)

end
