(* test-translate-terms.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure TestTranslateTerms = struct

  structure A = AST
  structure F = FLAST
  
  val println = (fn s => (print s; print "\n"))

  fun mkTest (e : A.exp) : unit -> unit = (fn () => let
    val e' = TranslateTermsFT.trExp e
    in
      println "e before translation:";
      PrintAST.printExp e;
      println "e after transation:";
      PrintFLAST.printExp e'
    end)

  val test0 = mkTest (ASTUtil.mkInt 0)
  val test1 = mkTest (ASTUtil.trueExp)

end
