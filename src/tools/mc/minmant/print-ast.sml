(* print-ast.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrintAST : sig

    val output : TextIO.outstream * AST.module -> unit
    val print : AST.module -> unit

  end = struct

    structure A = AST

    fun output (outS, _) = TextIO.output(outS, "***AST***\n")

    fun print m = output (TextIO.stdErr, m)

  end
