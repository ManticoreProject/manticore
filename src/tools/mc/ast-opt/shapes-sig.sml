(* shapes-sig.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

signature SHAPES =

  sig

    type shape

    val shapeOf  : AST.exp -> shape
    val toString : shape -> string

  end
