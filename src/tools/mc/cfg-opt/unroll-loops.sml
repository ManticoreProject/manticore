(* unroll-loops.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure UnrollLoops : sig

    val transform : CFG.module -> CFG.module

  end = struct

    structure CFA = CFACFG

    fun transform m = m

  end
