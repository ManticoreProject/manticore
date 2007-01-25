(* print-cps.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrintCPS : sig

    val print : CPS.module -> unit

  end = struct

    fun print _ = TextIO.print "<cps>\n"

  end
