(* predecessors.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Predecessors : sig

    (* For all basic blocks of the module, predecessor information
       will be filled in. Useful during translation from CFG to SSA
       when creating phi-nodes. *)

    val analyze : CFG.module -> unit

  end = struct

    structure C = CFG

    (* make life easier *)
    val setPreds = C.Label.setPreds
    val getPreds = C.Label.getPreds
    val maybeGetPreds = C.Label.maybeGetPreds


    fun analyze code = raise Fail "not implemented yet."

  end
