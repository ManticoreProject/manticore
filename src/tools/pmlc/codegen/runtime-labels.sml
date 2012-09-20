(* runtime-labels.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Fixed labels to interface with the outside world.
 *)

structure RuntimeLabels =
  struct

    local
      val global = Label.global
    in
  (* entry point for main program *)
    val entry = global "mantEntry"
  (* label of word that holds "magic number".  This value used to check the
   * consistency between the runtime and compiler offsets.
   *)
    val magic = global "mantMagic"
  (* label of flag that tells the runtime if the generated code is sequential *)
    val sequential = global "SequentialFlag"
  (* runtime code to invoke the GC *)
    val initGC = global "ASM_InvokeGC"
  (* runtime code to promote objects *)
    val promote = global "PromoteObj"
  (* runtime code to get a new global-heap chunk *)
    val getGlobalChunk = global "GetChunkForVProc"
    val allocVector = global "AllocVector"
    val allocIntArray = global "AllocIntArray"
    val allocLongArray = global "AllocLongArray"
    val allocFloatArray = global "AllocFloatArray"
    val allocDoubleArray = global "AllocDoubleArray"
    end (* local *)

  end (* RuntimeLabels *)
