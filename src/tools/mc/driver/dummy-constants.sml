(* dummy-constants.sml
 *
 * This file is used when compiling on an unsupported architecture.
 *)

structure RuntimeConstants : RUNTIME_CONSTANTS =
  struct

  (* word size and alignment *)
    val wordSzB : IntInf.int = 8
    val wordAlignB : IntInf.int = 8
    val boolSzB : IntInf.int = 8
    val extendedAlignB : IntInf.int = 8

  (* stack size and heap size info *)
    val spillAreaSzB : IntInf.int = 2096
    val spillAreaOffB : IntInf.int = 48
    val maxObjectSzB : IntInf.int = 281474976710655

  (* offsets into the VProc_t structure *)
    val atomic : IntInf.int = 8
    val sigPending : IntInf.int = 16
    val currentTId : IntInf.int = 24
    val actionStk : IntInf.int = 32
    val rdyQHd : IntInf.int = 40
    val rdyQTl : IntInf.int = 48
    val secondaryQHd : IntInf.int = 56
    val secondaryQTl : IntInf.int = 64
    val stdArg : IntInf.int = 72
    val stdEnvPtr : IntInf.int = 80
    val stdCont : IntInf.int = 88
    val stdExnCont : IntInf.int = 96
    val allocPtr : IntInf.int = 104
    val limitPtr : IntInf.int = 112

  (* mask to get address of VProc from alloc pointer *)
    val vpMask : IntInf.int = 0xfffffffffff00000

  (* magic number *)
    val magic : IntInf.int = 0xe8c78135

  end (* RuntimeConstants *)
