(* dummy-spec.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Dummy Target OS/architecture specific information.
 *)

structure DummySpec : TARGET_SPEC =
  struct

    val archName = "dummy"
    val abiName = "dummy"
    val osName = "dummy"

    structure ABI = struct
	val wordSzB : IntInf.int = 4
	val wordAlignB : IntInf.int = 4
	val boolSzB : IntInf.int = 4
	val extendedAlignB : IntInf.int = 4
    
	val spillAreaSzB : IntInf.int = 1024
	val spillAreaOffB : IntInf.int = 0
	val bitMaskSzB : IntInf.int = wordSzB - 1
      (* number aligned words * number of bits in the object-header bitmask  *)
	val maxObjectSzB : IntInf.int = wordAlignB * (bitMaskSzB * 8)  
	val allocChunkSzB : IntInf.int = 64 * 1024

	val magic : IntInf.int = 0xdeadbeef

      (* offsets into the VProc_t structure *)
	val inManticore : IntInf.int = 0
	val atomic : IntInf.int = 4
	val sigPending : IntInf.int = 8
	val actionStk : IntInf.int = 16
	val rdyQHd : IntInf.int = 20
	val rdyQTl : IntInf.int = 24
	val stdArg : IntInf.int = 28
	val stdEnvPtr : IntInf.int = 32
	val stdCont : IntInf.int = 36
	val stdExnCont : IntInf.int = 40
	val allocPtr : IntInf.int = 44
	val limitPtr : IntInf.int = 48
    
      (* mask to get address of VProc from allocation pointer *)
	val vpMask : IntInf.int = 0xfff00000

      end

  (* representation of True and False *)
    val trueRep : IntegerLit.integer = 3
    val falseRep : IntegerLit.integer = 1

  end

