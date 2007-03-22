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
	val wordSzB = 0w4
	val wordAlignB = 0w4
	val boolSzB = 0w4
	val extendedAlignB = 0w4
    
	val spillAreaSzB = 0w1024
	val bitMaskSzB = wordSzB - 0w1
      (* number aligned words * number of bits in the object-header bitmask  *)
	val maxObjectSzB = wordAlignB * (bitMaskSzB * 0w8)  
	val allocChunkSzB = 0w64 * 0w1024

      (* offsets into the VProc_t structure *)
	val inManticore : IntInf.int = 0
	val atomic : IntInf.int = 4
	val sigPending : IntInf.int = 8
	val actionStk : IntInf.int = 16
	val rdyQHd : IntInf.int = 20
	val rdyQTl : IntInf.int = 24
	val stdArg : IntInf.int = 28
	val stdPtr : IntInf.int = 32
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

