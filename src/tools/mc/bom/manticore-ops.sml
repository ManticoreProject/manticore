(* manticore-ops.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This file defines various high-level operators that are required to implement
 * the translation from AST to BOM.  The actual definitions of the operators
 * are loaded on demand.
 *)

structure ManticoreOps : sig

    val spawnOp : BOM.hlop

  end = struct

    structure H = HLOp
    structure Ty = BOMTy

    val spawnOp = H.new(
	  Atom.atom "spawn",
	  {params = [H.PARAM(Ty.T_Fun([], [Ty.exhTy], []))], exh=[], results = [Ty.tidTy]},
	  [])

  end
