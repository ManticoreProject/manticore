(* amd64-darwin-pseudo-ops-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * The AMD64 pseudo operations for Linux, which uses the gas syntax.
 *)

functor AMD64PseudoOpsFn (
    structure T : MLTREE
    structure MLTreeEval : MLTREE_EVAL where T = T
  ) : PSEUDO_OPS_BASIS = AMD64DarwinPseudoOps (
    structure T = T
    structure MLTreeEval = MLTreeEval)
