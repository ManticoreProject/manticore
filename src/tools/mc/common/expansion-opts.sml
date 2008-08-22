(* expansion-opts.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Compiler options for expanding expressions.
 *)

structure ExpansionOpts =
  struct

  (* options for pvals *)
    datatype pval_opt
      = WORK_STEALING
      | GANG_SCHEDULING
      | CANCELABLE

  (* options for parallel arrays *)
    datatype parr_opt
      = LEAF_SIZE of int

  (* options for the program *)
    datatype opt
      = PVAL of pval_opt list
      | PARR of parr_opt list

  (* default options *)
    val defaultPVal : opt = PVAL [GANG_SCHEDULING]
    val defaultPArr : opt = PARR [LEAF_SIZE 1024]
    val defaults : opt list = [defaultPVal, defaultPArr]

  (* parse a pval option *)
    fun pvalOptsFromString strs = (
	  case strs
	   of ["work-stealing"] => WORK_STEALING
	    | ["gang-scheduling"] => GANG_SCHEDULING
	    | ["cancelable"] => CANCELABLE
	    | _ => raise Fail "unknown opt"
          (* end case *))

  (* parse a parallel array option *)
    fun parrOptsFromString strs = (
	  case strs
	   of ["leaf-size(", size, ")"] => LEAF_SIZE (Option.valOf (Int.fromString size))
	    | _ => raise Fail "unknown opt"
          (* end case *))

    val sepByDot = String.tokens (fn c => c = #".")
    val sepByComma = String.tokens (fn c => c = #",")
    fun hasPrefix prefix = List.filter (fn x :: xs => x = prefix)
    fun withPrefix prefix = List.map List.tl o hasPrefix prefix

  (* parse a string of options 
   * e.g., "parr.leaf-size(1024)" ===> [PARR[LEAF_SIZE 1024]]
   *)
    fun fromString (str, opts) = let
	  val strs : string list = sepByDot str
          in
	     case strs
	      of "pval" :: strs =>
		 PVAL [pvalOptsFromString strs] :: opts
	       | "parr" :: strs => 
		 PARR [parrOptsFromString strs] :: opts
	  end

    val fromStrings = List.foldl fromString []

  end
