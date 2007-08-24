(* match-errors.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Error messages for the match compilation phase.
 *)

structure MatchErrors : sig

    type err_stream = Error.err_stream
    type location = Error.location

    val errRedundantMatch : (err_stream * location) -> unit
    val warnNonexhaustiveMatch : (err_stream * location) -> unit
    val warnNonexhaustiveBind : (err_stream * location) -> unit

  end = struct

    structure E = Error

    type err_stream = E.err_stream
    type location = E.location

    fun errRedundantMatch (errStrm, loc) = E.error(
	  errStrm, loc,
	  "redundant match")

    fun warnNonexhaustiveMatch (errStrm, loc) = Error.warning(
	  errStrm, loc,
	  "nonexhaustive match")

    fun warnNonexhaustiveBind (errStrm, loc) = Error.warning(
	  errStrm, loc,
	  "nonexhaustive binding")

  end

