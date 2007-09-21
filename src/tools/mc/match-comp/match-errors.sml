(* match-errors.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Error messages for the match compilation phase.
 *)

structure MatchErrors : sig

    type err_stream = Error.err_stream
    type span = Error.span

    val errRedundantMatch : (err_stream * span) -> unit
    val warnNonexhaustiveMatch : (err_stream * span) -> unit
    val warnNonexhaustiveBind : (err_stream * span) -> unit

  end = struct

    structure E = Error

    type err_stream = E.err_stream
    type span = E.span

    fun errRedundantMatch (errStrm, loc) = E.errorAt (
	  errStrm, loc,
	  ["redundant match"])

    fun warnNonexhaustiveMatch (errStrm, loc) = Error.warningAt (
	  errStrm, loc,
	  ["nonexhaustive match"])

    fun warnNonexhaustiveBind (errStrm, loc) = Error.warningAt (
	  errStrm, loc,
	  ["nonexhaustive binding"])

  end

