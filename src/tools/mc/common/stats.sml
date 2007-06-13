(* stats.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Collect statistics about the various optimizations and transformations.
 *)

structure Stats :> sig

    type counter

    val newCounter : string -> counter

    val tick : counter -> unit		(* increment by one *)
    val bump : (counter * int) -> unit	(* increment by an integer *)
    val count : counter -> int		(* return current count *)
    val name : counter -> string	(* return counter's name *)
    val reset : counter -> unit		(* reset counter to zero *)

    val sum : {from : counter, to : counter} -> int
    val sumAll : unit -> int

    val resetAll : unit -> unit
    val report : unit -> (string * int) list

    val verbose : bool ref		(* if set, report ticks *)

  end = struct

    structure A = Array

    val maxNumCounters = 512

    val names = A.array(maxNumCounters, "")
    val counters = A.array(maxNumCounters, 0)

    val nextCounter = ref 0
    val verbose = ref false

    type counter = int

    fun newCounter name = let
	  val n = !nextCounter
	  in
	    if (n < maxNumCounters)
	      then (A.update(names, n, name); nextCounter := n+1; n)
	      else raise Fail "too many counters"
	  end

    fun tick i = (
	  if (! verbose)
	    then print(concat["++ ", A.sub(names, i), "\n"])
	    else ();
	  A.update(counters, i, A.sub(counters, i)+1))
    fun bump (i, n) = A.update(counters, i, A.sub(counters, i)+n)
    fun count i = A.sub(counters, i)
    fun name i = A.sub(names, i)
    fun reset i = A.update(counters, i, 0)

    fun sum {from : counter, to : counter} =
	  if (to < from)
	    then 0
	    else ArraySlice.foldl
	      (fn (n, s) => n+s) 0
		(ArraySlice.slice(counters, from, SOME((to-from)+1)))

    fun sumAll () = sum{from = 0, to = (!nextCounter - 1)}

    fun resetAll () = A.modify (fn _ => 0) counters

    fun report () = List.tabulate (!nextCounter, fn i => (name i, count i))

  end

