(* pipeline.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Pipeline example from Mirani and Hudak (TOPLAS 2004).
 *)

functor Pipeline (
    structure S : FCMS
  ) : sig

    structure S : FCMS

  (* takes a list of functions and an argument list and maps the composition of all the functions over
   * all the arguments.
   *)
    val pipeline : ('a S.future -> 'a S.future) list -> 'a S.future list -> 'a S.future list

  end = struct

    structure S = S

    fun smap f xs = (
	  case xs
	   of nil => nil
	    | y :: ys => let
		val z = f y
		val zs = S.future(fn () => smap f ys)
	        in
		  S.sched (S.future(fn () => z :: S.touch zs)) (S.seq (S.e z) (S.d zs))
	        end
          (* end case *))

    fun pipeline fs xs = (
	  case fs
	   of nil => xs
	    | f :: fs => let
		val ys = S.future(fn () => smap f xs)
		val zs = S.future(fn () => pipeline fs (S.touch ys))
	        in
		  S.sched zs (S.par (S.e ys) (S.e zs))
		end
          (* end case *))

  end
