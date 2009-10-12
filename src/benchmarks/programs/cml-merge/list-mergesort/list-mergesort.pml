(* list-mergesort.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Mergesort over a list.
 *)

structure ListMergesort =
  struct

    fun merge (x,y) = (
	  case (x,y)
	   of (nil,y) => y
	    | (x,nil) => x
	    | ((hx::tx),
	       (hy::ty)) => 
	      if hx < hy then hx :: merge(tx,(hy::ty))
	      else hy :: merge((hx::tx),ty)
          (* end case *))

    fun split xs = (
	  case xs
	   of (nil) => (nil,nil)
	    | (a::nil) => (a::nil,nil)
	    | (a::b::t) => let val (x,y) = split(t) in (a::x,b::y) end
          (* end case *))

    fun mergesort xs = (
	  case xs
	   of (nil) => nil
	    | (x::nil) => x::nil
	    | (l) =>
	      let
		  val (l1,l2) = split(l)
	      in
		  merge(mergesort(l1),mergesort(l2))
	      end
          (* end case *))

  end

structure Main =
  struct


    val dfltN = 100000
	
    fun main (_, args) =
	let
	    val n = (case args
		      of arg :: _ => Option.getOpt (Int.fromString arg, dfltN)
		       | _ => dfltN)
	    fun doit () =
		let		
		    val x = List.tabulate(n, fn _ => Rand.inRangeInt (0, 10000))
		in 
		    ListMergesort.mergesort x
		end
		
	in
	    RunSeq.run doit
	end

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
