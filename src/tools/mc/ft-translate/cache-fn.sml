(* cache-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A functor that builds a cache of items of type t indexed by integers.
 * (The lookup index type could be generalized as a possible improvement.)
 *)

functor CacheFn (M : sig

    type t
    val mkItem : int -> t

  end) : sig

    val getItem : int -> M.t 
			 
  end = struct

    structure IHT = IntHashTable

    val cache : M.t IHT.hash_table = IHT.mkTable (8, Fail "CacheFn")
				     
    val insert = IHT.insert cache
    val find = IHT.find cache
	      
    fun getItem (n : int) : M.t = (case find n
	   of NONE => let
		val item = M.mkItem n
		in
		  insert (n, item); item
		end
	    | SOME item => item
	  (* end case *))

  end
