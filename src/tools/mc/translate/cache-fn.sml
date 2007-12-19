(* cache-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A functor that builds a cache of items of type t.
 * Items are looked up with ints.
 * (The lookup index type could be generalized as a possible improvement.)
 *)

functor CacheFn(M : sig

    type t
    val mkItem : int -> t

  end) : sig

    val getItem : int -> M.t 
			 
  end = struct

    structure IHT = IntHashTable

    val NotFound = LibBase.NotFound

    val cache : M.t IHT.hash_table = IHT.mkTable (8, NotFound)
				     
    val ins = IHT.insert cache
	      
    val lkp = IHT.lookup cache
	      
    fun getItem (n : int) : M.t =
	let fun remember itemN = itemN before ins (n, itemN)
	in
	    lkp n handle NotFound => remember (M.mkItem n)
	end

  end
