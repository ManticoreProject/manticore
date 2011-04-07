(* vector-seq.pml  
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

(* define LARGE_VECTORS_IN_GLOBAL_HEAP if large vectors (having length of max leaf size) should be allocated in the global heap *)

structure VectorSeq =
  struct

    structure V = Vector

    type 'a seq = 'a V.vector

#if LARGE_VECTORS_IN_GLOBAL_HEAP
    local
	_primcode (
	  define @promote-vec (vec : V.vector / exh : exh) : V.vector =
	      let vec : V.vector = promote (vec)
	      return (vec)
	    ;
	)
	val promoteVec : 'a V.vector -> 'a V.vector = _prim (@promote-vec)
    in

    fun fromList s = let
      val v = V.fromList s
      in
       (* once a vector has reached the max leaf size, we assume that vector
	* will eventually be shared with remote processors, which is why we
	* promote the vector here.
	*)
         if V.length v < MaxLeafSize.sz then
	     v
	 else
	     promoteVec v
      end

    fun fromListRev s = let
      val v = V.fromListRev s
      in
       (* once a vector has reached the max leaf size, we assume that vector
	* will eventually be shared with remote processors, which is why we
	* promote the vector here.
	*)
         if V.length v < MaxLeafSize.sz then
	     v
	 else
	     promoteVec v
      end
    end (* local *)
#else
    val fromList = V.fromList

    val fromListRev = V.fromListRev
#endif

    val empty = fromList nil

    fun singleton s = fromList (s::nil)

    fun null s = (V.length s = 0)

    val length = V.length

    val sub = V.sub

    fun tabulate (n, f) = fromList (List.tabulate (n, f))

    fun concat (x, y) = let
      val xn = length x
      val yn = length y
      fun elt i =
	if i < xn then
	  sub (x, i)
	else
	  sub (y, i-xn)
      in
	tabulate (xn+yn, elt)      
      end

    fun take (s, n) = let
      val len = length s
      in
	if n >= len then s
	else tabulate (n, fn i => sub (s, i))
      end

    fun drop (s, n) = let
      val len = length s
      in
	if n >= len then empty
	else tabulate (len-n, fn i => sub (s, i+n))
      end

    fun splitAt (s, i) = (take (s, i+1), drop (s, i+1))

    fun toList s = let
      val len = length s
      fun lp (i, acc) = 
	if i < 0 then acc
	else lp (i-1, sub(s,i)::acc)
      in
	lp (len-1, nil)
      end

    fun rev s = let
      val len = length s
      in
	tabulate (len, fn i => sub (s, len - i - 1))
      end

    fun map (f, s) = tabulate (length s, fn i => f (sub (s, i)))

    fun map2 (f, s1, s2) = let
      fun min (m, n) = if m < n then m else n
      val minlen = min (length s1, length s2)
      in
	tabulate (minlen, fn i => f (sub (s1, i), sub (s2, i)))
      end

    fun map2Eq (f, s1, s2) = let
      val len1 = length s1
      val len2 = length s2
      in
        if (len1 = len2) then
          tabulate (len1, fn i => f (sub (s1, i), sub (s2, i)))
	else
          raise Fail "map2Eq"
      end

    fun foldr (f, z, s) = let
      val len = length s
      fun lp (i, acc) = 
	if i < 0 then acc
	else lp (i-1, f (sub (s,i), acc))
      in
	lp (len-1, z)
      end       

    fun foldl (f, z, s) = let
      val len = length s
      fun lp (i, acc) =
	if i >= len then acc
	else lp (i+1, f (sub(s,i), acc))
      in
	lp (0, z)
      end       

    fun reduce (oper, unit, s) = foldl (oper, unit, s)

    fun cut (s, n) = (take (s, n), drop (s, n))

    fun filter (pred, s) =
      if null s then empty
      else let
	fun lp (i, acc) = 
	  if i < 0 then
	    fromList acc
	  else let
	    val x = sub (s, i)
	    in
	      if pred x then lp (i-1, x::acc)
	      else lp (i-1, acc) 
	    end
	in
	  lp (length s-1, nil)
	end

    fun zip (s1, s2) = let
      fun min (m, n) = if m < n then m else n
      val len = min (length s1, length s2)
      in
	if len = 0 then empty 
	else tabulate (len, fn i => (sub(s1, i), sub(s2, i)))
      end

    fun unzip s = 
      (tabulate (length s, fn i => fst (sub (s, i))),
       tabulate (length s, fn i => snd (sub (s, i))))

    fun update (s, i, x) = tabulate (length s, fn j => if i = j then x
						       else sub (s, j))

  (* short-circuits on finding false *)
    fun any pred s = let
      val len = length s
      fun lp i =
	if i >= len then true
	else pred (sub (s, i)) andalso lp (i+1)
      in
	lp 0
      end

  (* short-circuits on finding true *)
    fun exists pred s = let
      val len = length s
      fun lp i =
	if i >= len then false
	else pred (sub (s, i)) orelse lp (i+1)
      in
	lp 0
      end

  end
