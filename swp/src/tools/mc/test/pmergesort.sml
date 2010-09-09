structure PMergesort =
  struct

    fun len (_, s1, s2) = s2-s1
    val pappend = (op @)
    val plen = List.length
    val psub = List.nth

    fun psubseq (arr, i, j) = let
  	  val begin = List.drop (arr, i)
          in
	      List.take(begin, j-i)
          end

    fun psplit (arr) = let
	val n = List.length(arr)
        in
            (psubseq(arr, 0, n div 2), psubseq(arr, n div 2, n))
        end

    (* assume that b>a. *)
    fun binarySearch' (arr, a, b, x) = if (b = a)
        then a
        else let
          val p = (b+a) div 2
          val (a, b) = if (psub(arr,p) < x)
		          then (p+1, b)
		          else (a,   p)
          in
	      binarySearch'(arr, a, b, x)
          end

    fun binarySearch (arr, a, b, x) = let
	val (a, b) = if (a < b) then (a, b) else (b, a)
        in
	    binarySearch' (arr, a, b, x)
        end

    fun bsEx () = let
	val xs = [1,4,5,6,10,100]
        in
	    binarySearch(xs, 0, plen(xs), 1001)
        end

    fun pMerge (lArr, rArr) = let
        fun loop ( l as (lArr, l1, l2), r as (rArr, r1, r2) ) =
	    if (len(l) < len(r))
  	       then loop(r, l)
	    else if (len(l) = 0 orelse len(r) = 0)
	       then pappend(psubseq(l), psubseq(r))
	    else if (len(l) = 1)
	       then if (psub(lArr,l1) < psub(rArr,r1))
	               then [ psub(lArr,l1), psub(rArr,r1) ]
		       else [ psub(rArr,r1), psub(lArr,l1) ]
	    else let
	       val lPvt = len(l) div 2 + l1
	       val j = binarySearch(rArr, r1, r2, psub(lArr,lPvt))
	       val c1 = loop( (lArr, l1, lPvt), (rArr, r1, j) )
	       val c2 = loop( (lArr, lPvt, l2), (rArr, j, r2) )
	       in
		    pappend(c1, c2)
	       end
        in
            loop( (lArr, 0, plen(lArr)), (rArr, 0, plen(rArr)) )
        end

    fun mergeEx () = let
	val xs = [~121,3,7]
	val ys = [2,10,100,500]
        in
          pMerge(xs, ys)
        end

    fun pMergesort (arr) = if (plen(arr) = 1)
        then arr
        else let
	  val (lArr, rArr) = psplit(arr)
	  val lArr = pMergesort(lArr)
	  val rArr = pMergesort(rArr)
	  in
	     pMerge(lArr, rArr)
	  end

    fun sortEx () = let
	val xs = [7,5,~32,34,23432,3,1]
	in
	    pMergesort(xs)
	end

  end
