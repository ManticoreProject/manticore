structure BatcherBitonicSort = 
  struct

    fun parrSubseq (arr, i, j) = let
  	  val begin = List.drop (arr, i)
          in
	      List.take(begin, j-i)
          end

    fun bitonicSort (arr) = let
	val n = List.length(arr)
        in
           if (n = 1)
	   then arr
	   else let
              val bot = parrSubseq(arr, 0, n div 2)
	      val top = parrSubseq(arr, n div 2, n)
	      val mins = ListPair.map Int.min (bot, top)
	      val maxs = ListPair.map Int.max (bot, top)
	      in
		   bitonicSort(mins) @ bitonicSort(maxs)
	      end
        end

    val xs = [1,3,7,8,17,4,2,0]
    val ys = bitonicSort(xs)

    fun batcherSort (arr) = let
	val n = List.length(arr)
        in
	    if (n = 1)
	       then arr
	    else let
	      val bot = batcherSort(parrSubseq(arr, 0, n div 2))
	      val top = batcherSort(parrSubseq(arr, n div 2, n))
	      in
		    bitonicSort(bot @ List.rev(top))
	      end
        end

    val xs1 = [8,7,6,5,4,3,2,1]
    val xs1 = [0,1,2,19,10,0,1,6,~1]
    val ys1 = batcherSort(xs1)

  end
