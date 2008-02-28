structure BatcherBitonicSort = 
  struct

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

    fun bitonicSort (arr) = if (List.length(arr) = 1)
	   then arr
	   else let
              val (bot, top) = psplit(arr)
	      val mins = ListPair.map Int.min (bot, top)
	      val maxs = ListPair.map Int.max (bot, top)
	      in
		   bitonicSort(mins) @ bitonicSort(maxs)
	      end

    val xs = [1,3,7,8,17,4,2,0]
    val ys = bitonicSort(xs)

    fun batcherSort (arr) = if (List.length(arr) = 1)
	    then arr
	    else let
              val (bot, top) = psplit(arr)		    
	      val sortedBot = batcherSort(bot)
	      val sortedTop = batcherSort(top)
	      in
		    bitonicSort(sortedBot @ List.rev(sortedTop))
	      end

    val xs1 = [8,7,6,5,4,3,2,1]
    val xs1 = [0,1,2,19,10,0,1,6,~1]
    val ys1 = batcherSort(xs1)

  end
