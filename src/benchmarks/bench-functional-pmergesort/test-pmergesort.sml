structure TestPMergesort =
  struct
    
    structure FauxRope : ROPES =
      struct
        type 'a rope = 'a list
	val maxLeafSize = 1
	val empty = []
	val isEmpty = List.null
	fun isLeaf _ = true
	val length = List.length
	fun depth _ = 0
	fun concat (r1, r2) = r1 @ r2
	fun balance r = r
	val sub = List.nth
	fun splitAt (r, i) = (List.take(r, i), List.drop(r, i))
	fun toList r = r
	fun fromList r = r 
      end

    structure S = PMergesortFn(
                    structure K = 
		      struct
		        type ord_key = int
			val compare = Int.compare
		      end
		    structure R = FauxRope)

    fun test ls = ListPair.allEq (op =) (S.pMergeSort ls, ListMergeSort.sort (op >) ls)

    val tests = [
	         [1232,123222222,2,123,3,~2],
		 [1,2,3],
		 [3,2,1],
		 [12,2,2434,2312,24333,4,32,3,3,343,33,3,234657],
		 [],
		 [4],
		 [~123123,~232222,3,343]
	       ]

    val () = if List.all test tests
	       then ()
	    else raise Fail "test failed"

  end
