structure TestPMergesort =
  struct

  (* log : real -> (real -> real) *)
    fun log base x = Math.ln x / Math.ln base

  (* ceilingLg : int -> int *)
  (* The ceiling of the log_2 of the input. *)
    val ceilingLg = ceil o log 2.0 o real

    structure R = RopesFn (structure S = ListSeq 
                           val sizeL1CacheLine= 4
			   val wordSize = 32
			   val ceilingLg = ceilingLg)

    structure LR =
      struct
        structure S = ListSeq
        type 'a rope = 'a list
	val maxLeafSize = 0
	fun isLeaf _ = true
	fun toSeq x = x
	fun fromSeq x = x
	val isEmpty = List.null
	fun depth _ = 0
        open ListSeq
      end
    
    structure S = PMergesortFn(
                    structure K = 
		      struct
		        type ord_key = int
			val compare = Int.compare
		      end
		    structure R = R)

    val r = Random.rand(0, 1034) 
    fun randInt _ = Random.randRange (0, 10000) r
    fun randList _ = List.tabulate(randInt(), randInt) 

    fun pmsort ls = R.toSeq(S.pMergeSort (R.fromSeq ls))

    fun eq (x, y) = x = y

    fun test ls = ListPair.allEq eq (pmsort ls, ListMergeSort.sort (op >) ls)

    fun t n = 
	if List.all test (List.tabulate(n, randList))  
	   then ()
	else (raise Fail "test failed")


  end
