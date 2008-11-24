structure TestPMergesort =
  struct
  
    structure K = Int

  (* log : real -> (real -> real) *)
    fun log base x = Math.ln x / Math.ln base

  (* ceilingLg : int -> int *)
  (* The ceiling of the log_2 of the input. *)
    val ceilingLg = ceil o log 2.0 o real

    fun lessThan (x, y) = (
	  case K.compare(x, y)
	   of LESS => true
	    | _ => false
          (* end case *))

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

  (* merge two sorted lists into one sorted list *)
    fun merge (xs, ys) = (
	  case (xs, ys)
	   of (nil, ys) => ys
	    | (xs, nil) => xs
	    | (x :: xs, y :: ys) => 
	      if lessThan(x, y) then x :: merge(xs, y :: ys) else y :: merge(x :: xs, ys)
          (* end case *))

    structure S = PMergesortWithSeqBcFn(
                    structure K = 
		      struct
		        type ord_key = int
			val compare = Int.compare
		      end
		    structure R = R
		    val sMerge = merge
		    val sSort = ListQuicksort.quicksort
		  )

    val r = Random.rand(234, 33333) 
    fun randInt _ = Random.randRange (0, 10000) r
    fun randList _ = List.tabulate(randInt(), randInt) 

    fun pmsort ls = R.toSeq(S.pMergesort (R.fromSeq ls))

    fun eq (x, y) = x = y

    fun test ls = ListPair.allEq eq (pmsort ls, ListQuicksort.quicksort ls)

    fun t n = 
	if List.all test (List.tabulate(n, randList))  
	   then ()
	else (raise Fail "test failed")


  end
