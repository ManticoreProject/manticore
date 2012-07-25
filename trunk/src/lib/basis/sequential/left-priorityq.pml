(* left-priorityq.pml
 *
 * COPYRIGHT (c) 2009 Bell Labs, Lucent Technologies
 *
 * An implementation of priority queues based on leaftist heaps (see
 * Purely Functional Data Structures by Chris Okasaki).
 *)

structure LeftPriorityQ =
  struct

    datatype 'a queue = Q of (int * 'a heap)
    and 'a heap = EMPTY | ND of (int * 'a * 'a heap * 'a heap)

    val empty  = Q(0, EMPTY)

    fun singletonHeap x = ND(1, x, EMPTY, EMPTY)
    fun singleton x = Q(1, singletonHeap x)

    fun rank x =
	(case x
	  of EMPTY => 0
	   |  (ND(r, _, _, _)) => r
	(* end case *))

    fun mkNode (x, a, b) = if (rank a >= rank b)
	  then ND(rank b + 1, x, a, b)
	  else ND(rank a + 1, x, b, a)

    fun mergeHeap (priority, compare) (h1, h2) =
	  (case (h1, h2)
	    of (h, EMPTY) => h
	     | (EMPTY, h) => h
	     | (ND(_, x, h11, h12), ND(_, y, h21, h22)) => (
	       case compare(priority x, priority y)
		of GREATER => mkNode(x, h11, mergeHeap(priority, compare) (h12, h2))
		 | _ => mkNode(y, h21, mergeHeap(priority, compare) (h1, h22))
	       (* end case *))
	  (* end case *))

    fun insert (priority, compare) (x, Q(n, h)) = Q(n+1, mergeHeap(priority, compare) (singletonHeap x, h))

    fun next (priority, compare)  x = 
	  (case x
	    of (Q(_, EMPTY)) => Option.NONE
	     | (Q(n, ND(_, x, h1, h2))) => Option.SOME(x, Q(n-1, mergeHeap(priority, compare) (h1, h2)))
	  (* end case *))

    fun remove (priority, compare) x =
	(case x
	  of (Q(_, EMPTY)) => (raise Fail "LeftPriorityQ.remove")
	   | (Q(n, ND(_, x, h1, h2))) => (x, Q(n-1, mergeHeap(priority, compare) (h1, h2)))
	(* end case *))

    fun merge (priority, compare) (Q(n1, h1), Q(n2, h2)) = Q(n1+n2, mergeHeap(priority, compare) (h1, h2))

    fun numItems (Q(n, _)) = n

    fun isEmpty x =
	  (case x
	    of (Q(_, EMPTY)) => true
	     | _ => false
	  (* end case *))

  end
