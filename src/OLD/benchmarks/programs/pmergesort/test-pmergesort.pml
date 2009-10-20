structure TestPMergesort =
  struct

    structure S = ListSeq
    val print = Print.print
    val itos = Int.toString
    fun not b = if b then false else true

    fun p x = Print.printLn(Int.toString x)

    fun lessThan (x, y) = x < y

    val xs = PArray.fromList(3 :: 2 :: ~1343 :: 111 :: 1 :: nil)
    val xs' = PMergesort.pMergesort lessThan xs
    val () = Array64.app p (R.toSeq xs')

  end
