structure TestPMergesort =
  struct

    structure S = ListSeq
    structure R = Ropes
    val print = Print.print
    val itos = Int.toString
    fun not b = if b then false else true

    fun p x = Print.printLn(Int.toString x)

    fun lessThan (x, y) = x < y

    val xs = R.fromList(3 :: 2 :: ~1343 :: 111 :: 1 :: nil)
val _ = Print.printLn "sorting"
    val xs' = PMergesort.pMergesort lessThan xs
    val () = Array64.app p (R.toSeq xs')

  end
