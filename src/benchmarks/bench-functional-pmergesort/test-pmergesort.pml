structure TestPMergesort =
  struct

    structure S = ListSeq
    structure R = Ropes
    val print = Print.print
    val itos = Int.toString
    fun not b = if b then false else true

    val xs = 3 :: 2 :: ~1343 :: 111 :: 1 :: nil

    fun pmsort ls = R.toSeq(PMergesortWithSeqBc.pMergesort (R.fromSeq ls))

    fun p x = Print.printLn(Int.toString x)

(*    val () = List.app p (pmsort xs)*)

  end
