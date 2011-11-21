structure Eg3 = struct

  structure R = RopeFn (
		   structure S = VectorSeq
		   val maxLeafSize = 2)

  val r = R.fromList (List.tabulate (5, fn i => i))
  val xsl = R.foldli (fn (i : int, x : int, xs : (int * int) list) => (i, x) :: xs) [] r
  val xsr = R.foldri (fn (i : int, x : int, xs : (int * int) list) => (i, x) :: xs) [] r
  fun p2s (i1, i2) = "(" ^ Int.toString i1 ^ ", " ^ Int.toString i2 ^ ")"

  val _ = print (String.concatWith " " (List.map p2s xsl)^"\n")
  val _ = print (String.concatWith " " (List.map p2s xsr)^"\n")

end
