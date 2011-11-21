structure Eg2 = struct

  structure R = RopeFn (
		   structure S = VectorSeq
		   val maxLeafSize = 2)

  val _ = print (String.concatWith " " (List.map Int.toString (R.toList (R.concat (List.map R.singleton (List.tabulate (5, fn i => i))))))^"\n")

end
