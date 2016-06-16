
(* Sequential quicksort lifted from internet *)

structure QSort = struct
fun sort l = quicksort(l, [])

and quicksort (nil, acc) = acc
  | quicksort (x::l, acc) =
    let
      val (smaller, greater) = partition (x, l)
     in
       quicksort (smaller, x :: (quicksort (greater, acc)))
    end

and partition (x, nil) = (nil, nil)
  | partition (x, y::l) =
    let
      val (smaller, greater) = partition (x, l)
    in
      if y < x
	then (y::smaller, greater)
      else (smaller, y::greater)
    end
end  (* structure S *)


structure Main =
  struct

    fun testit () = let
      val max = 15000
	  val l = List.tabulate (max, fn i => max-i)
	  val l' = QSort.sort l
	  val (sorted,_) = List.foldl (fn (x, (sorted, max)) => 
					  		if not sorted 
					  		then (sorted, max)
					  		else (max <= x, x))
	  						(true, ~1) l'
	  in
	  	if sorted 
	  	then Print.print "correct\n"
	  	else Print.print "NOT CORRECT\n"
	  end
 end

val _ = Main.testit()
