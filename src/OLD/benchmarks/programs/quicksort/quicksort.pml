structure Quicksort = struct

    fun quicksort xs =
	if lengthP xs <= 1 then
	    xs
	else
	    let
		val p = subP (xs, lengthP xs div 2)
		val (lt, eq, gt) = (| filterP (fn x => x < p, xs), 
				      filterP (fn x => x = p, xs),
				      filterP (fn x => x > p, xs) |)
		val (l, u) = (| quicksort lt, quicksort gt |)
	    in
		concatP (l, (concatP (eq, u)))
	    end

end
