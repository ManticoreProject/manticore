structure Quicksort = struct

    fun quicksort xs =
	if lengthP xs <= 1 then
	    xs
	else
	    let
		val p = subP (xs, lengthP xs div 2)
		val (lt, gt) = (| quicksort (filterP (fn x => x < p, xs)), 
				  quicksort (filterP (fn x => x > p, xs)) |)
	    in
		concatP (lt, (concatP (filterP (fn x => x = p, xs), gt)))
	    end

end
