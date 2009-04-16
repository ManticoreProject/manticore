val lengthP = Ropes.length
val subP = Ropes.sub
val concatP = Ropes.concat
val filterP = Ropes.filterP

structure Quicksort = struct

    fun quicksort xs =
	if lengthP xs <= 1 then
	    [| |]
	else
	    let
		val p = subP (xs, lengthP xs div 2)
		val lt = filterP (fn x => x < p, xs)
		val eq = filterP (fn x => x = p, xs)
		val gt = filterP (fn x => x > p, xs)
		val (lt, gt) = (| quicksort lt, quicksort gt |)
	    in
		concatP (lt, (concatP (eq, gt)))
	    end

end
