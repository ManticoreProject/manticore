(* pquickhull.pml
 * 
 * Parallel quickhull written by Josh.
 *) 

fun isLess c = (case c of LESS => true | _ => false)
fun isEqual c = (case c of EQUAL => true | _ => false)
fun isGreater c = (case c of GREATER => true | _ => false)

fun quicksort (cmp, xs) =
    if lengthP xs < 1 then
	[| |]
    else
	let
	    val p = subP (xs, lengthP xs div 2)
	    val lt = filterP (fn x => isLess (cmp (x, p)), xs)
	    val eq = filterP (fn x => isEqual (cmp (x, p)), xs)
	    val gt = filterP (fn x => isGreater (cmp (x, p)), xs)
	    val (lt, gt) = (| quicksort (cmp, lt), quicksort (cmp, gt) |)
	in
	    concatP (lt, (concatP (eq, gt)))
	end

type point = float * float

fun distance ((q, w), (z, x)) = Float.sqrt ((q - z) * (q - z) + (w - x) * (w - x))

fun lastP x = subP (x, lengthP x - 1)

fun farthest (a, b, S) = 
    let
	fun dist x = (distance (a, x) + distance (b, x), x)
	fun cmp ((d1, _), (d2, _)) = Float.compare (d1, d2)
	val (_, pt) = lastP (quicksort (cmp, mapP (dist, S)))
    in
	pt
    end

(* returns true if the point (x,y) is to the right of the vector defined by (a,b). we use the
 * cross product to determine this case. *)
fun isRight ((*a as *) (x1, y1), (* b as *) (x2, y2)) (x, y) = 
    (x1 - x) * (y2 - y) - (y1 - y) * (x2 - x) < 0.0

(* returns those points to the right of the line defined by (a,b) *)
fun pointsRightOf (a, b, S) = filterP (isRight (a, b), S)

fun quickhull' (a, b, S) = 
    if lengthP S = 0 then
	[| |]
    else
	let
	    val c = farthest (a, b, S)
	    val (x, y) = (| quickhull' (a, c, pointsRightOf (a, c, S)), 
		            quickhull' (c, b, pointsRightOf (c, b, S)) |)
	in
	    concatP (concatP (x, [| c |]), y)
	end
	
fun quickhull S = 
    if lengthP S = 0 then
	[| |]
    else
	let
	    val p0 = subP (S, 0)
	    fun down ((x1, y1), (x2, y2)) = if x1 < x2 andalso y1 < y2 then (x1, y1) else (x2, y2)
	    fun up ((x1, y1), (x2, y2)) = if x1 > x2 andalso y1 > y2 then (x1, y1) else (x2, y2)
	    (* points x0 and y0 lie on the convex hull *)
	    val (x0, y0) = (| reduceP (down, p0, S), reduceP (up, p0, S) |)
	in
	    quickhull' (x0, y0, S)
	end
