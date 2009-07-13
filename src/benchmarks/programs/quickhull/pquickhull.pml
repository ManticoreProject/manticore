(* pquickhull.pml
 * 
 * Parallel quickhull written by Josh.
 *) 

fun isLess c = (case c of LESS => true | _ => false)
fun isEqual c = (case c of EQUAL => true | _ => false)
fun isGreater c = (case c of GREATER => true | _ => false)

fun quicksort (cmp, xs) =
    if lengthP xs <= 1 then
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

(* returns true if the point (x,y) is to the right of the vector defined by (a,b) *)
fun isRight ((*a as *) (x1, y1), (* b as *) (x2, y2)) (x, y) = 
    (* use the signed distance *)
    ((y1 - y2) * x + (x2 - x1) * y + (x1 * y2 - x2 * y1)) < 0

fun rightof (a, b, S) = filterP (isRight (a, b), S)

fun quickhull (a, b, S) = 
    if lengthP S = 0 then
	[| |]
    else
	let
	    val c = farthest (a, b, S)
            val A = rightof (a, c, S)
	    val B  = rightof (c, b, S)
	    val (x, y) = (| quickhull (a, c, A), quickhull (c, b, B) |)
        in
	    concatP (concatP (x, [| c |]), y)
	end
