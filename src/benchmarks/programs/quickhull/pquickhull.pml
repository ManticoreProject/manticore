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

(* FIXME: 100 is just a hard-coded hack...is there a better way to determine sampling along the line? *)
fun upto ((x, y) : point,(q, w) : point, dx, dy, lyst, i) = 
    if i <= 0 then
	lyst
    else
	upto ((x + dx, y + dy), (q, w), dx, dy, concatP (lyst, [| (x + dx, y + dy) |]),i-1)

fun line (a : point, b : point) = 
    let val (x, y) = a
        val (q, w) = b
	val dx = (x - q) / 100.0
	val dy = (y - w) / 100.0
	val lyst = [| a |]
    in 
	upto ((x, y), (q, w), dx, dy, lyst,100)
    end

fun isrightof ((a, b) : point, line) = 
    let
	val l = filterP (fn (x, y) =>  a > x andalso b > y, line)
    in
	lengthP l <> 0
    end

fun rightof (a, b, S) = 
    let
	val l = line (a, b)
    in
	filterP (fn x => isrightof (x, l), S)
    end

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
