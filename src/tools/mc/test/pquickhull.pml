fun first (a,b) = a 

fun quicksort xs =
	if lengthP xs <= 1 then
	    [| |]
	else
	    let
		val p = first(subP (xs, lengthP xs div 2))
		val lt = filterP (fn x => first(x) < p, xs)
		val eq = filterP (fn x => first(x) = p, xs)
		val gt = filterP (fn x => first(x) > p, xs)
		val (lt, gt) = (| quicksort lt, quicksort gt |)
	    in
		concatP (lt, (concatP (eq, gt)))
	    end

type point = float * float;

fun distance(a,b) = (let val (q,w) = a
                         val (z,x) = b
		     in 
		 	 Float.sqrt((q - z)*(q - z) + (w - x)*(w - x))
	             end);

fun farthest(a,b,S) = (let val (dist,pt) = subP(quicksort(mapP ( (fn (x) => (distance(a,x) + distance(b,x), x)), S)),lengthP(S)) in (pt) end);
         

fun upto((x,y) : point,(q,w) : point,dx,dy,lyst) = (if ((x = q) andalso (y = w)) then lyst
                                   else upto((x+dx,y+dy),(q,w),dx,dy,concatP(lyst,[|(x+dx,y+dy)|])));  


(* want a cleverer way to generate the line, but this isn't heinous for a naive approach *)
fun line(a : point,b : point) = (let val (x,y) = a
                     val (q,w) = b
		     val dx = (x - q) / 100.0
		     val dy = (y - w) / 100.0
		     val lyst = [|a|];
		  in 
		     upto((x,y),(q,w),dx,dy,lyst)
		  end);




fun isrightof(pt : point,line) = (let val (a,b) = pt in (let val l = (filterP ((fn (x,y) =>  a > x andalso b > y),line)) in (if lengthP l = 0 then false else true) end) end) 

fun isbelow(pt : point,line) = (let val (a,b) = pt in (let val l = (filterP ((fn (x,y) =>  a < x andalso b < y),line)) in (if lengthP l = 0 then false else true) end) end)

fun rightof(a,b,S) = (filterP ((fn (x) => isrightof(x,line(a,b))),  S));

fun below(a,b,S) = (filterP ((fn (x) => isbelow(x,line(a,b))),  S));


(* The following two functions are used to find the extreme points on a random list of points  *)

fun leftmosthighest (xs) = ( if lengthP xs <= 1 then
	    (~1.0,~1.0)
	else
	    let
		val (a,b) = subP (xs, lengthP xs div 2)
		val lt = filterP (fn (x,y) => x > a orelse y < b, xs)
		val eq = filterP (fn (x,y) => x = a andalso y = b, xs)
		val gt = filterP (fn (x,y) => x <= a andalso y > b, xs)
		val (lt, gt) = (| quicksort lt, quicksort gt |)
	    in
		subP(concatP (lt, (concatP (eq, gt))),lengthP xs - 1)
	    end);

fun rightmostlowest (xs) =  (if lengthP xs <= 1 then
	    (~1.0,~1.0)
	else
	    let
		val (a,b) = subP (xs, lengthP xs div 2)
		val lt = filterP (fn (x,y) => x < a orelse y > b, xs)
		val eq = filterP (fn (x,y) => x = a andalso y = b, xs)
		val gt = filterP (fn (x,y) => x >= a andalso y < b, xs)
		val (lt, gt) = (| quicksort lt, quicksort gt |)
	    in
		subP(concatP (lt, (concatP (eq, gt))),lengthP xs - 1)
	    end);

(* the main event *)

fun quickhull(a,b,S) = 
 (case lengthP(S) of 
   0  => [||]
  | _ => (let val c = farthest(a,b,S)
              val A = rightof(a,c,S)
	      val B  = rightof(c,b,S)
	      val (x,y) = (| quickhull(a,c,A), quickhull(c,b,B) |)
          in
	      concatP(concatP(x,[|c|]), y)
	  end));


fun genpoints (pa,x) = (if lengthP(pa) = x then pa
else genpoints(concatP(pa,[|Rand.randDouble(0.0,100.0), Rand.randDouble(0.0,100.0)|]), x));


val _ = concatP(quickhull((0.0,0.0),(1.0,1.0),[||]), quickhull((0.0,0.0),(1.0,1.0),[||]))

