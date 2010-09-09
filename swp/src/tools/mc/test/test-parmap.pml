val seqSz = readint();

fun timeToEval (str, f) = let
    val b = gettimeofday ()
    val v = f()
    val e = gettimeofday ()
    in
       print (str^": "^dtos (e-b)^"\n");
       v
    end
;

fun splitAt (n, ls) = let
    fun split (i, x :: xs, ys) = if (i < n)
        then split(i+1, xs, x :: ys)
        else (rev(ys), x :: xs)
    in
        split(0, ls, nil)
    end
;

(* Map the function f over the list ls in parallel.
 *
 *    PREDCONDITION: n = length(ls)
 *)
fun parMap (f, n, ls) = (case ls
    of nil => nil
     | _ => if (n <= seqSz)
        then map(f, ls)
        else let
          val n1 = n div 2
  	  val (xs, ys) = splitAt(n1, ls)
          dval fxs = parMap(f, n1, xs)
	  val fys = parMap(f, n-n1, ys)
          in
	     fxs @ fys
          end
   (* end case *))
;

fun zip (xs, ys) = let
    fun loop (xs, ys, zs) = (case (xs, ys)
        of (nil, _) => rev(zs)
	 | (_, nil) => rev(zs)
	 | (x :: xs, y :: ys) => loop(xs, ys, (x, y) :: zs)
        (* end case *))
     in
        loop(xs, ys, nil)
     end
;

fun testparmap (f, ls) = let
    val n = length(ls)
    val ls1 = timeToEval("map", let fun g() = map(f, ls) in g end)
    val ls2 = timeToEval("parMap", let fun g() = parMap(f, n, ls) in g end)
    fun equal ((x:int, y:int), eq) = x=y andalso eq
    in
       foldl(equal, true, zip(ls1, ls2)) andalso (length(ls1)=length(ls2)) andalso (length(ls1)=n)
    end
;

fun add1(x) = x+1;
fun id (x) = x;

if (testparmap(add1, tab(id, 0, readint(), 1)))
    then print "ok\n"
    else print "error\n"