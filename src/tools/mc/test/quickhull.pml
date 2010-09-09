type point = float * float;

(* no references allowed, so needed another way to find farthest point.  jury-rigged merge-sort seemed fine *)
fun merge(l) = (case (l) of 
    (nil, ys) => ys
  | (xs, nil) => xs
  | ((x,a)::xs', (y,b)::ys') =>
      (if x > y then (y,b) :: merge ((x,a)::xs', ys')
      else  (x,a) :: merge (xs', (y,b)::ys')));

fun merge_sort(l) = (case (l) of
    nil => nil
  | xs => (if (List.length(xs) > 1) then (let
      val ys = List.take (xs, List.length(xs) div 2)
      val zs = List.drop (xs, List.length(xs) div 2)
    in
      merge (merge_sort ys,merge_sort zs)
    end)   else xs));


fun distance(a,b) = (let val (q,w) = a
                         val (z,x) = b
		     in 
		 	 Float.sqrt((q - z)*(q - z) + (w - x)*(w - x))
	             end);

fun farthest(a,b,S) = (let val (dist,pt) = List.hd(List.rev(merge_sort(List.map (fn (x) => (distance(a,x) + distance(b,x), x)) S))) in (pt) end);
         

fun upto((x,y) : point,(q,w) : point,dx,dy,lyst) = (if ((x = q) andalso (y = w)) then lyst
                                   else upto((x+dx,y+dy),(q,w),dx,dy,lyst@[(x+dx,y+dy)]));  

fun line(a : point,b : point) = (let val (x,y) = a
                     val (q,w) = b
		     val dx = (x - q)
		     val dy = (y - w)
		     val lyst = [a];
		  in 
		     upto((x,y),(q,w),dx,dy,lyst)
		  end);



fun isrightof(pt : point,line) = (case (pt,line) of
                          (_,nil) => false
                        | (p,x::xs) => (let val (a,b) = p
                                            val (q,w) = x
                                       in  (if a > q andalso b > w then true else isrightof(pt,xs)) end)    
                          );

fun isbelow(pt : point,line) = (case (pt,line) of
                          (_,nil) => false
                        | (p,x::xs) => (let val (a,b) = p
                                            val (q,w) = x
                                       in  (if a < q andalso b < w then true else isrightof(pt,xs)) end)    
                          );


fun rightof(a,b,S) = (List.filter (fn (x) => isrightof(x,line(a,b)))  S);

fun quickhull(a,b,S) = 
 (case S of 
    nil => nil
  | _ => (let val c = farthest(a,b,S)
              val A = rightof(a,c,S)
	      val B  = rightof(c,b,S)
	      val x = quickhull(a,c,A)
	      val y = quickhull(c,b,B)
          in
	      x @ [c] @ y
	  end));

val _ = quickhull((0.0,0.0),(1.0,1.0),nil) @ quickhull((0.0,0.0),(1.0,1.0),nil)
