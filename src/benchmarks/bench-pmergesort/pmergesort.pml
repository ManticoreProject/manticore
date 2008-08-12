(* pmergesort-ip.pml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Copying version of parallel mergesort using arrays.
 *)

divert(-1)
changequote({,})   #change quotes to curly braces 
divert(0)
undefine({len})dnl

val seqSq = readint();
define({_SEQ_SZ_}, {seqSq})dnl

fun pow2 (n) = if (n=0) then 1 else 2 * pow2(n-1)
;

    fun copyArr (arr2) = let
	val arr1 = array(alength(arr2), asub(arr2, 0))
	fun loop (i) = if (i > 0)
            then (
               aupdate(arr1, i, asub(arr2, i));
	       loop(i-1))
            else ()
        in
	   loop(alength(arr1)-1);
	   arr1
        end
;

    val epsilon = 0.01;
    val abs = absd;

    fun arrayEq (arr1, arr2) = let
	fun loop (i) = if (i > 0)
            then (
               if (abs(asub(arr1, i) - asub(arr2, i)) <= epsilon)
                  then loop(i-1)
                  else false)
(*
(print(itos i^" "^dtos(asub(arr1, i))^" "^dtos(asub(arr2, i))^"\n");
loop(i-1)))
*)
            else true
        in
	   loop(alength(arr1)-1)	   
        end
;

    fun bubbleSort (arr) = let
	val n = alength(arr)
	fun swap (arr, i, j) = let
	    val t = asub(arr, i)
	    in
	       aupdate(arr, i, asub(arr, j));
	       aupdate(arr, j, t)
            end
	fun loop1 (i) = let
	    fun loop2 (j) = if (j >= i)
                then (
                      if (asub(arr, j-1) > asub(arr, j))
                         then swap (arr, j, j-1)
                         else ();
		      loop2(j-1))
                else ()
            in
               if (i < n)
		  then (
		   loop2(n-1);
		   loop1(i+1))
	          else ()
            end
         in
	    loop1(1)
         end
;

fun b2s (b) = if b then "true" else "false";

    fun len (_, s1, s2) = s2-s1
;

    fun maxval (x, y) = if x > y then x else y
;

    (* assume that b>a. *)
    fun binarySearch' (arr, a, b, x) = if (b = a)
        then a
        else let
          val p = (b+a) div 2
          val (a, b) = if (asub(arr,p) < x)
		          then (p+1, b)
		          else (a,   p)
          in
	      binarySearch'(arr, a, b, x)
          end
;

   (* find j such that arr[j] <= x <= arr[j+1] *)
    fun binarySearch (arr, a, b, x) = let
	val (a, b) = if (a < b) then (a, b) else (b, a)
        in
	    binarySearch' (arr, a, b, x)
        end
;

   (* copy l into d *)
    fun copy ( (dArr, d1, d2), (lArr, l1, l2) ) = let
	    val l = (lArr, l1, l2)
	    fun loop (i) = if (i >= 0)
                then (aupdate(dArr, d1+i, asub(lArr, l1+i));
		      loop(i-1))
                else ()
            in
	       loop(len(l)-1)
	    end
;

define({_PMERGE_}, {
  define({_PMERGE_FN_}, {$1})dnl
  define({_VAL_}, {$2})dnl
  define({_GOTO_SEQ_}, {$3})dnl
  define({_SEQ_FN_}, {$4})dnl
    fun _PMERGE_FN_ (d, l, r) = if (_GOTO_SEQ_)
        then (_SEQ_FN_ (d, l, r); 0)
        else (case (d, l, r)
 	       of ((dArr, d1, d2), (lArr, l1, l2), (rArr, r1, r2)) =>
		  if (len(l) < len(r))
		     then _PMERGE_FN_ (d, r, l)
	          else if (len(l) = 0 orelse len(r) = 0)
	               then (copy(d, l) ; 0)
	          else if (len(l) = 1)
	               then if (asub(lArr, l1) < asub(rArr, r1)) 
		            then (aupdate(dArr, d1,   asub(lArr, l1)); 
				  aupdate(dArr, d1+1, asub(rArr, r1));
				  0) 
		            else (aupdate(dArr, d1,   asub(rArr, r1)); 
				  aupdate(dArr, d1+1, asub(lArr, l1));
				  0)
		  
	          else let
	            val lLen' = len(l) div 2
		    val j = binarySearch(rArr, r1, r2, asub(lArr, lLen' + l1)) 
		    val rLen' = j - r1
		    _VAL_ c1 = _PMERGE_FN_ ( (dArr, d1, lLen' + rLen' + d1), 
					     (lArr, l1, lLen' + l1), 
					     (rArr, r1, j) )
		    val c2 = _PMERGE_FN_ ( (dArr, lLen' + rLen' + d1, d2), 
					   (lArr, lLen' + l1, l2), 
					   (rArr, j, r2) )
		    in
			  c1+c2
		    end)
;
})dnl
_PMERGE_({seqMerge}, {val}, {false}, {seqMerge})
_PMERGE_({pMerge'}, {dval}, {(maxval(len(r), len(l)) < _SEQ_SZ_)}, {seqMerge})

   (* merge sorted arrays arr[p..q] and arr[q..r] into the sorted array dArr[p..r] *)
    fun pMerge (dArr, arr, p, q, r) = 
	pMerge'( (dArr, p, r), (arr, p, q), (arr, q, r) )
;

define({_PMERGESORT_}, {
  define({_PMERGESORT_FN_}, {$1})dnl
  define({_VAL_}, {$2})dnl
  define({_GOTO_SEQ_}, {$3})dnl
  define({_SEQ_FN_}, {$4})dnl
    fun _PMERGESORT_FN_ (dArr, dArr', arr, p, r) = if (_GOTO_SEQ_)
        then (_SEQ_FN_ (dArr, dArr', arr, p, r); 0)
        else if (r-p > 1)
           then let
             val q = (p+r) div 2
             _VAL_ x = _PMERGESORT_FN_ (dArr', dArr, arr, p, q)
             val y = _PMERGESORT_FN_ (dArr', dArr, arr, q, r)
	     val xy = x+y
	     val z =pMerge(dArr, dArr', p, q, r)
             in
                xy+z
             end
        else (aupdate(dArr, p, asub(arr, p)); 0)
;
})dnl
_PMERGESORT_({seqMergesort}, {val}, {false}, {seqMergesort})
_PMERGESORT_({pMergesort'}, {dval}, {((r-p) < _SEQ_SZ_)}, {seqMergesort})

   (* parallel merge sort *)
    fun pMergesort (arr) = let
	val dArr = array(alength(arr), asub(arr,0))
        val dArr' = array(alength(arr), asub(arr,0))
	in
	   pMergesort'(dArr, dArr', arr, 0, alength(arr));
	   dArr
        end
;

fun arr2s (elt2s, arr) = let
    val n = alength(arr)
    fun loop (i, str) = if (i >= 0)
        then loop(i-1, elt2s (asub(arr, i))^", "^str)
        else str
    in
        "["^loop(n-1, "")^"]"
    end
;

fun genRandomDoubleArr (n) = let
    val arr : double array = array(n, 0.0:double)
    fun loop (i) = if (i < n)
        then (aupdate(arr, i, drand(0.0:double, 100.0:double)); 
	      loop(i+1))
        else ()
    in
       loop(0);
       arr
    end
;

fun debug () = let
    val n = readint()

    val arr = genRandomDoubleArr(n)
    val arr' = copyArr(arr)

    val b = gettimeofday ()
    val arr = pMergesort(arr)
    val e = gettimeofday ()

    val _ = bubbleSort(arr')
(*    val _ = print (arr2s (dtos, arr')^"\n"); *)
    in
(*        print (arr2s (dtos, arr)^"\n");*)
        print (dtos (e-b)^"\n");
        print (b2s(arrayEq(arr, arr'))^"\n")
    end
;

(* grab the input size x from stdin, generate a random array of length x, 
 * and sort it.
 *)
fun timeTest () = let
    val n = readint()

    val arr = genRandomDoubleArr(n)

ifdef({DEBUG}, {
    val _ = print (arr2s (dtos, arr)^"\n"); 
})dnl

    val b = gettimeofday ()
    val arr = pMergesort(arr)
    val e = gettimeofday ()
    in
ifdef({DEBUG}, {
        print (arr2s (dtos, arr)^"\n");
})dnl
        print (dtos (e-b)^"\n")
    end
;

ifdef({DEBUG}, {
val _ = debug();
})dnl
timeTest()
