fun parrString a =
  let val len = plen a
      fun build (curr, acc) =
        if curr=len
        then rev acc
        else build (curr+1, (itos (a!curr)) :: acc)
  in
      "[" ^ (concatWith (",", build (0, nil))) ^ "]"
  end
;

(* assuming b>a *)
fun binarySearch (arr, a, b, x) = if (b = a)
    then if x = arr!a
	    then SOME a
	    else NONE)
    else let
      val p = (b+a) div 2
      val (a, b) = if arr!p < x
		      then (p+1, b)
		      else (a,   p)
      in
	  binarySearch(arr, a, b, x)
      end
;

fun valOf opt = (case opt
		  of NONE => fail "option"
		   | SOME v => v)
;

val arr = [| 1, 3, 10, 15, 100 |];
val pOpts = [| valOf (binarySearch(arr, 0, plen arr-1, i)) | i in arr |];

()
