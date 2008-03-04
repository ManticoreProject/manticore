fun loop (arr, i) =
    if (i < alength(arr))
       then (aupdate(arr, i, i);
	     loop(arr, i+1))
       else ()
;

fun arr2ls (arr) = let
    fun loop (i, ls) =
	if (i >= 0)
	   then loop(i-1, asub(arr,i) :: ls)
	   else ls
    in
       loop(alength(arr)-1, nil)
    end
;

val arr = array(10, 0);
val len = alength(arr);
val _ = loop(arr, 0);

val arrLs : int list = arr2ls(arr);
val istrs : string list = map (itos, arrLs); 
val arrS = concatWith(",",istrs);

print ("array ex: "^itos len^" ["^arrS^"]\n")
