datatype tree 
  = LF of long 
  | ND of (tree * tree)

fun mkTree d = 
 (if (d <= 0) then 
    LF 1 
  else let
    val t = mkTree (d-1)
    in
      ND (t, t)
    end)

fun treeMul t = 
 (case t
    of LF n => n
     | ND (t1, t2) =>
        (pcase treeMul t1 & treeMul t2
            of 0 & ? => 0
	     | ? & 0 => 0 
             | m & n => m * n 
	     | otherwise => ~1
	    (* end pcase *))
    (* end case *))

val _ = Print.printLn (Long.toString (treeMul (mkTree 14)))
