datatype tree = LF | ND of (double * tree * tree);

fun mkTree d = if (d <= 0) 
    then LF 
    else ND(1.0, mkTree(d-1), mkTree(d-1));

fun treeAdd t = (case t
       of LF => 0.0
        | ND(x, t1, t2) => let
          pval l = treeAdd(t1)
          val r = treeAdd(t2) + x
          in
	      l + r
          end
      (* end case *));

val b = gettimeofday ();
val x = treeAdd (mkTree (readint()));
val e = gettimeofday ();

print (dtos(e-b) ^ "\n")
