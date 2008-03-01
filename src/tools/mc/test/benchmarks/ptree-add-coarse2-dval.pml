(* same as coarse parallel tree add, but does the add twice to measure the overhead of promoting the tree *)
datatype tree = LF | ND of (int * double * tree * tree);

fun mkTree d = if (d <= 0) 
    then LF 
    else ND(d, 1.0, mkTree(d-1), mkTree(d-1));

val d = readint();
val leafSize = 4;

fun seqTreeAdd (t) = (case t
    of LF => 0.0
     | ND(_, x, t1, t2) => seqTreeAdd(t1) + seqTreeAdd(t2) + x
    (* end case *));

fun treeAdd t = (case t
       of ND(d', x, t1, t2) => 
	  if (d' > (d-leafSize)) 
             then let
		  pval l = treeAdd(t1)
                  val r = treeAdd(t2) + x
                  in
	             l + r
                  end
	     else seqTreeAdd(t1) + seqTreeAdd(t2) + x
	| LF => 0.0
      (* end case *));

val t = mkTree(d);
val x = treeAdd (t);
val b = gettimeofday ();
val y = treeAdd (t);
val e = gettimeofday ();

print (dtos(e-b) ^ "\n")
