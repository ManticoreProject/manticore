datatype tree = LF of long | ND of (tree * tree);

fun treeAdd tr = (case tr
       of LF n => n
	| ND(t1, t2) => treeAdd t1 + treeAdd t2
      (* end case *));

()
