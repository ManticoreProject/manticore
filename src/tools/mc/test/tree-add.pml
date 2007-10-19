datatype tree = LF of long | ND of (tree * tree);

fun mkTree d = if (d <= 0) then LF 1 else ND(mkTree(d-1), mkTree(d-1));

fun treeAdd t = (case t
       of LF n => 1
        | ND(t1, t2) => treeAdd t1 + treeAdd t2
      (* end case *));

print (ltos(treeAdd (mkTree 12)) ^ "\n")
