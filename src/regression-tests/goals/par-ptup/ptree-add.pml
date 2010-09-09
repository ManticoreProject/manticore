datatype tree = LF of long | ND of (tree * tree);

fun mkTree d = if (d <= 0) then LF 1 else ND(mkTree(d-1), mkTree(d-1));

fun add (m : long, n : long) = m + n;

fun treeAdd t = (case t
       of LF n => n
        | ND(t1, t2) => add (| treeAdd t1, treeAdd t2 |)
      (* end case *));

val _ = Print.printLn (Long.toString (treeAdd (mkTree 14)))
