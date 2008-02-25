datatype tree = LF of int | ND of (tree * tree);

fun mkTree d = if (d <= 0) then LF 1 else ND(mkTree(d-1), mkTree(d-1));

fun add (m : int, n : int) = m + n;

fun treeAdd t = (case t
       of LF n => n
        | ND(t1, t2) => add (| treeAdd t1, treeAdd t2 |)
      (* end case *));

print (itos(treeAdd (mkTree (readint()))) ^ "\n")
