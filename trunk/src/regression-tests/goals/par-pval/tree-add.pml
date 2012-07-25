datatype tree 
  = LF of long 
  | ND of (tree * tree)

fun mkTree d = 
  if (d <= 0) 
  then LF 1 
  else ND(mkTree(d-1), mkTree(d-1))

fun treeAdd t =
 (case t
    of LF n => n
     | ND(t1, t2) => let
         pval sum2 = treeAdd t2
         in
           (treeAdd t1) + sum2
         end)

val _ = Print.printLn (Long.toString (treeAdd (mkTree 14)))

