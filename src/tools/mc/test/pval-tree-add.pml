datatype tree = EMPTY | NODE of (int * tree * tree);

fun treeAdd t = (case t
    of EMPTY => 0
     | NODE (i, l, r) => let
       pval x = treeAdd(l)
       pval y = treeAdd(r) + i
       in
          x + y
       end
    (* end case *));

treeAdd (NODE(1, EMPTY, EMPTY))
