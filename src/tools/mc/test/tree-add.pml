datatype tree = LF of int | ND of (int * tree * tree)

fun mkTree d = if (d <= 0) then LF 1 else ND(d, mkTree(d-1), mkTree(d-1))

fun treeAdd t = (case t
		  of LF n => 1
		   | ND(d, t1, t2) => (
		     if d < 10 then Print.printLn ("treeAdd at depth "^Int.toString d) else ();
		     treeAdd t1 + treeAdd t2)
		(* end case *))

val _ = (
    Print.printLn "starting treeAdd(20)";
    treeAdd(mkTree 20);
    Print.printLn "finished treeAdd(20)";
    ())
