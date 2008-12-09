datatype tree = LF of int | ND of (int * tree * tree)

fun mkTree totalD = let
  fun f d = if (d <= 0) then LF 1 else ND(totalD-d, f(d-1), f(d-1))
  in
    f totalD
  end

fun treeAdd t = (case t
		  of LF n => 1
		   | ND(d, t1, t2) => treeAdd t1 + treeAdd t2
		(* end case *))

val n = 15

(* test global GC by allocating a large binary tree. we estimate the number of bytes to represent the tree. 
 * - node: 36 bytes
 * - leaf: 20 bytes
 * thus, the size of the tree in bytes is roughly 2^d-1 * 36 + 2^d * 20 bytes
 *)
val _ = (
    Print.printLn "starting treeAdd("^Int.toString n^")";
    let val t = mkTree sz
    in
      Print.printLn "constructed tree";
      Print.printLn("treeAdd()="^Int.toString (treeAdd t));
      ()
    end;
    Print.printLn "finished treeAdd";
    ())
