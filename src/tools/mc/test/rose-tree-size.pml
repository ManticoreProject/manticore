(* Computing the size of a rose tree. *)

datatype 'a rose_tree 
  = Node of 'a * ('a rose_tree list);

fun add (m:int, n) = m+n;

fun sum ns = foldl (add, 0, ns);

fun sizeOf t = case t
  of Node (_, ts) => 1 + sum (map (sizeOf, ts));

()
