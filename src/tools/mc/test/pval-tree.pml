datatype t 
  = Leaf of int
  | Node of t * t;

fun prod_ptup t = case t
  of Leaf n => n
   | Node (tL, tR) => let
       val (pL, pR) = (| prod_ptup tL, prod_ptup tR |)
       in
         pL * pR
       end;

fun prod_pval t = case t
  of Leaf n => n
   | Node (tL, tR) => let
       pval pR = prod_pval tR
       val pL = prod_pval tL
       in
         if (pL = 0) then 0
         else (pL * pR)
       end;

fun build1Tree depth =
  if depth = 0 then
    Leaf 1
  else
    Node (build1Tree (depth-1), build1Tree (depth-1));

fun buildLopsidedTree depth =
  if depth = 0 then
    Leaf 0
  else
    Node (Leaf 0, build1Tree depth);

val T = buildLopsidedTree 20;

val t0 = gettimeofday();
val p_ptup = prod_ptup T;
val t1 = gettimeofday();

val t2 = gettimeofday();
val p_pval = prod_pval T;
val t3 = gettimeofday();

val ptupTime = t1-t0;
val pvalTime = t3-t2;

(print "Time for ptup product: ";
 print (dtos ptupTime);
 print "\n";
 print "Time for pval product: ";
 print (dtos pvalTime);
 print "\n")

