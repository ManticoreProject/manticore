type vector = int parray;

type sparse_vector = (int * int) parray;

fun dotp (sv, v) = sumP [| x * (v!i) | (i,x) in sv |];

val sv0 = [| (0, 1), (5, 1) |];

val v0  = [| 1, 1, 1, 1, 1, 1 |];

val dotp0 = dotp (sv0, v0);

val _ = print ("Testing dotp: expecting 2 => " ^ (itos dotp0) ^ "\n");

type sparse_matrix = sparse_vector parray;

fun smvm (sm, v) = [| dotp (row, v) | row in sm |];

val sm0 = [| sv0, sv0 |];

val smvm0 = smvm (sm0, v0);

fun vtos v =
   let val n = plen v
       fun build (m, acc) =
         if (m >= n) then
           acc
         else if (m = (n - 1)) then 
           build (m+1, acc ^ (itos (v!m)))
	 else
	   build (m+1, acc ^ (itos (v!m)) ^ ",")
   in
     "[|" ^ (build (0, "")) ^ "|]"
   end;

val _ = print ("Testing smvm: expecting [|2,2|] => " ^ (vtos smvm0) ^ "\n");

val _ = print "Done.\n"

