(* test of list filter *)

fun intListToString ns = ("[" ^ concatWith(",",map(itos, ns)) ^ "]");

fun positive (n:int) = (n>0);

val nums = (0::1::0::2::0::3::0::4::0::5::0::~1::6::~2::nil);

val filteredNums = filter (positive, nums);

val _ = (print (intListToString filteredNums);
	      print "\n")

