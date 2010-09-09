(* test of list filter *)

fun intListToString ns = ("[" ^ String.concatWith "," (List.map Int.toString ns) ^ "]")

fun positive n = (n>0)

val nums = (0::1::0::2::0::3::0::4::0::5::0::~1::6::~2::nil)

fun rev' xs = let
      fun loop (xs, ys) = (case xs
            of nil => ys
	     | x :: xs => loop(xs, x :: ys)
            (* end case *))
      in
          loop (xs, nil)
      end

fun filter' (pred, xs) = let
      fun loop (xs, ys) = (case xs
            of nil => rev' ys
	     | x :: xs => loop(xs, if pred x then x :: ys else ys)
            (* end case *))
      in
        loop (xs, nil)
      end

val filteredNums = filter' (positive, nums)

val _ = Print.printLn (intListToString filteredNums)
