structure Arr = Array64
fun alloc n = Arr.array(n, 0)
(* increment each element of the array *)
fun inc arr = let
      val n = Arr.length arr
      fun f i = if i < 0 then () else (Arr.update(arr, i, Arr.sub(arr, i)+1); f(i-1))
      in
        f(n-1)
      end
(* increment each element of the array n times *)
fun incn (n, arr, x) = let
      fun f i = if i < 0 then () else (inc arr; f(i-1))
      in
        f n
      end
fun sum (n, arr) = let
      fun f (i, s) = if i < 0 then s else f(i-1, Arr.sub(arr, i)+s)
      in
        f(n-1, 0)
      end

val n = 1000
(*262144*)

val () = (
    Print.printLn "starting global GC test";
    let val arr = alloc n
    in
	Print.printLn "incrementing each element of the array";
	incn(n*10, arr, 1024);
	Print.print "summing the array elements=";
	Print.printLn(Int.toString(sum(n, arr)));
	()
    end;
    Print.printLn "finished global GC test";
    ())
