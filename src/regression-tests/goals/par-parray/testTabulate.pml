fun fib i = if i <= 2
            then 1
            else fib(i - 1) + fib(i-2)
            
fun failwith s = raise Fail(s)
datatype dir = datatype Tabulate.dir
datatype rope = datatype RopeTy.rope

fun testLength rp = (case rp
  of Leaf s => Seq.length s
   | Cat (l, _, _, _) => l)

fun testDepth rp = (case rp
  of Leaf _ => 0
   | Cat (_, d, _, _) => d)

(* non-coalescing rope concatenation *)
fun testNccat2 (rp1, rp2) = let
  val l = testLength rp1 + testLength rp2
  val d = Int.max (testDepth rp1, testDepth rp2) + 1
  in
    Cat (l, d, rp1, rp2)
  end

(*for some reason, removing the "print test\n" causes this to crash.*)  
fun rootU(rp, uc) = (print "test\n"; case uc
      of (nil, nil, nil) => (rp)
       | (ls, r :: rs, Left :: ds) => ( rootU (testNccat2 (rp, r), (ls, rs, ds)))
       | (l :: ls, rs, Right :: ds) =>(rootU (testNccat2 (l, rp), (ls, rs, ds)))
       | _ => failwith "rootU")


fun makeLeaf n = Leaf (Seq.tabulate(10, fn i => i))

val ls = List.tabulate(3, fn _ => makeLeaf 10)
val rs = List.tabulate(3, fn _ => makeLeaf 10)
val ds = [Left, Left, Right, Left, Right, Right]

val _ = List.app (fn l => Tabulate.printRope l 0) (ls @ rs)

val _ = fib 20        

val _ = print "calling rootU\n"
val res = rootU(makeLeaf 10, (ls, rs, ds))

val _ = print "done with rootU\n"

val _ = Tabulate.printRope res 0
val _ = print "\n"  
