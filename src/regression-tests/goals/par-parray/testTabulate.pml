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
fun rootU(rp, uc) = (case uc
      of (nil, nil, nil) => (rp)
       | (ls, r :: rs, Left :: ds) => (rootU (testNccat2 (rp, r), (ls, rs, ds)))
       | (l :: ls, rs, Right :: ds) => (rootU (testNccat2 (l, rp), (ls, rs, ds)))
       | _ => (print "Raising exception!\n"; failwith "rootU"))


fun makeLeaf n = Leaf(Seq.fromList[99999999])



val ls = [makeLeaf(), makeLeaf ()]
val rs = [makeLeaf (), makeLeaf ()]
val ds = [Left, Right, Left, Right]

val x = makeLeaf ()

val _ = print "calling rootU\n"
val resASDFJKL = rootU(x, (ls, rs, ds))

val _ = print "done with rootU\n"

val _ = Tabulate.printRope(resASDFJKL, 0)
val _ = print "\n"  
