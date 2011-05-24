structure S1 = IntSeq
structure S2 = IntSeq
structure S3 = IntSeq

structure R1 = IntRope
structure R2 = IntRope
structure R3 = IntRope

fun seqMap (f, len, s1, s2) = let
  fun f' i = f (S1.sub (s1, i), S2.sub (s2, i))
  in
    S3.tabulate (len, f')
  end

fun ropeMap f = let
  fun mapF (rope1, rope2) = (case rope1
    of R1.Leaf s1 => (case rope2
         of R2.Leaf s2 => let
              val n = R1.length rope1
	      in
                if (n < 1) then R3.empty ()
		else R3.leaf (seqMap (f, n, s1, s2))
              end)
	  | R1.Cat cat1 => (case cat1
	      of (d1, len1, r1L, r1R) => (case rope2
		   of R2.Cat cat2 => (case cat2
		        of (_, _, r2L, r2R) =>
			     R3.Cat (| d1, len1, mapF (r1L, r2L), mapF (r1R, r2R) |)))))
   in
     mapF
   end

fun getArgs args = let
  fun lp (args, size) = (case args
    of nil => (case size
         of NONE => 100000
          | SOME s => s)
     | r::s::t =>
         if String.same (r, "-size") then
           lp (t, Int.fromString s)
         else
           lp (s::t, size)
     | _::t => lp (t, size))
  in
    lp (args, NONE)
  end

val sz = getArgs (CommandLine.arguments ())

val r1 = R1.tabulate (sz, fn n => n + 20)
val r2 = R2.tabulate (sz, fn n => n + 19)
val r3 = ropeMap (fn (a, b) => a-b) (r1, r2)

val _ = Print.printLn ("expecting " ^ Int.toString sz ^ ": " ^ Int.toString (R3.length r3))
val _ = Print.printLn "done"

