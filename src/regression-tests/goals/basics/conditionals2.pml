(* adapted from the Larceny benchmark *)

structure Benchmark = struct

(* TAKL -- The TAKeuchi function using lists as counters. *)

fun list_n n =
  let fun loop (n, a) =
        if n = 0
          then a
          else loop (n-1, n::a)
  in loop (n, [])
  end

val l18 = list_n 18
val l12 = list_n 12
val l6  = list_n  6

(* Part of the fun of this benchmark is seeing how well the compiler
   can understand this ridiculous code, which dates back to the original
   Common Lisp.  So it probably isn't a good idea to improve upon it. *)

(* To be compatible with Manticore, I've had to change two expressions below,
   from (y = []) to (List.null y). This perhaps makes it even more ridiculous :)
      ~kavon
*)

fun shorterp (x, y) =
  (not (List.null y))
  andalso ((List.null x) orelse (shorterp (List.tl x, List.tl y)))

fun mas (x, y, z) =
  if not (shorterp (y, x))
    then z
    else mas (mas (List.tl x, y, z),
              mas (List.tl y, z, x),
              mas (List.tl z, x, y))



fun go _ = mas (l18, l12, l6)

end


structure Main =
  struct

  	val iterations = 5

    fun main _ = let

      fun doit () = Benchmark.go ()

      fun lp 0 = ()
      	| lp n = (doit(); lp (n-1))

      fun start () = lp iterations

  	in
      	start()
  	end

end

val _ = Main.main ()
