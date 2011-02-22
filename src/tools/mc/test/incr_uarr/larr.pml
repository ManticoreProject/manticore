(* FIXME! this benchmark exhibits poor performance. the issue seems to
be that closure conversion does bad things to the modify function. in
particular, it places values like the array offset and the value to
store in the array into the environment pointer that we pass along to
the block generated for the "then" arm of the if statement. -- mike *)

fun update (orig, round) = orig + round

fun incrArray (arr : LongArray.array) round =
  LongArray.modify (fn x => update (x, round)) arr 

fun main () =
  (case CommandLine.arguments () of
      nrounds :: len :: nil =>
      let val SOME nrounds = Long.fromString nrounds
          val SOME len = Int.fromString len	
          val arr = LongArray.array (len, 0:long)
	  fun loop k = 
	      if k = 0 then ()
	      else (
                  incrArray arr (nrounds - k) ;
                  loop (k - 1)
                )
        in loop nrounds end)

val _ = main ()
