
(*
  Basic test of fork-join with CML channels.
*)


fun forkJoin n = let
    val theChannel = PrimChan.new()
    fun sendBackOne () = PrimChan.send (theChannel, 1)

    fun testLoop (n, acc) = (case n
      of 0 => acc
       | n => let
          val _ = spawn (sendBackOne ())
          val ans = PrimChan.recv theChannel
       in
        testLoop (n-1, acc + ans)
       end
      (* end case *))
  in
    testLoop (n, 0)
  end

val num = 5000000
val ans = forkJoin num
val _ = print ("fork-joined " ^ Int.toString ans ^ " times.\n")
