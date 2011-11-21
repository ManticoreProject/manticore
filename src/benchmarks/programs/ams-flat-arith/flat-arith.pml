val pr = Print.printLn
val itos = Int.toString

structure AlreadyFlat = struct

  fun arith n = let
    val a = (10*n*n + 7*n + 17) mod 100
    val b = (91*n*n + 8*n + 18) mod 100
    val c = (11*n*n + 9*n + 12) mod 100
    val d = (21*n*n + 2*n + 27) mod 100
    val e = (33*n*n + 3*n + 37) mod 100
    in
      a+b+c+d+e
    end
 
  fun go n = [| arith i | i in [| 0 to n-1 |] |]

end

structure Main = struct

  val dfltN = 10

  fun getSizeArg args = (case args
    of arg1 :: arg2 :: args =>
         if String.same (arg1, "-size") then 
	     Int.fromString arg2
	 else 
	     getSizeArg (arg2 :: args)
     | _ => NONE
    (* end case *))
	
  fun ignore x = ()

  fun main (_, args) = let
    val n = (case getSizeArg args 
      of SOME n => n 
       | NONE => dfltN
      (* end case *))
    fun doit () = let
      val ns = AlreadyFlat.go n
      in
	ignore (itos (PArray.length ns));
        ()
      end
    in
      RunPar.runMicrosec doit 
    end

end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
