val pr = Print.printLn
val itos = Int.toString

structure FlatFlops = struct

  val (sin, cos, tan, sqrt) = (Double.sin, Double.cos, Double.tan, Double.sqrt)

  fun flops n = let
    val a = sin(2.0*n*n) + cos(7.0*n) / 17.0
    val b = cos(3.0*n*n) + tan(8.0*n) / 18.0
    val c = tan(4.0*n*n) + sqrt(9.0*n) / 12.0
    val d = sqrt(5.0*n*n) + sin(1.1*n) / 27.0
    val e = sin(6.0*n*n) + cos(3.1*n) / 37.0
    in
      a+b+c+d+e
    end
 
  fun go n = [| flops (Double.fromInt i) | i in [| 0 to n-1 |] |]

end

structure Main = struct

  val dfltN = 1000

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
      val ns = FlatFlops.go n
      in
	ignore (itos (PArray.length ns));
        ()
      end
    in
      RunPar.runMicrosec doit
    end

end

fun workaround thunk = ImplicitThread.runOnWorkGroup (WorkStealing.workGroup (), thunk)
val _ = workaround (fn () => Main.main (CommandLine.name (), CommandLine.arguments ()))
