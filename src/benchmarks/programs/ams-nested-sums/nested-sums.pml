val pr = Print.printLn
val itos = Int.toString

structure NestedSums = struct

  fun sumP (xs : int parray) : int = PArray.reduce (fn (x:int, y:int) => x + y) 0 xs

  fun spoof_sumP (xs : int parray) : int = 12345

  fun nestedSums i = let
    val _ = pr ("nestedSums " ^ itos i)
    val rng = [| 1 to i |]
    val _ = pr ("nestedSums: built rng")
    val nss = [| [| 1 to j |] | j in rng |]
    val _ = pr ("nestedSums: built nss")
    in
      [| sumP ns | ns in nss |]
    end

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
	
  fun main (_, args) = let
    val n = (case getSizeArg args 
      of SOME n => n 
       | NONE => dfltN
      (* end case *))
    fun doit () = let
      val ns = NestedSums.nestedSums n
      in
	pr (itos (PArray.length ns));
        ()
      end
    in
      RunPar.runMicrosec doit 
    end

end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
