val pr = Print.printLn
val itos = Int.toString

structure NestedFirsts = struct

  fun nestedFirsts i = let
    val rng = [| 1 to i |]
    val nss = [| [| 1 to j |] | j in rng |]
    in
      [| (ns!0) | ns in nss |]
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
      val ns = NestedFirsts.nestedFirsts n
      in
	pr (itos (PArray.length ns));
        ()
      end
    in
      RunPar.runMicrosec doit 
    end

end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
