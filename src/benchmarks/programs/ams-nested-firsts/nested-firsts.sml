val pr = (fn s => print s; print "\n")
val itos = Int.toString

structure NestedFirsts = struct

  fun nestedFirsts i = let
    val rng = List.tabulate (i, fn n => n+1)			    
    val nss = List.map (fn i => List.tabulate (i, fn n => n+1)) rng
    in
      List.map (fn ns => List.nth (ns, 0)) nss
    end

end

structure Main = struct

  val dfltN = 10

  fun getSizeArg args = (case args
    of arg1 :: arg2 :: args =>
         if (arg1 = "-size") then 
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
	print (itos (List.length ns));
	print "\n";
        ()
      end
    val _ = RunSeq.runMicrosec doit
    in
      OS.Process.success
    end

end

