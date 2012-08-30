(* wrapper.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Wrapper =
struct

    structure Translate = Translate (open PMLFrontEnd.Sxml)

  (* Initialize MLton constants with default values*)
    val _ = PMLFrontEnd.init ()
                       
    fun makeFileDummy f = fn () =>
        {file = f,
         print = fn s:string => (),
         done = fn () => ()}

    fun compileMLB (input: string, asmFile : string) = let
	  val sxml = PMLFrontEnd.compileMLB {input=input}
	  in
	    print "About to translate...\n";
	    Translate.translate sxml
	  end

end
