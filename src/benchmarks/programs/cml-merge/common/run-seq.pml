structure RunSeq (* sig
  val run : (unit -> unit) -> unit
  end *) = struct

    fun run f =
	let
	    val b = Time.now ()
	    val _ = f ()
	    val e = Time.now ()
	in
	    Print.printLn (Time.toString (e - b));
	    ()
	end

  end
