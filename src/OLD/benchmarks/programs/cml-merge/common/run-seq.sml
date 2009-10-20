structure RunSeq (* sig
  val run : (unit -> unit) -> unit
  end *) = struct

    fun run f =
	let
	    val t0 = Time.now ()
	    val _ = f ()
	    val t = Time.-(Time.now(), t0)
	in
	    print (Time.toString t^"\n");
	    ()
	end

  end
