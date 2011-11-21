structure Main =
  struct

    val dfltN = 10000000

    val rand = Random.rand (0, 1000000)

    fun output 0 = ()
      | output i = (
	TextIO.print (Int.toString (Random.randNat rand)^"\n");
	output (i-1))

    fun main (_, n :: _) =
	let
	    val n = Option.getOpt (Int.fromString n, dfltN)
	in
	    output n;
	    OS.Process.success
	end


  end
