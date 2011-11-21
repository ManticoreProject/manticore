(* generates a list of 2d points inside the unit square uniformly at random *)
structure Main =
  struct

    val dfltN = 500000

    val rand = Random.rand (0, 10000)

    fun xrand () = if Random.randNat rand mod 2 = 0 then ~1.0 * Random.randReal rand else Random.randReal rand

    fun output 0 = ()
      | output i = (
	print (Real.toString (xrand ())^" "^Real.toString (xrand ())^"\n");
	output (i-1))

    fun main (_, n :: _) =
	let
	    val n = Option.getOpt (Int.fromString n, dfltN)
	in
	    output n;
	    OS.Process.success
	end
	
  end
