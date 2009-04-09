(* mandelbrot.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Compute the Mandelbrot set in parallel.
 *)

structure Mandelbrot =
  struct

    fun x i = x0 + dx * Float.fromInt i
    fun y j = y0 - dy * Float.fromInt j

    fun loop (cnt, re, im) = 
	if (cnt < 255) andalso (re*re + im*im > 4.0) 
	then loop(cnt+1, re*re - re*im + re, 2.0*re*im + im) 
	else cnt

    fun mandelbrot N =
	[| 
	 [| loop(0, x i, y j) | i in [| 0 to N |] |] 
	 | j in [| 0 to N |] 
	|] 

  end
