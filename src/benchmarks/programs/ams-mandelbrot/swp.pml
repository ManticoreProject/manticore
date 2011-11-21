(* mandelbrot.pml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel version of mandelbrot-set computation. The output goes to out.ppm.
 *)

structure Mandelbrot =
  struct

    val xBase = ~2.0
    val yBase = 1.25
    val side = 2.5

    val maxCount = 1000

    fun pix2rgb cnt = 
	if cnt >= maxCount then
	    (0.0, 0.0, 0.0)
	else let
		val w = Float.fromInt cnt / (Float.fromInt (maxCount-1))
	    in
		(w, w, 0.25 + w*0.75)
	    end

    fun mandelbrot N = let
	  fun elt (i, j) = let
	        val delta = side / (Float.fromInt (N-1))
		val c_re = xBase + (delta * Float.fromInt j)
		val c_im = yBase - (delta * Float.fromInt i)
		fun CORE_INNER_LOOP_SWP (cnt, z_re, z_im) = 
		    if (cnt < maxCount)
		      then let
			val z_re_sq = z_re * z_re
			val z_im_sq = z_im * z_im
			in
			  if ((z_re_sq + z_im_sq) >= 4.0)
			    then cnt
			    else let
			      val z_re_im = z_re * z_im
			      in
				CORE_INNER_LOOP_SWP(cnt+1, (z_re_sq - z_im_sq) + c_re, z_re_im + z_re_im + c_im)
			      end
			end
		      else cnt
		in
		  CORE_INNER_LOOP_SWP (0, c_re, c_im)
		end
	  val pixels = Rope.tabulate (N, fn i => Rope.tabulate (N, fn j => elt (i, j)))
	  in
            pixels
	  end

  end

structure Main =
  struct

  val dfltN = 256

  fun getArgs args = let
    fun lp (args, chatty, size) = (case args
      of s::ss =>
           if String.same (s, "-v") then
             lp (ss, true, size)
           else if String.same (s, "-size") then (case ss
             of s'::ss' => lp (ss', chatty, Int.fromString s')
              | nil => lp ([], chatty, SOME dfltN)
             (* end case *))
           else (* breeze past other options; could be used elsewhere *)
             lp (ss, chatty, size)
       | nil => (case size
           of NONE => (chatty, dfltN)
            | SOME sz => (chatty, sz)
           (* end case *))
      (* end case *))
    in
      lp (args, false, NONE)
    end

  fun tellMeAbout (nss: int Rope.rope Rope.rope) = let
    val itos = Int.toString
    fun row ns = let
      val len = Rope.length ns
      fun tos i = itos (Rope.sub (ns, i))
      fun lp i = 
        if i=(len-1) then (tos i ^ "|]")
        else let
          val s = (tos i ^ ",")
          in
            s ^ (lp (i+1))
          end
       in
         lp 0
       end
    fun rows nss = let
      val len = Rope.length nss
      fun tos i = row (Rope.sub (nss, i))
      fun lp i =
        if i=(len-1) then (tos i ^ "|]")
        else let
          val s = tos i ^ ",\n"
          in
            s ^ lp (i+1)
          end
       in
         lp 0
       end
    in
      Print.printLn ("[|" ^ (rows nss))
    end
			
  fun main (_, args) = let
    val (chatty, n) = getArgs args
    fun doit () = Mandelbrot.mandelbrot n
    val counts = RunPar.runMicrosec doit
    val _ = if chatty then tellMeAbout counts else ()
    in
      counts
    end

(*
    val dfltN = 1024

    fun getSizeArg args =
	(case args
	  of arg1 :: arg2 :: args =>
	     if String.same (arg1, "-size") then Int.fromString arg2
	     else getSizeArg (arg2 :: args)
	   | _ => NONE
	(* end case *))
	
    fun main (_, args) =
	let
	    val n = (case getSizeArg args of NONE => dfltN | SOME n => n)
	    fun doit () = Mandelbrot.mandelbrot n
	    val image = RunPar.runMicrosec doit
	in
	    ()
	end
*)

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
