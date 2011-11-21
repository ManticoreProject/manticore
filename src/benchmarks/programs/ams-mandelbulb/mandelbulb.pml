(* mandelbulb.pml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel version of mandelbulb computation.
 *)

structure Mandelbulb = struct

  fun add ((x1:double, y1:double, z1:double), (x2, y2, z2)) = (x1+x2, y1+y2, z1+z2)

  fun scale (a:double, (x, y, z)) = (a*x, a*y, a*z)

  fun magnitude (x, y, z) = Double.sqrt (x*x + y*y + z*z)

(* http://www.skytopia.com/project/fractal/2mandelbulb.html#formula *)
  fun pow n (x, y, z) = let
    val n' = Double.fromInt n
    val r = magnitude (x, y, z)
    val phi = Double.atan2 (y, x)
    val theta = Double.atan2 (Double.sqrt (x*x + y*y), z)
    val nPhi = n' * phi
    val nTheta = n' * theta
    val v = (Double.sin nTheta * Double.cos nPhi,
	     Double.sin nTheta * Double.sin nPhi,
	     Double.cos nTheta)
    in
      scale (Double.pow (r, n'), v)
    end

  val lim = 1000

  fun iter n c = let
    val pow' = pow n
    fun lp (i, z) = 
      if (i >= lim) then lim
      else if magnitude z >= 2.0 then i
      else lp (i+1, add (pow' z, c))	
    in
      lp (0, c)
    end

  val xbase = ~2.0
  val ybase = ~2.0
  val zbase = ~2.0
  val side = 4.0

  val power = 8;

  fun mandelbulb n = let
    val iter' = iter power
    fun elt (i, j, k) = let
      val delta = side / Double.fromInt (n-1)
      val cx = xbase + (delta * Double.fromInt k)
      val cy = ybase + (delta * Double.fromInt j)
      val cz = zbase + (delta * Double.fromInt i)
      in
        iter' (cx, cy, cz)
      end
    val range = [| 0 to n-1 |]
    in
      [| [| [| elt (i,j,k) | k in range |] | j in range |] | i in range |]
    end

end

structure Main = struct

  val dfltN = 64

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
			
  fun main (_, args) = let
    val (chatty, n) = getArgs args
    fun doit () = Mandelbulb.mandelbulb n
    val counts = RunPar.runMicrosec doit
    val _ = if chatty then Print.printLn (PArray.tos_intParrParr counts)
            else ()
    in
      counts
    end

end

fun workaround thunk = ImplicitThread.runOnWorkGroup (WorkStealing.workGroup (), thunk)
val _ = workaround (fn () => Main.main (CommandLine.name (), CommandLine.arguments ()))
