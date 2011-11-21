(* smvm.pml
 * 
 * Sparse matrix vector multiplication.
 * Common example in the literature.
 *) 

structure SMVM = struct

  fun add (x:double, y:double) = x+y

  fun sum a = PArray.reduce add 0.0 a

  fun dotp (sv, v) = sum [| x * (v!i) | (i, x) in sv |]

  fun smvm (sm, v) = [| dotp (sv, v) | sv in sm |]

end

structure Main = struct

  fun qs (ns : long list) = (case ns
    of nil => ns
     | q::nil => ns
     | p::ns => let
         val (ls, gs) = List.partition (fn n => n <= p) ns
         in
           (qs ls) @ (p :: (qs gs))
         end
    (* end case *))

  fun med (ns : long list) = let
    val sorted = qs ns
    val len = List.length sorted
    in
      List.nth (sorted, len div 2)
    end

(*  
  val epsilon = 0.0000000001
  fun bumpSV1 sv = PArray.map (fn (i,x) => (i+1, x+epsilon)) sv
  fun bumpSV2 sv = PArray.map (fn (i,x) => (i-1, x-epsilon)) sv
  fun bumpSM sm = PArray.map (fn sv => bumpSV2 (bumpSV1 sv)) sm
*)	
  
  fun rnd () = Rand.randDouble (0.0, 1.0)
  
  fun imul (n:int, m:int) = m*n
  fun copies n x = List.tabulate (n, fn _ => x)
  val prod = foldl imul 1
  fun tenToThe n = prod (copies n 10)

  val lim = 1000000
  val sparsity = 5000
  val times = 1

  fun csv ss = Print.printLn (String.concatWith "," ss)

  fun loc n = Print.printLn ("at location " ^ Int.toString n)

  fun main (_, args) = let
    val time = Stopwatch.time
    fun mksv (u, g) = [| (i, rnd ()) | i in [| 0 to u by g |] |]
    fun mkv u = [| rnd () | _ in [| 0 to u |] |]
    fun go (n, svTimes, vTimes, dotpTimes, smvmTimes) = 
      if (n <= 0) then let
        val itos = Int.toString
        val tos = Long.toString
        val svMed = med svTimes
        val vMed = med vTimes
        val dotpMed = med dotpTimes
	val smvmMed = med smvmTimes
        in
          csv [itos lim, itos (lim div sparsity), tos svMed, tos vMed, tos dotpMed, tos smvmMed]   
        end
      else let
        val (testsv, t1) = time (fn () => mksv (lim, sparsity))
        val (testv, t2) = time (fn () => mkv lim)
	val (p, t3) = time (fn () => SMVM.dotp (testsv, testv))
        val sm = [| mksv (lim, sparsity) | _ in [| 1 to 100000 |] |]
        val (xs, t4) = time (fn () => SMVM.smvm (sm, testv))
        in
	  (* Print.printLn ("iteration " ^ Int.toString n); *)
	  (* Print.printLn ("time to build testsv (sparse vector of length " ^ *)
	  (* 		 Int.toString (lim div 17) ^ "): " ^ *)
	  (* 		 Long.toString t1); *)
	  (* Print.printLn ("time to build testv (vector of " ^ Int.toString lim ^ *)
	  (* 		 ") random doubles: " ^ *)
	  (* 		 Long.toString t2); *)
	  (* Print.printLn ("time to compute dot product: " ^ Long.toString t3); *)
	  (* Print.printLn ("dotp: " ^ Double.toString p); *)
	  go (n-1, t1::svTimes, t2::vTimes, t3::dotpTimes, t4::smvmTimes)
	end
    in
      (* Print.printLn (Int.toString times ^ " runs conducted"); *)
      (* Print.printLn "testsv median time,testv median time,dotp median time"; *)
      RunPar.runMicrosec (fn _ => go (times, nil, nil, nil, nil))
    end

end

fun workaround thunk = ImplicitThread.runOnWorkGroup (WorkStealing.workGroup (), thunk)

val _ = let
  val name = CommandLine.name ()
  val args = CommandLine.arguments ()
  fun doit () = Main.main (name, args)
  in
    workaround doit
  end
