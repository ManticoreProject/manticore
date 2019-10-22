
structure Fib = struct

  val pi = 3.14159265358979323846
  val epsilon = 0.001

  fun one () = Double.tan (pi / 4.0)
  fun two () = one () + one ()
  fun zero () = Double.sin pi
  fun eq (x, y) = Double.abs(x - y) < epsilon

  fun go n =
    if eq (n, 0.0)
      then zero()
    else if eq (n, 1.0)
      then one()
    else  go (n-one()) + go (n-two())

end (* end struct *)


structure Main =
  struct

  fun run () = let
      val n = 32.0
      val correct = 2178309.0
      val result = Fib.go n
    in
      (print (Double.toString result);
      if not (Fib.eq (correct, result))
                    then print "\nNOT close!\n"
                    else print "\nwithin bounds.\n")
    end

end

val _ = Main.run()
