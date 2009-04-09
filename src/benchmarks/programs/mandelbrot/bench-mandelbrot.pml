val (_, t) = Time.toEval (fn () => Mandelbrot.mandelbrot (PrimIO.readInt ()))
val () = Print.printLn(Long.toString t);
