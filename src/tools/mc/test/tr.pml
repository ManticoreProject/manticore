(*val () = Print.printLn "Rope test: enter an integer followed by an EOF"
val () = TestRopes.t (PrimIO.readInt())
*)
val () = Print.printLn "Rope test: enter an integer followed by an EOF"
val () = TestRopes.t' (PrimIO.readInt())
val () = Print.printLn "Rope test complete"
