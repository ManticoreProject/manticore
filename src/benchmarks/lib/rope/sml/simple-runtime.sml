structure SimpleRuntime = struct
fun hungryProcs () = false
fun numProcs () = 1
fun par2 (f, g) = (f (), g ())
fun parN fs = List.map (fn f => f ()) fs
end
