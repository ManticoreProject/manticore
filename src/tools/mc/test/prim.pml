(* test of inline BOM *)
val f = _prim (fun f (x:[int] / exh : cont(any)) : [int] =
	           let v : int = #0(x)
                   let v' : int = I32Add(v, 1000)
                   return (alloc(v')))

val x = f 1
val _ = Print.print (Int.toString x^"\n")
