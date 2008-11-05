_primcode(
  define @f(x : any / exh : exh) : any =
    let v : any = ArrayLoadI64(x, 0)
    return(v)
  ;
)

val f : 'a Array64.array -> 'a = _prim(@f)
val g = f
val x = Print.print(Int.toString (g (Array64.array(1, 0))))
