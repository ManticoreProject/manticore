_primcode(
  define @f(x : [![any],int] / exh : exh) : any =
    let data : ![any] = #0(x)
    let v : any = ArrLoad(data, 0)
    return(v)
  ;
)

val f : 'a Array.array -> 'a = _prim(@f)
val g = f
val x = Print.print(Int.toString (g (Array.array(1, 0))))
