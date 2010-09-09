_primcode (

define @f (x : int) : int = return (I32Add(x,1));
define @g (x : int) : int = return (I32Sub(x,1));

@f (@g (x)) ==> @g (@f (x));

define @test-rewrite (x : ml_int / exh : exh) : ml_int =
    let x1 : int = @f (#0(x))
    let x2 : int = @g (x1)
    return (alloc (x2))
  ;

)

val t : int -> int = _prim (@test-rewrite)
val _ = Print.printLn (Int.toString (t (PrimIO.readInt ())))
