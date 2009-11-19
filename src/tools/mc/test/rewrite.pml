_primcode (

define @f (x : int) : int = return (I32Add(x,1));
define @g (x : int) : int = return (I32Sub(x,1));

@f (@g (x)) ==> @g (@f (x));

define @test-rewrite (x : ml_int / exh : exh) : ml_int =
    let x1 : int = @g (#0(x))
    let x2 : int = @f (x1)
    return (alloc (x2))
  ;

define @myinc (x : ml_int / exh : exh) : ml_int =
    return (alloc (I32Add(#0(x), 1)))
  ;

define @mydec (x : ml_int / exh : exh) : ml_int =
    return (alloc (I32Sub(#0(x), 1)))
  ;

@myinc (@mydec (x)) ==> x { 1 };
@mydec (@myinc (x)) ==> x { 1 };

)

val i : int = PrimIO.readInt ()
val t : int -> int = _prim (@test-rewrite)
val _ = Print.printLn (Int.toString (t (i)))

val inc : int -> int = _prim (@myinc)
val dec : int -> int = _prim (@mydec)
val _ = Print.printLn (Int.toString (inc (dec (i))))
