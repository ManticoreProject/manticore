datatype t = T;
val x : t = T;
fun f x = let exception X of t in if f x then raise X T else f x end;
f x
