datatype T1 = C1 of int
datatype T2 = C2 of (int * int)
datatype T3 = K31 | K32 | C3 of (int * int)

fun f1 (C1 x) = x

fun f2 (C2 x) = x

fun f3 (C2(x, y)) = x+y

fun f4 (C2(_, y)) = y

