structure A = Array64

val sub = A.sub

val arr = A.array(1000, 0.0)
val _   = A.update(arr, 50, 3.14)
val x : double = sub(arr, 50)

val arr2 = A.array(1000, 0)
val _ = A.update(arr2, 50, 3)
val y : int = sub(arr2, 50)

val _ = Print.printLn(Double.toString x ^ Int.toString y)
