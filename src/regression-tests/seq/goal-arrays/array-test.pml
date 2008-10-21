structure A = Array64
val arr = A.array(1000, 0.0)
val _ = A.update(arr, 50, 3.14)
val x : double = A.sub(arr, 50)

val arr2 = A.array(1000, 0)
val _ = A.update(arr2, 50, 3)
val y : int = A.sub(arr2, 50)

val _ = Print.printLn(Double.toString x^Int.toString y)
