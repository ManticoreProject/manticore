structure A = Array

val sub = A.sub

val arr = A.array(1000, 0.0)
val _   = A.update(arr, 50, 3.14)
val x_unique_tag : double = sub(arr, 50)

val arr2 = A.array(1000, 0)
val _ = A.update(arr2, 50, 3)
val y_unique_tag : int = sub(arr2, 50)

val _ = Print.printLn(Double.toString x_unique_tag ^ Int.toString y_unique_tag)
