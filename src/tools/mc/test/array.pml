structure A = Array

val n = 1048575 div 8
val a = A.array (n, 0:long)
fun lpz i = 
  if i < 0 then ()
  else (A.update(a, i, 2); lpz (i - 1))
val _ = lpz (n-1)
fun sm i = 
  if i < 0 then 0
  else (A.sub(a, i) + sm (i - 1))
val x = sm (n-1)
val _ = Print.printLn(Long.toString x)
