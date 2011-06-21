structure A = UnsafeIntArray

val n = 1048575 div 8
val a = A.create n
fun lp i = 
  if i < 0 then ()
  else (A.update(a, i, 2); lp (i - 1))
val _ = lp (n-1)
fun sm i = 
  if i < 0 then 0
  else (A.sub(a, i) + sm (i - 1))
val x = sm (n-1)
val _ = Print.printLn(Int.toString x)
