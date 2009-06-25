fun signum (n : int) =
   if (n > 0)
   then 1
   else if (n < 0)
        then ~1
        else 0;
(| signum (1000 : int), signum (~1000 : int) |)

