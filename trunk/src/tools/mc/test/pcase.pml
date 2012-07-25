val x = (pcase 1 & 2
          of ? & 3 => "nope"
           | 4 & ? => "uh-uh"
           | otherwise => "test succeeded")

val _ = (print x; print "\n")


         
