val letters = [| "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P",
                 "Q","R","S","T","U","V","W","X","Y","Z" |];

fun circlePlus (s1, s2) = (s2 ^ s1);

val alphabet = reduceP (circlePlus, "", letters);

print (alphabet ^ "\n")
