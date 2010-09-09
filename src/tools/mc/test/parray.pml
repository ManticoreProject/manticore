val a = [| "This", "is", "a", "self-describing", "parallel", "array", "of", "strings." |];

val _ = Print.print (Int.toString (plen a))
