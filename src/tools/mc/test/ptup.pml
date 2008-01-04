fun signum (n : int) = (print "hi\n";
    if (n > 0) then 1 
    else if (n < 0) then ~1
    else 0);

val _ = print "signum\n";
let val (x,y) = (| signum 1000, signum (~1000) |) 
in
    print ((itos x)^(itos y)^"\n")
end
