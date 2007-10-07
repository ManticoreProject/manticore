fun prFmt (msg, fmt, x) = print(msg ^ (fmt x) ^ "\n");
val _ = print "a printing test\n";
val _ = prFmt("an integer: ", itos, 42);
val _ = prFmt("a negative integer: ", itos, ~17);
val _ = prFmt("a long: ", ltos, 9999999);
val _ = prFmt("a negative long: ", ltos, ~133);
val _ = prFmt("a float: ", ftos, 0.01);
val _ = prFmt("a negative float: ", ftos, ~3.1415);
val _ = prFmt("a double: ", dtos, 0.01);
val _ = prFmt("a negative double: ", dtos, ~3.1415);
print "done\n"
