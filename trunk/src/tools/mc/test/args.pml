val args = CommandLine.arguments();

val _ = List.app (fn s => Print.print("arg: \"" ^ s ^ "\"\n")) args

