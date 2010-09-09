val () = Print.printLn "dict"

val k = FLS.newKey 0

val () = FLS.setTopology(Topologies.TREE_TOPOLOGY(TreeTopology.LEAF 0, TreeTopology.TOP))

val () = FLS.setKey(k, 1024)

val Option.SOME x = FLS.getKey k
val () = Print.printLn(Int.toString x)

val k' = FLS.newKey false
val () = FLS.setKey(k', true)
val Option.SOME x = FLS.getKey k'
val () = if x then Print.printLn "true" else Print.printLn "false"

val () = FLS.setKey(k, 1025)
val Option.SOME x = FLS.getKey k
val () = Print.printLn(Int.toString x)

val () = Print.printLn "dict"
