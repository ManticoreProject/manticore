
val longTests = [
    1 + (2 : long),
    2 - (1 : long),
    10 * (5 : long),
    10 div (2 : long),
    10 mod (4 : long)
]

val intTests = [
    1 + 2,
    2 - 1,
    10 * 5,
    10 div 2,
    10 mod 4
]

val algTests = [
    fn x => x + 0,
    fn x => x * 0,
    fn x => x * 1,
    fn x => x - x,
    fn x => x - 0,
    fn x => 0 - x,
    fn x => 0 div x,
    fn x => x div 1
]

val algTests2 = [
    fn (x : long) => x + 0,
    fn (x : long) => x * 0,
    fn (x : long) => x * 1,
    fn (x : long) => x - x,
    fn (x : long) => x - 0,
    fn (x : long) => 0 - x,
    fn (x : long) => 0 div x,
    fn (x : long) => x div 1
]

fun output toStr v = 
    (Print.print(toStr v) ; Print.print "\n")

fun doAlgTests toStr tests xs = let
    fun print items = 
        (Print.print "--\n" ;
         List.app (output toStr) items)
    
    fun doTest f = List.map f xs
    
    val results = List.map doTest tests
in
    List.app print results
end


val _ = Print.print ("intTests\n")
val _ = List.app (output Int.toString) intTests

val _ = Print.print ("algTests\n")
val _ = doAlgTests Int.toString algTests intTests

val _ = Print.print ("longTests\n")
val _ = List.app (output Long.toString) longTests

val _ = Print.print ("algTests2\n")
val _ = doAlgTests Long.toString algTests2 longTests
