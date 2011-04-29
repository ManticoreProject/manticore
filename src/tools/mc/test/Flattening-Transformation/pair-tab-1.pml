val ln = Print.printLn
val cat = String.concat
val itos = Int.toString

fun spr (m, n) = cat ["(", itos m, ",", itos n, ")"]

fun sumUpTo n = if (n<=1) then 1 else (n*n+n) div 2

fun add ((a,b),(c,d)) = (a+c,b+d)

val default = 500000

fun valOf opt = (case opt
  of SOME x => x
   | NONE => Debug.failwith "NONE"
  (* end case *))

fun getSize args = (case args
  of a1::a2::t =>
       if String.same (a1, "-size") then
         valOf (Int.fromString a2)
       else
         getSize (a2::t)
   | _ => default
  (* end case *))

fun test z = let
  val _ = ln ("testing " ^ itos z)
  val arr = [| (n, n+1) | n in [| 1 to z |] |]
  val (sum1, sum2) = PArray.reduce add (0,0) arr
  val s1 = sumUpTo z
  val s2 = sumUpTo(z+1)-1
  val _ = ln ("finished testing " ^ itos z)
  in
    (* ln (cat ["expecting ", spr (s1, s2), ": ", spr (sum1, sum2)]) *)
    (sum1, sum2)
  end

fun main (name, args) = let
  val upperLim = getSize args
  val tests = [| test n | n in [| upperLim to (upperLim+50) |] |]
  in
    PArray.app (fn p => Print.printLn (spr p)) tests
  end

val _ = main (CommandLine.name (), CommandLine.arguments ())

val _ = Print.printLn "done."
