(* this program tests the compilation of pattern matching polymorphic
 * data constructors.  The correct output is
 *	1
 *	2
 *	3
 *)

datatype ('a, 'b) option2 = NONE2 | SOME2 of ('a * 'b)

fun f x = (case x
       of NONE => 1
	| SOME(a, b) => a
      (* end case *))

fun f2 x = (case x
       of NONE2 => 1
	| SOME2(a, b) => a
      (* end case *))

fun f3 x = (case x
       of NONE2 => (3, 4)
	| SOME2 x => x
    (* end case *))

val _ = Print.printLn(Int.toString(f(SOME(1, 2))))
val _ = Print.printLn(Int.toString(f2(SOME2(2, 3))))
val _ = Print.printLn(Int.toString(f2(SOME2(f3 NONE2))))
