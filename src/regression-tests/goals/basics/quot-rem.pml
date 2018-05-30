(* test cases for div/quot and mod/rem *)

(* div has the semantics of hardware divide instructions, and thus Int.quot in SML, where it rounds towards zero *)
val _ = Print.print("~3 div 2 = " ^ Int.toString(~3 div 2) ^ "\n")	(* ~1 *)
val _ = Print.print("3 div 2 = " ^ Int.toString(3 div 2) ^ "\n")	(* 1 *)

(* mod has the semantics of hardware divide instructions, and thus Int.rem in SML *)
val _ = Print.print("~5 mod 4 = " ^ Int.toString(~5 mod 4) ^ "\n")		(* ~1 *)
val _ = Print.print("~5 mod ~4 = " ^ Int.toString(~5 mod ~4) ^ "\n")	(* ~1 *)
val _ = Print.print("5 mod ~4 = " ^ Int.toString(5 mod ~4) ^ "\n")		(* 1 *)
val _ = Print.print("5 mod 4 = " ^ Int.toString(5 mod 4) ^ "\n")		(* 1 *)

val _ = Print.print("~3 div 2 = " ^ Long.toString((~3 : long) div 2) ^ "\n")	(* ~1 *)
val _ = Print.print("~5 mod 4 = " ^ Long.toString((~5 : long) mod 4) ^ "\n")	(* ~1 *)


(* NOTE once the MLRISC bug involving 32-bit sign extensions are fixed,
   or once we've transitioned to LLVM backend by default, use the below
   as a test instead. expected output is 6 "correct\n" prints *)
(*
fun check f g = 
	fn (a, b, expected) => let
		val (f_func, f_name) = f
		val (g_func, g_name) = g
		val fres = f_func a b
		val gres = g_func a b
	in
		if fres = gres andalso fres = expected 
		then Print.print "correct\n"
		else let
			val a = Int.toString a
			val b = Int.toString b
			val fres = Int.toString fres
			val gres = Int.toString gres
			val expected = Int.toString expected
		in
			Print.print ("wrong.\n"
			  ^ a ^ " " ^ f_name ^ " " ^ b ^ " = " ^ fres ^ "\n"
			  ^ a ^ " " ^ g_name ^ " " ^ b ^ " = " ^ gres ^ "\n"
			  ^ "and expected them both to be " ^ expected ^ "\n")
		end 
	end 

val modCases = [
	(~5, 4, ~1),
	(~5, ~4, ~1),
	(5, ~4, 1),
	(5, 4, 1)
]

val divCases = [
	(~3, 2, ~1),
	(3, 2, 1)
]

fun f_mod a b = a mod b
fun f_rem a b = Int.rem(a, b)
val modChecker = check ((f_mod, "mod")) ((f_rem, "rem"))

fun f_div a b = a div b
fun f_quot a b = Int.quot(a, b)
val divChecker = check ((f_div, "div")) ((f_quot, "quot"))

val _ = List.app modChecker modCases
val _ = List.app divChecker divCases
*)
