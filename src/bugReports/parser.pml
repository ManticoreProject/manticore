(* parser.pml
 * This version of the parser seg faults when it tries to print out the location in the table
 * it is looking up 3/19/2014
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This is an implementation of the Packrat parser presented in:
 * "Packrat Parsing: Simple, Powerful, Lazy, Linear Time."  It parses the simple expression, term
 * factor, grammar, producing an integer as a result.

 * E <- T '+' E | T        (0)
 * T <- F '*' T | F        (1)
 * F <- '(' E ')' | int    (2)
**)


type 'a vector = 'a Vector.vector

structure Parser = 
struct
    val vsub = Vector.sub

    (*The Parsed type contains the value of parsing the expression, as well as an index 
    **of the last character in the expression*)
    datatype result = Parsed of int * int | NoParse

    type memotable = ((result IVar.ivar) vector) vector

    (*matrix subscript*)
    fun sub(table, row, col) = Vector.sub(Vector.sub(table, row), col)

    (*update an element of the memo table*)
    fun update(table, row, col, newVal) = 
        let val i = sub(table, row ,col)
            val _ = IVar.putIVar(i, newVal)
        in newVal end

    (*lookup an element in the memo table*)
    fun lookup(table, row, col) = 
            (*BUG: This call to print causes a seg fault*)
        let val _ = print ("Looking up (" ^ Int.toString row ^ ", " ^ Int.toString col ^ ")\n")
            val theIVar = sub(table, row, col)
            val r = IVar.getIVar theIVar
        in r end

    fun parse input = 
        let val n = Vector.length input
            val table : memotable = Vector.tabulate(3, fn _ => Vector.tabulate(n, fn _ => IVar.newIVar()))
            fun lp i parseFun = if i = 0
                                then parseFun i
                                else let val _ = parseFun i
                                     in lp (i-1) parseFun
                                     end          
            and pExp i = 
                let fun matchPlus() = case lookup(table, 1, i)
                        of Parsed(l, i') => if vsub(input, i') = "+" 
                            then
                            (case lookup(table, 0, i'+1)
                                of Parsed(r, i'') => update(table, 0, i, Parsed(l + r, i''))
                                 | NoParse => NoParse)
                            else NoParse
                         |NoParse => NoParse
                in case matchPlus()
                    of Parsed(v, i) => Parsed(v, i)
                     | NoParse => update(table, 0, i, lookup(table, 1, i))
                end

            and pTerm i = 
                let fun matchTimes() = case lookup(table, 2, i)
                        of Parsed(l, i') => if vsub(input, i') = "*" 
                            then
                            (case lookup(table, 1, i'+1)
                                of Parsed(r, i'') => update(table, 1, i, Parsed(l * r, i''))
                                 | NoParse => NoParse)
                            else NoParse
                         | NoParse => NoParse
                in case matchTimes()
                    of Parsed(v, i) => Parsed(v, i)
                     | NoParse => update(table, 1, i, lookup(table, 2, i))
                end

            and pFactor i = 
                let fun matchParens() = if vsub(input, i) = "(" 
                        then 
                        case lookup(table, 0, i+1)
                            of Parsed(v, i') => if vsub(input, i') = ")"
                                                then update(table, 2, i, Parsed(v, i'+1))
                                                else NoParse
                              |NoParse => NoParse
                        else NoParse
                    fun matchInt() = case Int.fromString(vsub(input, i))
                                        of SOME v => update(table, 2, i, Parsed(v, i+1))
                                         | NONE => update(table, 2, i, NoParse)
                in case matchParens()
                    of Parsed(v, i) => Parsed(v, i)
                     | NoParse => matchInt()
                end
            val _ = SpecPar.spec(fn _ => lp (n-1) pFactor, 
                                 fn _ => SpecPar.spec(fn _ => lp (n-1) pTerm, fn _ => lp (n-1) pExp))  
            val res = IVar.getIVar (sub(table, 0, 0))
        in (res, table)
        end


end

structure Main =
struct

    fun genInput terms = 
        if terms = 0
        then [Int.toString (Rand.inRangeInt(0, 10))]
        else case Rand.inRangeInt(0, 3)
                of 0 => genInput(terms-1) @ ["+"] @ genInput (terms-1)
                 | 1 => genInput(terms-1) @ ["*"] @ genInput (terms-1)
                 | 2 => "(" :: genInput(terms-1) @ [")"]
                 |_ => raise Fail("Impossible: Out of range\n")

    fun printStr s = print( Vector.foldr (fn(i, s) => i ^ " " ^ s) "" s ^ "\n")

    val dfltN = 1

    fun getSizeArg args = case args
	  of arg1 :: arg2 :: args =>
	     if String.same (arg1, "-size") then Int.fromString arg2
	     else getSizeArg (arg2 :: args)
	   | _ => NONE

    fun main (_, args) =
	let val n = (case getSizeArg args
		      of NONE => dfltN
		       | SOME n => n)
		val str = Vector.fromList (genInput n @ ["end"])
	    fun doit () = Parser.parse str
	    val (res, _) = RunPar.run doit
	in
	    case res
	        of Parser.Parsed(v, _) => (printStr str; print ("Result = " ^ Int.toString v ^ "\n"))
	         | NoParse => print ("Parsing failed\n")
	end

end

val _ = SpecPar.spec(fn _ => (), fn _ => ())

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())



































