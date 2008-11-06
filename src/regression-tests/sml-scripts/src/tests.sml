(* tests.sml
 *
 * COPYRIGHT (c) 2008 Adam Shaw (http://people.cs.uchicago.edu/~adamshaw)
 * All rights reserved.
 *
 * Serializable test results.
 *)

(*
"Wed Mar 08 19:06:45 1995"

The function is equivalent to Date.fmt "%a %b %d %H:%M:%S %Y"
*)

structure Tests = struct

  type stamp = {timestamp : Date.date,
		version   : string}

  datatype outcome 
    = CompileFailed
    | TestFailed
    | TestSucceeded

  datatype readme
    = README of {timestamp : Date.date,
		 goalName  : string,
		 text      : string}

  datatype test_result 
    = TR of {stamp    : stamp,
	     goalName : string,
	     testName : string,
	     outcome  : outcome,
	     expected : string,
	     actual   : string}

(* n.b. "expected" and "actual" will be bound to "" when compile failed *)

(* readFile : string -> string *)
  fun readFile infile = let
    val inStream = TextIO.openIn infile
    fun read acc =
     (case TextIO.inputLine inStream
        of NONE => String.concatWith "\n" (rev acc)
	 | SOME s => read (s::acc)
        (* end case *))
    in
      read [] before TextIO.closeIn inStream
    end

(*
Date.toString:
  "Wed Mar 08 19:06:45 1995"
  The function is equivalent to Date.fmt "%a %b %d %H:%M:%S %Y"
*)

(* format : Date.date -> string *)
  val format = Date.toString

(* unformat : string -> Date.date *)
  fun unformat s = 
   (case Date.fromString s
      of NONE => raise Fail ("cannot parse this date: " ^ s)
       | SOME d => d
      (* end case *))

(* stampToString : stamp -> string *)
  fun stampToString {timestamp, version} =
    concat ["<stamp>\n",
	    format timestamp, "\n",
	    version, "\n",	      
	    "</stamp>\n"]

(* stampFromString : string -> stamp *)
  fun stampFromString s = 
   (case String.tokens (fn c => c = #"\n") s
      of ["<stamp>", d, v, "</stamp>"] => {timestamp = unformat d,
					   version = v}
       | _ => raise Fail ("couldn't parse the following as a stamp: " ^ s)
      (* end case *))

  fun outcomeToString CompileFailed = "CompileFailed"
    | outcomeToString TestFailed    = "TestFailed"
    | outcomeToString TestSucceeded = "TestSucceeded"

  fun outcomeFromString "CompileFailed" = CompileFailed
    | outcomeFromString "TestFailed"    = TestFailed
    | outcomeFromString "TestSucceeded" = TestSucceeded
    | outcomeFromString other = raise Fail ("unrecognized outcome: " ^ other)

  fun test_resultToString (TR info) = let
    val {stamp, goalName, testName, outcome, expected, actual} = info
    in
      String.concatWith "\n" ["<test_result>",
			      stampToString stamp,
			      "<goalName>",
			      goalName,
			      "</goalName>",
			      "<testName>",
			      testName,
			      "</testName>",
			      "<outcome>",
			      outcomeToString outcome,
			      "</outcome>",
			      "<expected>",
			      expected,
			      "</expected>",
			      "<actual>",
			      actual,
			      "</actual>",
			      "</test_result>\n"]
    end

(* scrape : string -> string list -> string option *)
  local
    fun scrape tagname ss = let
      fun loop ([], _) = raise Fail ("unmatched tag " ^ tagname)
	| loop (curr::more, acc) = if curr = concat ["</", tagname, ">"]
	  			   then (String.concatWith "\n" (rev acc), more)
				   else loop (more, curr::acc)
      in
        if hd ss = concat ["<", tagname, ">"]
	then loop (tl ss, [])
	else raise Fail "malformed"
      end
  in
  (* test_resultFromString : string -> test_result *)
    fun test_resultFromString s =
     (case String.tokens (fn c => c = #"\n") s
        of ("<test_result>"::
	    "<stamp>" :: d :: v :: "</stamp>" ::
	    "<goalName>" :: g :: "</goalName>" ::
	    "<testName>" :: t :: "</testName>" ::
	    "<outcome>" :: oc :: "</outcome>" ::
	    more) => let
              val (exp, r)  = scrape "expected" more
	      val (act, r') = scrape "actual" r
              in
	        case r'
		  of ["</test_result>"] => 
		       TR {stamp = {timestamp = unformat d, version = v},
			   goalName = g,
			   testName = t,
			   outcome = outcomeFromString oc,
			   expected = exp,
			   actual = act}
		   | _ => raise Fail ("unexpected end to test_result:\n" ^ 
				      (String.concatWith "\n" r'))
              end
	 | _ => raise Fail ("couldn't parse this test_result:\n" ^ s)
        (* end case *)) 
  end (* local *)

(* writeTestResult : test_result * string -> unit *)
  fun writeTestResult (tr, outfile) = let
    val s = test_resultToString tr
    val outStream = TextIO.openOut outfile
    in
      TextIO.output (outStream, s) before TextIO.closeOut outStream
    end

(* readTestResult : string -> test_result *)
  val readTestResult = test_resultFromString o readFile

end
