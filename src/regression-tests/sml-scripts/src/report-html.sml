(* report-html.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Write tests (as defined in the Tests module) to HTML.
 *)

structure ReportHTML = struct

  structure U = Utils
  structure H = MiniHTML
  structure L = Locations
  structure T = Tests

(* niceTake : 'a list * int -> 'a list *)
(* Like List.take, but doesn't raise an exn if there aren't enough elements. *)
  fun niceTake (xs, n) = let
    fun tk (_, 0, acc) = rev acc
      | tk ([], _, acc) = rev acc
      | tk (x::xs, n, acc) = tk (xs, n-1, x::acc)
    in
      if (n > 0) then tk (xs, n, []) else []
    end

(* recentTests : T.test_result -> T.test_result list *)
  fun recentTests (T.TR {goalName, testName, ...}) = let
    val s = String.concatWith "." [goalName, testName, "result"]
    val sameTest = String.isSuffix s 
    val files = U.filesWithin sameTest L.archiveDir
    val sortedFiles = U.antialphasort (fn s => s) files
    val last4 = niceTake (sortedFiles, 4)
    in
      map T.readTestResult last4
    end

(* codify : int option -> string -> H.html *)
  fun codify optLim s = let
    val loc = String.tokens (fn c => c = #"\n") s
    val lim = case optLim of SOME lim => lim | NONE => List.length loc
    fun loop ([], _, acc) = rev acc
      | loop ([h], _, acc)  = rev (H.str h :: acc)
      | loop (_, 0, acc) = rev (H.str "..." :: acc)
      | loop (h::t, n, acc) = loop (t, n-1, H.br :: H.str h :: acc)
    in
      if s = "" orelse lim <= 0
      then H.nbsp
      else H.codeH (H.seq (loop (loc, lim, [])))
    end

(* outcomeHTML : T.outcome -> H.html *)
  fun outcomeHTML oc =
   (case oc
      of T.DidNotCompile whyNot => H.spanCH ("outcome-dnc", 
					     if whyNot = "" then
                                               H.str "DidNotCompile"
					     else
                                               H.seq [H.str "DidNotCompile",
						      H.pH (codify (SOME 5) whyNot)])
       | T.TestFailed => H.spanCS ("outcome-failed", "TestFailed")
       | T.TestSucceeded => H.spanCS ("outcome-succeeded", "TestSucceeded")
     (* end case *))

(* test_result : T.test_result -> H.html *)
  fun test_result (tr as T.TR info) = let
    val {testName, goalName, outcome, expected, actual, ...} = info
    fun mkRow isHeader (T.TR {stamp=T.STAMP{timestamp, version}, outcome, ...}) = let
      val mkCell  = if isHeader then H.th else H.td
      val mkCellH = if isHeader then H.thH else H.tdH
      in
        H.trH [mkCell  (Date.toString timestamp ^ " (" ^ version ^ ")"),
	       mkCellH (outcomeHTML outcome)]
      end
    val recent = recentTests tr
    val recentTable = H.tableCH ("recent", (mkRow true tr) ::
					   map (mkRow false) recent)
					  
    in
      case outcome
        of T.DidNotCompile reason =>
             H.seq [H.h2CS ("testfile", testName),
		    H.pCH  ("results", recentTable)]
	 | _ => let
             val expCode = codify NONE expected
	     val actCode = codify NONE actual
             val cmpTable = H.tableCH ("results", [H.trH [H.th "expected",
							  H.th "actual"],
						   H.trH [H.tdH expCode,
							  H.tdH actCode]])
	     in
	       H.seq [H.h2CS ("testfile", testName),
		      H.pCH  ("results", recentTable),
		      H.pCH  ("results", cmpTable)]
	     end
    end

(* readme : T.readme -> H.html *)
  fun readme (T.README {text, ...}) = H.pCS ("readme", text)

(* goal : T.goal -> H.html *)
  fun goal (T.G (me, results)) = let
    fun getTestName (T.TR {testName, ...}) = testName
    val T.README {goalName, ...} = me
    val content = H.seq (H.h2CS ("goal", goalName) ::
			 readme me ::
			 map test_result (U.alphasort getTestName results))
    in
      H.divCAH ("goal",
		[{key="id", value=goalName}],
		content)
    end

(* report : T.report -> H.html *)
  fun mkReport r = let
    fun getGoalName (T.G (T.README {goalName, ...}, _)) = goalName
    val (d, ver) = 
     (case r
        of (T.G (_, T.TR {stamp, ...}::_))::_ => let
             val T.STAMP {timestamp, version} = stamp
             in
               (Date.toString timestamp, version)
             end
         | _ => raise Fail "empty report"
        (* end case *))
    val title = "Manticore: Regression Test Results"
    val body  = H.seq (H.h1 title ::
		       H.h2CS ("datetime", d) ::
		       H.h3CS ("revision", ver) ::
		       map goal (U.alphasort getGoalName r))
    in
      (* Assumption: there will be a file results.css in the same directory as the output. *)
      if T.nullReport r
      then (print "null report\n"; raise Fail "null report")
      else H.htdoc (title ^ " " ^ d, ["./results.css"], body)
    end
    
end
