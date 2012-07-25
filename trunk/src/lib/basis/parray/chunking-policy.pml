(* chunking-policy.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Determine the chunking policy via the command line. Chunking policies include
 * the following:
 *   Sequential                -chunking-strategy SEQ
 *   Eager Tree Splitting      -chunking-strategy ETS SST    
 *        (the integer SST is the stop-splitting threshold)
 *   Lazy Tree Splitting       -chunking-strategy LTS PPT
 *        (the integer PPT is the profitable-parallelism threshold)
 *)

structure ChunkingPolicy = struct

  val fail = Fail.fail "ChunkingPolicy"

  datatype chunking_policy
    = Sequential
    | ETS of int (* SST *)
    | LTS of int (* PPT *)

  fun toString cp = (case cp
    of Sequential => "Sequential"
     | ETS n => "ETS " ^ Int.toString n
     | LTS n => "LTS " ^ Int.toString n)

  local
    val dflt = LTS 1
    fun cvt args = (case args
      of a1 :: a2 :: _ =>
           if ParseCommandLine.stringSame (a1, "ETS") then
	     (case Int.fromString a2
	       of SOME SST => SOME (ETS SST)
		| NONE => NONE)
	   else if ParseCommandLine.stringSame (a1, "LTS") then
	     (case Int.fromString a2
	       of SOME PPT => SOME (LTS PPT)
		| NONE => NONE)
	   else if ParseCommandLine.stringSame (a1, "SEQ") then
	     SOME Sequential
	   else
	     NONE
       | a1 :: _ =>
           if ParseCommandLine.stringSame (a1, "SEQ") then
	     SOME Sequential
	   else
	     fail "cvt" "invalid argument (loc1)"
       | _ => fail "cvt" "invalid argument (loc2)"
      (* end case *))
    val policyR = Ref.new (ParseCommandLine.parse "-chunking-policy" cvt dflt)
  in
    fun get () = Ref.get policyR
    fun set policy' = Ref.set (policyR, policy')
  end (* local *)

end (* structure *)
