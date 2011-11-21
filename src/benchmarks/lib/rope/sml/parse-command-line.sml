structure ParseCommandLine (* : sig
    val exists  string -> bool
    val parse   : string -> (string list -> 'a option) -> 'a -> 'a
    val parse1  : string -> string -> 'a option) -> 'a -> 'a
 end *) = struct

fun stringSame (s1, s2) = String.compare (s1, s2) = EQUAL

fun find param = let
  fun f args = (case args
    of nil => 
         NONE
     | a :: args =>
         if stringSame (a, param) then
	   SOME args
	 else
	   f args)
  in
    f (CommandLine.arguments ())
  end

fun exists param = (case find param
  of SOME nil => true
   | SOME _ => (raise Fail "exists: failed to parse argument")
   | _ => false)

fun parse param cvt dflt = (case find param
  of NONE =>
       dflt
   | SOME args => (case cvt args
       of NONE =>
	    (raise Fail "find: failed to parse argument")
	| SOME y => y))

fun parse1 param cvt dflt = let
  fun f args = (case args
    of nil => NONE
     | a :: args => cvt a)
  in
    parse param f dflt
  end

end
