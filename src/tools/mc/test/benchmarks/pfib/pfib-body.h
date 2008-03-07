#define pfib_body(pfib, dval)                     
    (case i
       of 0 => 0
	| 1 => 1
	| n => let
         dval x = pfib(i-1)
         val y = pfib(i-2)
         in
	      x + y
	 end
      (* end case *))
