structure AssocList =
  struct

(*    _primcode (

      define @lookup(t : assoc_tag / exh : any) : option =
        fun f (al : assoc_list / exh : exh) : option =
	    case al
	     of ANIL => return(NONE)
	      | ACONS (t' : assoc_tag, elt : any, al : assoc_list) =>
		if Equal(t, t')
		   then return(SOME(elt))
		else 
		    apply f(al / exh)
            end
         apply f(al / exh)
      ;

      define @test(/exh : any) : [int]= 
        return(alloc(0))
      ;

    )
*)

_primcode(
      define @test(/exh : any) : [int]= 
        return(alloc(0))
      ;   
)

val x = _prim (hlop @test)

val _ = print (itos x^"\n")

  end
