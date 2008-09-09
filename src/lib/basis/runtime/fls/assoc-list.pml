(* assoc-list.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


structure AssocList =
  struct

    structure O = Option
    structure PT = PrimTypes
    structure L = List

    _primcode (
      typedef assoc_tag = any;
      typedef elt = any;
      typedef assoc_list = L.list;

      define @find (ls : assoc_list, tg : assoc_tag / exh : PT.exh) : O.option =
        fun lp (ls : L.list / exh : PT.exh) : O.option =
          case ls
	   of L.NIL => return(O.NONE)
	    | L.CONS(x : [assoc_tag, elt], xs : L.list) =>
	      if Equal(#0(x), tg)
                 then return(O.SOME(#1(x)))
                 else apply lp(xs / exh)
          end
        apply lp(ls / exh)
      ;

      define @insert (ls : assoc_list, tg : assoc_tag, elt : elt / exh : PT.exh) : assoc_list =
        return(L.CONS(alloc(tg, elt), ls))
      ;


      define @test (x : PT.unit / exh : PT.exh) : PT.bool =
        cont fail () = return(PT.FALSE)
        let ls : L.list = L.NIL
        let x : O.option = @find(ls, tag(test) / exh)
        do case x of O.NONE => return() | O.SOME (x:any) => throw fail() end
        let ls : L.list = @insert(ls, tag(test), alloc(1234) / exh)
        let x : O.option = @find(ls, tag(test) / exh)
        do case x of O.NONE => throw fail() 
		   | O.SOME (x:[int]) => 
		     if I32Eq(#0(x), 1234) then return() else throw fail() 
	   end
        let ls : L.list = @insert(ls, tag(test2), alloc(12345) / exh)
        let x : O.option = @find(ls, tag(test2) / exh)
        do case x of O.NONE => throw fail() 
		   | O.SOME (x:[int]) => 
		     if I32Eq(#0(x), 12345) then return() else throw fail() 
	   end
        let x : O.option = @find(ls, tag(test) / exh)
        do case x of O.NONE => throw fail() 
		   | O.SOME (x:[int]) => 
		     if I32Eq(#0(x), 1234) then return() else throw fail() 
	   end
        let x : O.option = @find(ls, tag(test3) / exh)
        do case x of O.NONE => return()
		   | O.SOME (x:[int]) => 
		     throw fail()
	   end
        return(PT.TRUE)
      ;
    )

(*   val test : unit -> bool = _prim (@test)
   val _ = UnitTesting.validate "assoc-list-1" test
*)

  end
