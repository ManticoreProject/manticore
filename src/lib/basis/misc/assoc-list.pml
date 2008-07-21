structure AssocList =
  struct

    structure O = Option
    structure PT = PrimTypes
    structure L = List

    _primcode (
      typedef assoc_tag = any;
      typedef elt = any;
      typedef assoc_list = L.list;

      define @find (ls : L.list, tg : assoc_tag / exh : PT.exh) : O.option =
        fun lp (ls : L.list / exh : PT.exh) : O.option =
          case ls
	   of NIL => return(NONE)
	    | L.CONS(x : [assoc_tag, elt], xs : L.list) =>
	      if Equal(#0(x), tg)
                 then return(O.SOME(#1(x)))
                 else apply lp(xs / exh)
          end
        apply lp(ls / exh)
      ;

      define @insert (ls : L.list, tg : assoc_tag, elt : elt / exh : PT.exh) : L.list =
        return(L.CONS(alloc(tg, elt), ls))
      ;

      define @test (x : PT.unit / exh : PT.exh) : PT.bool =
        cont fail () = return(FALSE)
        let ls : L.list = NIL
        let x : O.option = hlop @find(ls, tag(test) / exh)
        do case x of NONE => return() | SOME (x:any) => throw fail() end
        let ls : L.list = hlop @insert(ls, tag(test), alloc(1234) / exh)
        let x : O.option = hlop @find(ls, tag(test) / exh)
        do case x of NONE => throw fail() 
		   | SOME (x:[int]) => 
		     if I32Eq(#0(x), 1234) then return() else throw fail() 
	   end
        let ls : L.list = hlop @insert(ls, tag(test2), alloc(12345) / exh)
        let x : O.option = hlop @find(ls, tag(test2) / exh)
        do case x of NONE => throw fail() 
		   | SOME (x:[int]) => 
		     if I32Eq(#0(x), 12345) then return() else throw fail() 
	   end
        let x : O.option = hlop @find(ls, tag(test) / exh)
        do case x of NONE => throw fail() 
		   | SOME (x:[int]) => 
		     if I32Eq(#0(x), 1234) then return() else throw fail() 
	   end
        let x : O.option = hlop @find(ls, tag(test3) / exh)
        do case x of NONE => return()
		   | SOME (x:[int]) => 
		     throw fail()
	   end
        return(TRUE)
      ;

    )

    val test = _prim (hlop @test)
    val _ = UnitTesting.validate "assoc-list-1" test

  end
