structure RopeOps =
  struct

    structure F = Future1
    structure PT = PrimTypes
    structure L = List

    datatype 'a rope =
	     LEAF of (int * 'a L.list)
	   | CAT of (int * int * 'a rope * 'a rope)

    _primcode(

      (* For mapping a function over the elements of the leaves of a rope in parallel. *)
      define @rope-map (f : fun (any / PT.exh -> any), r : rope / exh : PT.exh) : rope =

	fun m (r : rope / ) : rope = 
	  case r
	    of LEAF(len:[int], data:L.list) => 
		 let newData : L.list = PrimList.@map (f, data / PT.exh)
		 let newLeaf : rope = LEAF(#0(len), newData)
		 (* do ccall M_Print("leaf size \000")
		 do ccall M_PrintInt(len)
		 do ccall M_Print("\n\000") *)
		 return (newLeaf)
	     | CAT(len:[int], depth:[int], r1:rope, r2:rope) =>
		 fun th (u : unit / exh : PT.exh) : rope = apply m (r2)
		 let fut : future = F.@future (th / exh)
		 let newR1 : rope = apply m (r1)
		 let newR2 : rope = F.@touch (fut / exh)
		 let newR : rope = CAT (len, depth, newR1, newR2)
		 return (newR)
	  end (* case *)
	  (* end definition of m *)

	let newR : rope = apply m (r)
	return (newR)
      ;

      define @rope-map-wrapper (args : [(* f *) fun (any / PT.exh -> any),
				       (* r *) rope]  / exh : PT.exh) : rope =

	let f : fun (any / PT.exh -> any) = #0(args)
	let r : rope = #1(args)

	@rope-map(f, r / exh)
      ;

      define @rope-length-int (r : rope / exh : PT.exh) : int =
	case r
	 of LEAF(n : [int], _ : list) => return (#0(n))
	  | CAT (n : [int], d : [int], _ : rope, _ : rope) => return (#0(n))
	end
      ;

    (* A subscript operator for ropes (which were parrays in the surface program). *)
      define @rope-sub (r : rope, n : int / exh : PT.exh) : any =

	fun getFromRope (r : rope, n : int / ) : any =
	  case r
	    of LEAF (len:[int], data:L.list) =>
		 let foundIt : PT.bool = I32Lt (n, #0(len))
		 if foundIt
		   then 
		     let res2 : any = PrimList.@nth(data, n / exh)
		     return (res2)
		   else
		     do assert(PT.FALSE)
		     return(enum(0):any)
	     | CAT (len:[int], depth:[int], r1:rope, r2:rope) =>
		 let leftLen : int = @rope-length-int (r1 / exh)
		 let onTheLeft : PT.bool = I32Lt (n, leftLen)
		   if onTheLeft
		     then
		       let res3 : any = apply getFromRope (r1, n)
		       return (res3)
		     else
		       let newN : int = I32Sub (n, leftLen)
		       let res4 : any = apply getFromRope (r2, newN)
		       return (res4)
	    end

	let res5 : any = apply getFromRope (r, n)
	return (res5)
      ;

      define @rope-sub-wrapper (arg : [rope, PT.ml_int] / exh : PT.exh) : any =
	let r : rope     = #0(arg)
	let mln : ml_int = #1(arg)
	let n : int      = unwrap(mln)
	@rope-sub(r, n / exh)
      ;

    )

    val ropeMap : (('a -> 'b) * 'a rope) -> 'b rope = _prim(@rope-map-wrapper)
    val ropeSub : ('a rope * int) -> 'a

  end
