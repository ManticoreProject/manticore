(* rope-ops.sml
 *
 * Ropes with vectors at the leaves.
 *
 * (c) 2008 The Manticore Group (http://manticore.cs.uchicago.edu)
 *)

(*
 * One point about putting vectors at the leaves of ropes.  For non-pointer types
 * (e.g., float parray), you can allocate uninitialized vectors and fill them in
 * as you compute values, but for parrays of pointers, you'll need to build a list
 * of results and then use a "fromList" allocation.  This requirement is because
 * of the GC invariants.
 *   - John 
 *)

structure RopeOps =
  struct

    structure F = Future1
    structure PT = PrimTypes
    structure L = List
    structure A = Array64

    datatype 'a rope =
	     LEAF of (int * 'a A.array)
	   | CAT of (int * int * 'a rope * 'a rope)

    _primcode(

      (* For mapping a function over the elements of the leaves of a rope in parallel. *)
      define @rope-map (f : fun (any / exh -> any), r : rope / exh : exh) : rope =
	fun m (r : rope / ) : rope = 
	  case r
	    of LEAF(len:int, data:A.array) => 
		 let newData : A.array = A.@functional-map (f, data / exh)
		 let newLeaf : rope = LEAF(len, newData)
		 return (newLeaf)
	     | CAT(len:int, depth:int, r1:rope, r2:rope) =>
		 fun th (u : unit / exh : exh) : rope = apply m (r2)
		 let fut : F.future = F.@future (th / exh)
		 let newR1 : rope = apply m (r1)
		 let newR2 : rope = F.@touch (fut / exh)
		 let newR : rope = CAT (len, depth, newR1, newR2)
		 return (newR)
	  end (* case *)
	  (* end definition of m *)
	let newR : rope = apply m (r)
	return (newR)
      ;

      define @rope-map-w (args : [(* f *) fun (any / exh -> any),
                                  (* r *) rope]  / exh : exh) : rope =
	let f : fun (any / exh -> any) = #0(args)
	let r : rope = #1(args)
	@rope-map(f, r / exh)
      ;

      define @rope-length-int (r : rope / exh : exh) : int =
	case r
	 of LEAF(n : int, _ : list) => return (n)
	  | CAT (n : int, _ : int, _ : rope, _ : rope) => return (n)
	end
      ;

    (* A subscript operator for ropes (which were parrays in the surface program). *)
      define @rope-sub (r : rope, n : int / exh : exh) : any =
	fun sub (r : rope, n : int / ) : any =
	  case r
	    of LEAF (len:int, data:L.list) =>
		 let foundIt : PT.bool = I32Lt (n, len)
		 if foundIt
		   then 
		     let res2 : any = A.@sub(data, n / exh)
		     return (res2)
		   else
		     let e : exn = Fail(@"bogus rope")
		     throw exh(e)
	     | CAT (len:int, depth:int, r1:rope, r2:rope) =>
		 let leftLen : int = @rope-length-int (r1 / exh)
		 let onTheLeft : PT.bool = I32Lt (n, leftLen)
		   if onTheLeft
		     then
		       let res3 : any = apply sub (r1, n)
		       return (res3)
		     else
		       let newN : int = I32Sub (n, leftLen)
		       let res4 : any = apply sub (r2, newN)
		       return (res4)
	    end
	let res5 : any = apply sub (r, n)
	return (res5)
      ;

      define @rope-sub-w (arg : [rope, PT.ml_int] / exh : exh) : any =
	let r : rope     = #0(arg)
	let mln : ml_int = #1(arg)
	let n : int      = unwrap(mln)
	@rope-sub(r, n / exh)
      ;

    )

    val ropeMap : (('a -> 'b) * 'a rope) -> 'b rope = _prim(@rope-map-w)
    val ropeSub : ('a rope * int) -> 'a = _prim(@rope-sub-w)

  end
