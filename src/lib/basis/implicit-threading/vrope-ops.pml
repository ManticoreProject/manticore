(* vrope-ops.sml
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

structure VRopeOps =
  struct

    structure F = Future1
    structure PT = PrimTypes
    structure L = List
    structure A = Array64

    datatype 'a vrope =
	     VLEAF of (int * 'a A.array)
	   | VCAT of (int * int * 'a vrope * 'a vrope)

    _primcode(

      (* For mapping a function over the elements of the leaves of a vrope in parallel. *)
      define @vrope-map (f : fun (any / PT.exh -> any), r : vrope / exh : PT.exh) : vrope =
	fun m (r : vrope / ) : vrope = 
	  case r
	    of VLEAF(len:PT.ml_int, data:A.array) => 
		 let newData : A.array = A.@functional-map (f, data / PT.exh)
		 let newLeaf : vrope = VLEAF(len, newData)
		 return (newLeaf)
	     | VCAT(len:PT.ml_int, depth:PT.ml_int, r1:vrope, r2:vrope) =>
		 fun th (u : unit / exh : PT.exh) : vrope = apply m (r2)
		 let fut : future = F.@future (th / exh)
		 let newR1 : vrope = apply m (r1)
		 let newR2 : vrope = F.@touch (fut / exh)
		 let newR : vrope = VCAT (len, depth, newR1, newR2)
		 return (newR)
	  end (* case *)
	  (* end definition of m *)
	let newR : vrope = apply m (r)
	return (newR)
      ;

      define @vrope-map-w (args : [(* f *) fun (any / PT.exh -> any),
                                  (* r *) vrope]  / exh : PT.exh) : vrope =
	let f : fun (any / PT.exh -> any) = #0(args)
	let r : vrope = #1(args)
	@vrope-map(f, r / exh)
      ;

      define @vrope-length-int (r : vrope / exh : PT.exh) : int =
	case r
	 of VLEAF(n : PT.ml_int, _ : list) => return (#0(n))
	  | VCAT (n : PT.ml_int, _ : PT.ml_int, _ : vrope, _ : vrope) => return (#0(n))
	end
      ;

    (* A subscript operator for vropes (which were parrays in the surface program). *)
      define @vrope-sub (r : vrope, n : int / exh : PT.exh) : any =
	fun sub (r : vrope, n : int / ) : any =
	  case r
	    of VLEAF (len:PT.ml_int, data:L.list) =>
		 let foundIt : PT.bool = I32Lt (n, #0(len))
		 if foundIt
		   then 
		     let res2 : any = A.@sub(data, n / exh)
		     return (res2)
		   else
		     do assert(PT.FALSE)
		     return(enum(0):any)
	     | VCAT (len:PT.ml_int, depth:PT.ml_int, r1:vrope, r2:vrope) =>
		 let leftLen : int = @vrope-length-int (r1 / exh)
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

      define @vrope-sub-w (arg : [vrope, PT.ml_int] / exh : PT.exh) : any =
	let r : vrope     = #0(arg)
	let mln : ml_int = #1(arg)
	let n : int      = unwrap(mln)
	@vrope-sub(r, n / exh)
      ;

    )

    val vropeMap : (('a -> 'b) * 'a vrope) -> 'b vrope = _prim(@vrope-map-w)
    val vropeSub : ('a vrope * int) -> 'a = _prim(@vrope-sub-w)

  end
