(* read-set.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Chronologically ordered read sets
 *)

 

structure ReadSet = 
struct
	
    

	(*Careful, if this changes, we could possibly be indexing a "WithoutK" item 
     *incorrectly when filtering the read set*)
    datatype 'a item = Write of 'a * 'a * 'a | NilItem | WithK of 'a * 'a * 'a * 'a * 'a
                     | WithoutK of 'a * 'a | Abort of unit 


    _primcode(
        extern int inLocalHeap(void *, void *);

    	typedef read_set = ![List.list, (*0: spine of the read set*) 
    						 item, 	    (*1: last element of the read set*)
    						 item, 	    (*2: last checkpoint (element on short path)*)
    						 int];	    (*3: number of checkpoints in read set*)

    	define @new() : read_set = 
    		let rs : read_set = alloc(nil, NilItem, NilItem, 0)
    		return(rs)
    	;

    	define @insert-without-k(tv:any, readSet : read_set / exh:exh) : read_set =
            let tv : [int] = ([int]) tv
    		let newItem : item = WithoutK(tv, NilItem)
    		let vp : vproc = host_vproc
    		let heapBase : long = vpload(HEAP_BASE, vp)
            let limitPtr : long = vpload(LIMIT_PTR, vp)
            let lastAddr : any = (any) #1(readSet)
            case #1(readSet)
                of WithoutK(_:any,next:item) =>
                    if I64Gte(lastAddr, heapBase)
                    then
                        if I64Lt(lastAddr, limitPtr)
                        then
                            let last : ![any,item,item] = (![any,item,item]) #1(readSet)
                            do #2(last) := newItem
                            do #1(readSet) := newItem
                            let newRS : read_set = alloc(#0(readSet), newItem, #2(readSet), #3(readSet))
                            do ccall M_Print_Int("inserting %d into unpromoted read set\n", #0(tv))
                            return(newRS)
                        else
                            do ccall M_Print_Long("starting new cell, previous address is %lu, ", lastAddr)
                            do ccall M_Print_Long("base is %lu, ", heapBase)
                            do ccall M_Print_Long("limit pointer is at %lu\n", limitPtr)
                            let newList : List.list = CONS(newItem, #0(readSet))
                            let newRS : read_set = alloc(newList, newItem, #2(readSet), #3(readSet))
                            return(newRS)
                    else
                        do ccall M_Print_Long("starting new cell, previous address is %lu, ", lastAddr)
                        do ccall M_Print_Long("base is %lu, ", heapBase)
                        do ccall M_Print_Long("limit pointer is at %lu\n", limitPtr)
                        let newList : List.list = CONS(newItem, #0(readSet))
                        let newRS : read_set = alloc(newList, newItem, #2(readSet), #3(readSet))
                        return(newRS)
                 | NilItem => 
                    let newRS : read_set = alloc(CONS(newItem, nil), newItem, NilItem, 0)
                    return(newRS)
                 | WithK(_:any,_:cont(any),_:item,_:item,_:item) => 
                    throw exh(Fail(@"Insert: WithK"))
                 | Write(_:any,_:any,_:any) => throw exh(Fail(@"Insert: Write"))
                 | Abort(x:unit) => throw exh(Fail(@"Insert: Abort"))
            end
        ;

        define @new-wrap(x:unit / exh:exh) : read_set = 
            let rs : read_set = @new()
            return(rs);

        define @insert-wrap(arg : [any, read_set] / exh:exh) : read_set = 
            let newRS : read_set = @insert-without-k(#0(arg), #1(arg) / exh)
            return(newRS);


        define @app(arg : [fun(any / exh -> unit), read_set] / exh:exh) : unit = 
            let f : fun(any / exh -> unit) = #0(arg)
            let readSet : read_set = #1(arg)
            fun lpCell(i:item) : () = 
                case i
                    of WithoutK(x:any, next:item) =>
                        do ccall M_Print_Long("\nInside WithoutK, address of cell is %p, ", i)
                        do ccall M_Print_Long("address of boxed int is %p, ", x)
                        let y : [long,any,item] = ([long,any,item]) i
                        do ccall M_Print_Long("tag is %lu: ", #0(y))
                        let _ : unit = apply f(x / exh)
                        apply lpCell(next)
                     | WithK(x:any,_:cont(any),_:item,next:item,_:item) =>
                        do ccall M_Print_Long("\nInside WithK, address of cell is %p, ", i)
                        do ccall M_Print_Long("address of boxed int is %p: ", x)
                        let y : [long,any,item] = ([long,any,item]) i
                        do ccall M_Print_Long("tag is %lu: ", #0(y))
                        let _ : unit = apply f(x / exh)
                        apply lpCell(next)
                     | NilItem => return()
                end
            fun lpSpine(l : List.list) : unit = 
                case l 
                    of CONS(hd:item, tl:List.list) => 
                        let x : unit = apply lpSpine(tl)
                        do apply lpCell(hd)
                        return(x)
                     | nil => return(UNIT)
                end
            apply lpSpine(#0(readSet));

    )

    type 'a readset = _prim(read_set)
    val new : unit -> 'a readset = _prim(@new-wrap)
    val insert : 'a * 'a readset -> 'a readset = _prim(@insert-wrap)
    val app : ('a -> unit) * 'a readset -> unit = _prim(@app)
end



