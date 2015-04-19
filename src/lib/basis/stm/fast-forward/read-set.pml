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

        typedef mutWithK = ![any,    (*0: tag*)
                             any,    (*1: tvar*)
                             any,    (*2: continuation*)
                             any,    (*3: write set*)
                             item,    (*4: next pointer*)
                             item];  (*5: next checkpoint pointer*)

        typedef stamp = VClock.stamp;
        typedef tvar = ![any, long, stamp]; (*contents, lock, version stamp*)

    	define @new() : read_set = 
    		let rs : read_set = alloc(nil, NilItem, NilItem, 0)
    		return(rs)
    	;

        define inline @abort(readSet : read_set, stamp : ![stamp]/ exh:exh) : () = 
            case #2(readSet)
                of WithK(tv:tvar,abortK:cont(any),ws:item,_:item,_:item) =>
                    let current : any = #0(tv)
                    if I64Eq(#1(tv), 0:long)
                    then
                        if I64Lt(#2(tv), #0(stamp))
                        then 
                            do FLS.@set-key(WRITE_SET, ws / exh)
                            let captureFreq : int = FLS.@get-counter2()
                            do FLS.@set-counter(captureFreq)
                            do FLS.@set-key(READ_SET, readSet / exh)
                            (*BUMP_PABORT*)
                            throw abortK(current)
                        else
                            let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                            throw abortK()
                    else
                        let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                        throw abortK()
                 | NilItem => 
                    let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                    throw abortK()
                 | _ => throw exh(Fail(@"abort: impossible!\n"))
            end;

        define @validate(readSet : read_set, stamp : ![stamp], newStamp : stamp, unlock : fun(unit / exh -> unit) / exh:exh) : () = 
            let rev : List.list = PrimList.@rev(#0(readSet) / exh)
            let rawStamp : stamp = #0(stamp)
            fun lpItem(i : item, rest:List.list, checkpoint : item, newStamp : stamp, current : List.list, count:int) : () = 
                case i
                   of WithK(tv:tvar, k:cont(any), ws:item, next:item, _:item) =>
                        let casted : mutWithK = (mutWithK) i
                        let valid : bool = 
                            if I64Eq(#1(tv), 0:long)
                            then
                                if I64Lt(#2(tv), rawStamp)
                                then return(true)
                                else return(false)
                            else 
                                if I64Eq(#1(tv), rawStamp)
                                then
                                    if I64Lt(#2(tv), rawStamp)
                                    then return(true)
                                    else return(false)
                                else return(false)
                        if(valid)
                        then
                            if Equal(k, enum(0))
                            then
                                apply lpItem(next, rest, checkpoint, newStamp, current, count)
                            else
                                apply lpItem(next, rest, i, newStamp, current, I32Add(count, 1))
                        else
                            if Equal(k, enum(0))
                            then 
                                let _ : unit = apply unlock(UNIT / exh)
                                do #4(casted) := NilItem
                                do #0(stamp) := newStamp
                                let newRS : read_set = alloc(current, checkpoint, checkpoint, count)
                                @abort(newRS, stamp / exh)
                            else 
                                let _ : unit = apply unlock(UNIT / exh)
                                do #4(casted) := NilItem
                                do #0(stamp) := newStamp
                                let newRS : read_set = alloc(current, i, i, I32Add(count, 1))
                                @abort(newRS, stamp / exh)
                    | WithoutK(tv:tvar, next:item) =>
                        let casted : ![any, any, item] = (![any,any,item]) i
                        let valid : bool = 
                            if I64Eq(#1(tv), 0:long)
                            then
                                if I64Lt(#2(tv), rawStamp)
                                then return(true)
                                else return(false)
                            else 
                                if I64Eq(#1(tv), rawStamp)
                                then
                                    if I64Lt(#2(tv), rawStamp)
                                    then return(true)
                                    else return(false)
                                else return(false)
                        if(valid)
                        then apply lpItem(next, rest, checkpoint, newStamp, current, count)
                        else 
                            let _ : unit = apply unlock(UNIT / exh)
                            do #2(casted) := NilItem
                            do #0(stamp) := newStamp
                            let newRS : read_set = alloc(current, checkpoint, checkpoint, count)
                            @abort(newRS, stamp / exh)
                    | NilItem => apply lpSpine(rest, checkpoint, newStamp, current, count)
                end
            and lpSpine(l:List.list, checkpoint : item, newStamp : stamp, current:List.list, count:int) : () = 
                case l
                   of CONS(hd:item, tl:List.list) => apply lpItem(hd, tl, checkpoint, newStamp, CONS(hd, current), count)
                    | nil => return()
                end
            do apply lpSpine(rev, NilItem, newStamp, nil, 0)
            return()
        ;

        define inline @getNumK(rs : read_set) : int = return(#3(rs));

        define @filterRS(readSet : read_set) : () = 
            fun dropKs(l:item, n:int) : int =   (*drop every other continuation*)
                case l
                   of NilItem => return(n)
                    | WithK(_:any,_:cont(any),_:List.list,_:item,next:item) =>
                        case next
                           of NilItem => return(n)
                            | WithK(_:any,_:cont(any),_:List.list,_:item,nextNext:item) =>
                                (* NOTE: if compiled with -debug, this will generate warnings
                                 * that we are updating a bogus local pointer, however, given the
                                 * nature of the data structure, we do preserve the heap invariants*)
                                let l : mutWithK = (mutWithK) l
                                let next : mutWithK = (mutWithK) next
                                do #2(next) := enum(0):any
                                do #5(l) := nextNext
                                apply dropKs(nextNext, I32Sub(n, 1))
                        end
                end
            let x :int = apply dropKs(#2(readSet), #3(readSet))
            do #3(readSet) := x
            return();

        define @insert-with-k(tv:any, k:cont(any), ws:item, readSet : read_set / exh:exh) : read_set = 
            let newItem : item = WithK(tv, k, ws, NilItem, #2(readSet))
            let vp : vproc = host_vproc
            let nurseryBase : long = vpload(NURSERY_BASE, vp)
            let limitPtr : long = vpload(LIMIT_PTR, vp)
            let lastAddr : any = (any) #1(readSet)
            case #1(readSet)
                of WithoutK(_:any,next:item) =>
                    if I64Gte(lastAddr, nurseryBase)
                    then
                        if I64Lt(lastAddr, limitPtr)
                        then
                            let last : ![any,item,item] = (![any,item,item]) #1(readSet)
                            do #2(last) := newItem
                            do #1(readSet) := newItem
                            let newRS : read_set = alloc(#0(readSet), newItem, newItem, I32Add(#3(readSet), 1))
                            return(newRS)
                        else
                            let newList : List.list = CONS(newItem, #0(readSet))
                            let newRS : read_set = alloc(newList, newItem, newItem, I32Add(#3(readSet), 1))
                            return(newRS)
                    else
                        let newList : List.list = CONS(newItem, #0(readSet))
                        let newRS : read_set = alloc(newList, newItem, newItem, I32Add(#3(readSet), 1))
                        return(newRS)
                 | NilItem => 
                    let newRS : read_set = alloc(CONS(newItem, nil), newItem, newItem, 1)
                    return(newRS)
                 | WithK(_:any,_:cont(any),_:item,_:item,_:item) => 
                    if I64Gte(lastAddr, nurseryBase)
                    then
                        if I64Lt(lastAddr, limitPtr)
                        then
                            let last : ![any,any,any,any,item,any] = (![any,any,any,any,item,any] ) #1(readSet)
                            do #4(last) := newItem
                            do #1(readSet) := newItem
                            let newRS : read_set = alloc(#0(readSet), newItem, newItem, I32Add(#3(readSet), 1))
                            return(newRS)
                        else
                            let newList : List.list = CONS(newItem, #0(readSet))
                            let newRS : read_set = alloc(newList, newItem, newItem, I32Add(#3(readSet), 1))
                            return(newRS)
                    else
                        let newList : List.list = CONS(newItem, #0(readSet))
                        let newRS : read_set = alloc(newList, newItem, newItem, I32Add(#3(readSet), 1))
                        return(newRS)
                 | Write(_:any,_:any,_:any) => throw exh(Fail(@"Insert: Write"))
                 | Abort(x:unit) => throw exh(Fail(@"Insert: Abort"))
            end
        ;

    	define @insert-without-k(tv:any, readSet : read_set / exh:exh) : read_set =
    		let newItem : item = WithoutK(tv, NilItem)
    		let vp : vproc = host_vproc
    		let nurseryBase : long = vpload(NURSERY_BASE, vp)
            let limitPtr : long = vpload(LIMIT_PTR, vp)
            let lastAddr : any = (any) #1(readSet)
            case #1(readSet)
                of WithoutK(_:any,next:item) =>
                    if I64Gte(lastAddr, nurseryBase)
                    then
                        if I64Lt(lastAddr, limitPtr)
                        then
                            let last : ![any,item,item] = (![any,item,item]) #1(readSet)
                            do #2(last) := newItem
                            do #1(readSet) := newItem
                            return(readSet)
                        else
                            let newList : List.list = CONS(newItem, #0(readSet))
                            let newRS : read_set = alloc(newList, newItem, #2(readSet), #3(readSet))
                            return(newRS)
                    else
                        let newList : List.list = CONS(newItem, #0(readSet))
                        let newRS : read_set = alloc(newList, newItem, #2(readSet), #3(readSet))
                        return(newRS)
                 | NilItem => 
                    let newRS : read_set = alloc(CONS(newItem, nil), newItem, NilItem, 0)
                    return(newRS)
                 | WithK(_:any,_:cont(any),_:item,_:item,_:item) => 
                    if I64Gte(lastAddr, nurseryBase)
                    then
                        if I64Lt(lastAddr, limitPtr)
                        then
                            let last : ![any,any,any,any,item,any] = (![any,any,any,any,item,any] ) #1(readSet)
                            do #4(last) := newItem
                            do #1(readSet) := newItem
                            let newRS : read_set = alloc(#0(readSet), newItem, #2(readSet), #3(readSet))
                            return(newRS)
                        else
                            let newList : List.list = CONS(newItem, #0(readSet))
                            let newRS : read_set = alloc(newList, newItem, #2(readSet), #3(readSet))
                            return(newRS)
                    else
                        let newList : List.list = CONS(newItem, #0(readSet))
                        let newRS : read_set = alloc(newList, newItem, #2(readSet), #3(readSet))
                        return(newRS)
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

        define @app2(arg : [fun(any / exh -> unit), read_set] / exh:exh) : unit = 
            let f : fun(any / exh -> unit) = #0(arg)
            let readSet : read_set = #1(arg)
            fun lpCell(i:item) : () = 
                case i
                    of WithoutK(x:any, next:item) =>
                        let y : [long,any,item] = ([long,any,item]) i
                        let _ : unit = apply f(x / exh)
                        apply lpCell(next)
                     | WithK(x:any,_:cont(any),_:item,next:item,_:item) =>
                        let y : [long,any,item] = ([long,any,item]) i
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
    val app : ('a -> unit) * 'a readset -> unit = _prim(@app2)
end



