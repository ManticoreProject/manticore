(* read-set.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Chronologically ordered read sets
 *)

 

structure ReadSet2 = 
struct
	
#define COUNT 

#ifdef COUNT
#define BUMP_PABORT do ccall M_BumpCounter(0)
#define PRINT_PABORT_COUNT let counter1 : int = ccall M_SumCounter(0) \
                           do ccall M_Print_Int("Partial-Aborts = %d\n", counter1)
#define BUMP_FABORT do ccall M_BumpCounter(1)
#define PRINT_FABORT_COUNT let counter2 : int = ccall M_SumCounter(1) \
                           do ccall M_Print_Int("Full-Aborts = %d\n", counter2)                     
#define PRINT_COMBINED do ccall M_Print_Int("Total-Aborts = %d\n", I32Add(counter1, counter2))     
#define BUMP_KCOUNT do ccall M_BumpCounter(2)
#define PRINT_KCOUNT let counter1 : int = ccall M_SumCounter(2) \
                     do ccall M_Print_Int("Fast Forward Continuation Hits = %d\n", counter1)                                                                                                 
#else
#define BUMP_PABORT
#define PRINT_PABORT_COUNT
#define BUMP_FABORT
#define PRINT_FABORT_COUNT
#define PRINT_COMBINED 
#define BUMP_KCOUNT
#define PRINT_KCOUNT 
#endif
    
#define START_TIMER let vp : vproc = host_vproc do ccall GenTimerStart(vp)
#define STOP_TIMER let vp : vproc = host_vproc do ccall GenTimerStop(vp)
   


	(*Careful, if this changes, we could possibly be indexing a "WithoutK" item 
     *incorrectly when filtering the read set*)
    datatype 'a item = Write of 'a * 'a * 'a | NilItem | WithK of 'a * 'a * 'a * 'a * 'a
                     | WithoutK of 'a * 'a | Abort of unit 


    datatype 'a rsCons = RSCONS of 'a * 'a * 'a | RSNil

    _primcode(
        define @allocPrintFun(x:unit / exh:exh) : any = 
            fun f(x:any / exh:exh) : unit = return(UNIT)
            let box : ![fun(any / exh -> unit)] = alloc(f)
            let box : ![fun(any / exh -> unit)] = promote(box)
            return(box);
    )
    
    val allocPrintFun : unit -> 'a = _prim(@allocPrintFun)
    val printFunPtr = allocPrintFun()
    fun getPrintFunPtr() = printFunPtr

    _primcode(

        extern int inLocalHeap(void *, void *);

    	typedef read_set = ![item,      (*0: first element of the read set*) 
    						 item, 	    (*1: last element of the read set*)
    						 item, 	    (*2: last checkpoint (element on short path)*)
    						 int];	    (*3: number of checkpoints in read set*)

        typedef mutWithK = ![any,    (*0: tag*)
                             any,    (*1: tvar*)
                             item,   (*2: next pointer*)
                             any,    (*3: write set*)
                             any,    (*4: continuation*)
                             item];  (*5: next checkpoint pointer*)

        typedef stamp = VClock.stamp;
        typedef tvar = ![any, long, stamp]; (*contents, lock, version stamp*)

        define @getPrintFunPtr = getPrintFunPtr;

        define @registerPrintFun(f : fun(any / exh -> unit) / exh:exh) : unit = 
            let funBox : ![fun(any / exh -> unit)] = @getPrintFunPtr(UNIT / exh)
            let f : fun(any / exh -> unit) = promote(f)
            do #0(funBox) := f
            return(UNIT);

    	define @new() : read_set = 
            let dummyTRef : ![any,long,long] = alloc(enum(0), 0:long, 0:long)
            let dummy : item = WithoutK(dummyTRef, NilItem)
    		let rs : read_set = alloc(dummy, dummy, NilItem, 0)
    		return(rs)
    	;

        define inline @logStat(x:any / exh:exh) : () = 
#ifdef COLLECT_STATS                            
            let stats : list = FLS.@get-key(STATS_KEY / exh)
            let stats : list = CONS(x, stats)
            FLS.@set-key(STATS_KEY, stats / exh)
#else
            return()
#endif          
        ;

        define @printTV(tv : any / exh:exh) : () = 
            let tv : [any] = ([any]) tv
            let f : ![fun(any / exh -> unit)] = @getPrintFunPtr(UNIT / exh)
            let f : fun(any / exh -> unit) = #0(f)
            let _ : unit = apply f(#0(tv) / exh)
            return();

        define inline @abort(readSet : read_set, stamp : ![stamp]/ exh:exh) : () = 
            let cp : item = #2(readSet)
            case cp
                of WithK(tv:tvar,_:item,ws:item,abortK:cont(any),_:item) =>
                    let current : any = #0(tv)
                    if I64Eq(#1(tv), 0:long)
                    then
                        if I64Lt(#2(tv), #0(stamp))
                        then 
                            do FLS.@set-key(WRITE_SET, ws / exh)
                            let captureFreq : int = FLS.@get-counter2()
                            do FLS.@set-counter(captureFreq)
                            do FLS.@set-key(READ_SET, readSet / exh)
                            let casted : mutWithK = (mutWithK) cp
                            do #2(casted) := NilItem
                            BUMP_PABORT
                            (*do @logStat(alloc(#3(readSet)) / exh)*)
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

        define inline @getNumK(rs:item / exh:exh) : int = 
            fun lp(i:item, count:int) : int =
                case i 
                   of NilItem => return(count)
                    | WithK(_:tvar,_:item,_:item,_:cont(any),next:item) => apply lp(next, I32Add(count, 1))
                end
            apply lp(rs, 0)
        ;

        define @validate(readSet : read_set, stamp : ![stamp], newStamp : stamp/ exh:exh) : () = 
            let rawStamp : stamp = #0(stamp)
            fun validateLoop(i : item, checkpoint : item, newStamp : stamp, count:int) : () = 
                case i
                   of WithK(tv:tvar, next:item, ws:item, k:cont(any), _:item) =>
                        let valid : bool = 
                            if I64Eq(#1(tv), 0:long)
                            then
                                if I64Lt(#2(tv), rawStamp)
                                then return(true)
                                else return(false)
                            else return(false)
                        if(valid)
                        then
                            if Equal(k, enum(0))
                            then apply validateLoop(next, checkpoint, newStamp, count)
                            else apply validateLoop(next, i, newStamp, I32Add(count, 1))
                        else
                            if Equal(k, enum(0))
                            then 
                                do #0(stamp) := newStamp
                                let newRS : read_set = alloc(#0(readSet), checkpoint, checkpoint, count)
                                @abort(newRS, stamp / exh)
                            else 
                                do #0(stamp) := newStamp
                                let newRS : read_set = alloc(#0(readSet), i, i, I32Add(count, 1))
                                @abort(newRS, stamp / exh)
                    | WithoutK(tv:tvar, next:item) =>
                        let valid : bool = 
                            if I64Eq(#1(tv), 0:long)
                            then
                                if I64Lt(#2(tv), rawStamp)
                                then return(true)
                                else return(false)
                            else return(false)
                        if(valid)
                        then apply validateLoop(next, checkpoint, newStamp, count)
                        else 
                            do #0(stamp) := newStamp
                            let newRS : read_set = alloc(#0(readSet), checkpoint, checkpoint, count)
                            @abort(newRS, stamp / exh)
                    | NilItem => return()
                end
            do apply validateLoop(#0(readSet), NilItem, newStamp, 0)
            return()
        ;

        define @validate-commit(readSet : read_set, stamp : ![stamp], newStamp : stamp, unlock : fun(unit / exh -> unit) / exh:exh) : () = 
            let rawStamp : stamp = #0(stamp)
            let count : ![int] = alloc(0)
            fun validateLoop(i : item, checkpoint : item, newStamp : stamp) : () = 
                case i
                   of WithK(tv:tvar, next:item, ws:item, k:cont(any), _:item) =>
                        if I64Lt(#2(tv), rawStamp)
                        then
                            if Equal(k, enum(0))
                            then
                                apply validateLoop(next, checkpoint, newStamp)
                            else
                                do #0(count) := I32Add(#0(count), 1)
                                apply validateLoop(next, i, newStamp)
                        else
                            if Equal(k, enum(0))
                            then 
                                let _ : unit = apply unlock(UNIT / exh)
                                do #0(stamp) := newStamp
                                let newRS : read_set = alloc(#0(readSet), checkpoint, checkpoint, #0(count))
                                @abort(newRS, stamp / exh)
                            else 
                                let _ : unit = apply unlock(UNIT / exh)
                                do #0(stamp) := newStamp
                                let newRS : read_set = alloc(#0(readSet), i, i, I32Add(#0(count), 1))
                                @abort(newRS, stamp / exh)
                    | WithoutK(tv:tvar, next:item) =>
                        let casted : ![any, any, item] = (![any,any,item]) i
                        if I64Lt(#2(tv), rawStamp)
                        then apply validateLoop(next, checkpoint, newStamp)
                        else 
                            let _ : unit = apply unlock(UNIT / exh)
                            do #0(stamp) := newStamp
                            let newRS : read_set = alloc(#0(readSet), checkpoint, checkpoint, #0(count))
                            @abort(newRS, stamp / exh)
                    | NilItem => return()
                end
            do apply validateLoop(#0(readSet), NilItem, newStamp)
            return()
        ;

        define inline @getNumK(rs : read_set) : int = return(#3(rs));

        define @filterRS(readSet : read_set) : () = 
            fun dropKs(l:item, n:int) : int =   (*drop every other continuation*)
                case l
                   of NilItem => return(n)
                    | WithK(_:any,_:item,_:List.list,_:cont(any),next:item) =>
                        case next
                           of NilItem => return(n)
                            | WithK(_:any,_:cont(any),_:List.list,_:item,nextNext:item) =>
                                (* NOTE: if compiled with -debug, this will generate warnings
                                 * that we are updating a bogus local pointer, however, given the
                                 * nature of the data structure, we do preserve the heap invariants*)
                                let l : mutWithK = (mutWithK) l
                                let next : mutWithK = (mutWithK) next
                                do #4(next) := enum(0):any
                                do #5(l) := nextNext
                                apply dropKs(nextNext, I32Sub(n, 1))
                        end
                end
            let x :int = apply dropKs(#2(readSet), #3(readSet))
            do #3(readSet) := x
            return();

        (*Note that these next two defines, rely on the fact that a heap limit check will not get
         *inserted within the body*)
        (*Add a checkpointed read to the read set*)
        define @insert-with-k(tv:any, k:cont(any), ws:item, readSet : read_set / exh:exh) : read_set = 
            let newItem : item = WithK(tv, NilItem, ws, k, #2(readSet))
            let vp : vproc = host_vproc
            let nurseryBase : long = vpload(NURSERY_BASE, vp)
            let limitPtr : long = vpload(LIMIT_PTR, vp)
            let lastAddr : any = (any) #1(readSet)
            let casted : ![any,item,item] = (![any,item,item])lastAddr
            if I64Gte(lastAddr, nurseryBase)
            then
                if I64Lt(lastAddr, limitPtr)
                then (*last item is still in nursery*)
                    do #2(casted) := newItem
                    do #1(readSet) := newItem
                    do #3(readSet) := I32Add(#3(readSet), 1)
                    return(readSet)
                else (*not in nursery, add last item to remember set*)
                    let rs : List.list = vpload(REMEMBER_SET, vp)
                    let address : any = (any) &2(casted)
                    let newRemSet : List.list = RSCONS(address, newItem, rs)
                    do vpstore(REMEMBER_SET, vp, newRemSet)
                    do #2(casted) := newItem
                    let newRS : read_set = alloc(#0(readSet), newItem, newItem, I32Add(#3(readSet), 1))
                    return(newRS)
            else (*not in nursery, add last item to remember set*)
                let rs : List.list = vpload(REMEMBER_SET, vp)
                let address : any = (any) &2(casted)
                let newRemSet : List.list = RSCONS(address, newItem, rs)
                do vpstore(REMEMBER_SET, vp, newRemSet)
                do #2(casted) := newItem
                let newRS : read_set = alloc(#0(readSet), newItem, newItem, I32Add(#3(readSet), 1))
                return(newRS)
        ;

        define @printHeader(tv:any) : () = 
            let header : any = ArrLoad(tv, ~1)
            do ccall M_Print_Long("Header is %lu\n", header)
            return();

        (*add a non checkpointed read to the read set*)
    	define @insert-without-k(tv:any, readSet : read_set / exh:exh) : read_set =
    		let newItem : item = WithoutK(tv, NilItem)
    		let vp : vproc = host_vproc
    		let nurseryBase : long = vpload(NURSERY_BASE, vp)
            let limitPtr : long = vpload(LIMIT_PTR, vp)
            let lastAddr : any = (any) #1(readSet)
            let casted : ![any,any,item] = (![any,any,item]) lastAddr
            if I64Gte(lastAddr, nurseryBase)
            then
                if I64Lt(lastAddr, limitPtr)
                then (*last item is still in nursery*)
                    do #2(casted) := newItem
                    do #1(readSet) := newItem
                    return(readSet)
                else (*not in nursery, add last item to remember set*)
                    do #2(casted) := newItem
                    let newRS : read_set = alloc(#0(readSet), newItem, #2(readSet), #3(readSet))
                    let rs : List.list = vpload(REMEMBER_SET, vp)
                    let address : any = (any) &2(casted)
                    let newRemSet : List.list = RSCONS(address, newItem, rs)
                    do vpstore(REMEMBER_SET, vp, newRemSet)
                    return(newRS)
            else (*not in nursery, add last item to remember set*)
                do #2(casted) := newItem
                let newRS : read_set = alloc(#0(readSet), newItem, #2(readSet), #3(readSet))
                let rs : List.list = vpload(REMEMBER_SET, vp)
                let address : any = (any) &2(casted)
                let newRemSet : List.list = RSCONS(address, newItem, rs)
                do vpstore(REMEMBER_SET, vp, newRemSet)
                return(newRS)
        ;
        
        define @new-w(x:unit / exh:exh) : read_set = 
            let firstElem : item = WithoutK(alloc(~1), NilItem)
            let rs : read_set = alloc(firstElem, firstElem, firstElem, 0)
            return(rs);

        define @insert-w(arg : [any, read_set] / exh:exh) : read_set = 
            let new : read_set = @insert-without-k(#0(arg), #1(arg) / exh)
            return(new);

        define @printRS(arg : [read_set, fun(any / exh -> unit)] / exh:exh) : unit =
            let rs : read_set = #0(arg)
            let f : fun(any / exh -> unit) = #1(arg)
            fun printLoop(i:item) : unit = 
                case i 
                   of NilItem => return(UNIT)
                    | WithoutK(hd:any, tl:item) => 
                        let _ : unit = apply f(hd / exh)
                        apply printLoop(tl)
                end
            apply printLoop(#0(rs));
    )
    val registerPrintFun : ('a -> unit) -> unit = _prim(@registerPrintFun)

    type 'a read_set = _prim(read_set)
    val new : unit -> 'a read_set = _prim(@new-w)
    val insert : 'a * 'a read_set -> 'a read_set = _prim(@insert-w)
    val printRS : 'a read_set * ('a -> unit) -> unit = _prim(@printRS)

end



