(* read-set.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Chronologically ordered read sets
 *)

 

structure ReadSet = 
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

    	typedef read_set = ![List.list, (*0: spine of the read set*) 
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
    		let rs : read_set = alloc(CONS(dummy, nil), dummy, NilItem, 0)
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

        define inline @abort(readSet : read_set, stamp : ![stamp]/ exh:exh) : () = 
            case #2(readSet)
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
                            BUMP_PABORT
                            do @logStat(alloc(#3(readSet)) / exh)
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

        define @validate(readSet : read_set, stamp : ![stamp], newStamp : stamp/ exh:exh) : () = 
            let rev : List.list = PrimList.@rev(#0(readSet) / exh)
            let rawStamp : stamp = #0(stamp)
            let count : ![int] = alloc(0)
            fun validateLoop(i : item, rest:List.list, checkpoint : item, newStamp : stamp, current : List.list) : () = 
                case i
                   of WithK(tv:tvar, next:item, ws:item, k:cont(any), _:item) =>
                        let casted : mutWithK = (mutWithK) i
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
                            then
                                apply validateLoop(next, rest, checkpoint, newStamp, current)
                            else
                                do #0(count) := I32Add(#0(count), 1)
                                apply validateLoop(next, rest, i, newStamp, current)
                        else
                            if Equal(k, enum(0))
                            then 
                                do #2(casted) := NilItem
                                do #0(stamp) := newStamp
                                let newRS : read_set = alloc(current, checkpoint, checkpoint, #0(count))
                                @abort(newRS, stamp / exh)
                            else 
                                do #2(casted) := NilItem
                                do #0(stamp) := newStamp
                                let newRS : read_set = alloc(current, i, i, I32Add(#0(count), 1))
                                @abort(newRS, stamp / exh)
                    | WithoutK(tv:tvar, next:item) =>
                        let casted : ![any, any, item] = (![any,any,item]) i
                        let valid : bool = 
                            if I64Eq(#1(tv), 0:long)
                            then
                                if I64Lt(#2(tv), rawStamp)
                                then return(true)
                                else return(false)
                            else return(false)
                        if(valid)
                        then apply validateLoop(next, rest, checkpoint, newStamp, current)
                        else 
                            do #2(casted) := NilItem
                            do #0(stamp) := newStamp
                            let newRS : read_set = alloc(current, checkpoint, checkpoint, #0(count))
                            @abort(newRS, stamp / exh)
                    | NilItem => 
                        case rest
                           of CONS(hd:item, tl:List.list) => apply validateLoop(hd, tl, checkpoint, newStamp, CONS(hd, current))
                            | nil => return()
                        end
                end
            do apply validateLoop(NilItem, rev, NilItem, newStamp, nil)
            return()
        ;

        define @validate-commit(readSet : read_set, stamp : ![stamp], newStamp : stamp, unlock : fun(unit / exh -> unit) / exh:exh) : () = 
            let rev : List.list = PrimList.@rev(#0(readSet) / exh)
            let rawStamp : stamp = #0(stamp)
            let count : ![int] = alloc(0)
            fun validateLoop(i : item, rest:List.list, checkpoint : item, newStamp : stamp, current : List.list) : () = 
                case i
                   of WithK(tv:tvar, next:item, ws:item, k:cont(any), _:item) =>
                        let casted : mutWithK = (mutWithK) i
                        if I64Lt(#2(tv), rawStamp)
                        then
                            if Equal(k, enum(0))
                            then
                                apply validateLoop(next, rest, checkpoint, newStamp, current)
                            else
                                do #0(count) := I32Add(#0(count), 1)
                                apply validateLoop(next, rest, i, newStamp, current)
                        else
                            if Equal(k, enum(0))
                            then 
                                let _ : unit = apply unlock(UNIT / exh)
                                do #2(casted) := NilItem
                                do #0(stamp) := newStamp
                                let newRS : read_set = alloc(current, checkpoint, checkpoint, #0(count))
                                @abort(newRS, stamp / exh)
                            else 
                                let _ : unit = apply unlock(UNIT / exh)
                                do #2(casted) := NilItem
                                do #0(stamp) := newStamp
                                let newRS : read_set = alloc(current, i, i, I32Add(#0(count), 1))
                                @abort(newRS, stamp / exh)
                    | WithoutK(tv:tvar, next:item) =>
                        let casted : ![any, any, item] = (![any,any,item]) i
                        if I64Lt(#2(tv), rawStamp)
                        then apply validateLoop(next, rest, checkpoint, newStamp, current)
                        else 
                            let _ : unit = apply unlock(UNIT / exh)
                            do #2(casted) := NilItem
                            do #0(stamp) := newStamp
                            let newRS : read_set = alloc(current, checkpoint, checkpoint, #0(count))
                            @abort(newRS, stamp / exh)
                    | NilItem => 
                        case rest
                           of CONS(hd:item, tl:List.list) => apply validateLoop(hd, tl, checkpoint, newStamp, CONS(hd, current))
                            | nil => return()
                        end
                end
            do apply validateLoop(NilItem, rev, NilItem, newStamp, nil)
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
                then
                    do #2(casted) := newItem
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
        ;

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
                then
                    do #2(casted) := newItem
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
        ;


    )
    val registerPrintFun : ('a -> unit) -> unit = _prim(@registerPrintFun)


end



