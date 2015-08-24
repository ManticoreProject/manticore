(* read-set.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Chronologically ordered read sets
 *)

 

structure NoRecOrderedReadSet = 
struct
    
#define NEXT 3
#define NEXTK 6
#define KPOINTER 5
#define HEAD 0
#define TAIL 1
#define LASTK 2
#define NUMK 3

#define START_TIMER let vp : vproc = host_vproc do ccall GenTimerStart(vp)
#define STOP_TIMER let vp : vproc = host_vproc do ccall GenTimerStop(vp)
   


	(*Careful, if this changes, we could possibly be indexing a "WithoutK" item 
     *incorrectly when filtering the read set*)
    datatype 'a item = Write of 'a * 'a * 'a | NilItem | WithK of 'a * 'a * 'a * 'a * 'a * 'a
                     | WithoutK of 'a * 'a * 'a | Abort of unit 


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
        extern void M_Print_Long2(void *, void *, void *);

    	typedef read_set = ![item,      (*0: first element of the read set*) 
    						 item, 	    (*1: last element of the read set*)
    						 item, 	    (*2: last checkpoint (element on short path)*)
    						 int];	    (*3: number of checkpoints in read set*)

        typedef mutWithK = ![any,    (*0: tag*)
                             any,    (*1: tvar*)
                             any,    (*2: contents read*)
                             item,   (*3: next pointer*)
                             any,    (*4: write set*)
                             any,    (*5: continuation*)
                             item];  (*6: next checkpoint pointer*)

        typedef stamp = VClock.stamp;
        typedef tvar = ![any, long, stamp]; (*contents, lock, version stamp*)

        define inline @get-stamp(/exh:exh) : stamp = 
            fun stampLoop() : long = 
                let current : long = VClock.@get(/exh)
                let lastBit : long = I64AndB(current, 1:long)
                if I64Eq(lastBit, 0:long)
                then return(current)
                else do Pause() apply stampLoop()
            let stamp : stamp = apply stampLoop()
            return(stamp)
        ;

        define @getPrintFunPtr = getPrintFunPtr;

        define @registerPrintFun(f : fun(any / exh -> unit) / exh:exh) : unit = 
            let funBox : ![fun(any / exh -> unit)] = @getPrintFunPtr(UNIT / exh)
            let f : fun(any / exh -> unit) = promote(f)
            do #0(funBox) := f
            return(UNIT);

    	define @new() : read_set = 
            let dummyTRef : ![any,long,long] = alloc(enum(0), 0:long, 0:long)
            let dummy : item = WithoutK(dummyTRef, enum(0), NilItem)
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

        define inline @abort(readSet : read_set, checkpoint : item, startStamp : ![stamp, int, int, long], count:int, revalidate : fun(item, item, int / -> ) / exh:exh) : () = 
            case checkpoint 
               of NilItem => (*no checkpoint available*)
                    let abortK : cont() = FLS.@get-key(ABORT_KEY / exh) 
                    throw abortK()
                | WithK(tv:tvar, _:any, _:item, ws:item, abortK:cont(any),_:item) => 
                    let casted : ![any, any, any, item] = (![any, any, any, item]) checkpoint
                    do #NEXT(casted) := NilItem
                    fun getLoop() : any = 
                        let t : long = VClock.@get(/exh)
                        if I64Eq(t, #0(startStamp))
                        then return(#0(tv))
                        else
                            let currentTime : stamp = @get-stamp(/exh)
                            do #0(startStamp) := currentTime
                            do apply revalidate(#HEAD(readSet), NilItem, 0)
                            apply getLoop()
                    let current : any = apply getLoop()
                    let newRS : read_set = alloc(#HEAD(readSet), checkpoint, checkpoint, count)
                    do FLS.@set-key(READ_SET, newRS / exh)
                    do FLS.@set-key(WRITE_SET, ws / exh)
                    let captureFreq : int = FLS.@get-counter2()
                    do FLS.@set-counter(captureFreq)
                    BUMP_PABORT
                    throw abortK(current)
            end
        ;

        define @validate(readSet : read_set, startStamp:![stamp, int, int, long] / exh:exh) : () = 
            fun validateLoopABCD(rs : item, abortInfo : item, count:int) : () =
                case rs 
                   of NilItem => (*finished validating*)
                        let currentTime : stamp = VClock.@get(/exh)
                        if I64Eq(currentTime, #0(startStamp))
                        then return() (*no one committed while validating*)
                        else  (*someone else committed, so revalidate*)
                            let currentTime : stamp = @get-stamp(/exh)
                            do #0(startStamp) := currentTime
                            apply validateLoopABCD(#HEAD(readSet), NilItem, 0)
                    | WithoutK(tv:tvar, x:any, next:item) =>
                        if Equal(#0(tv), x)
                        then apply validateLoopABCD(next, abortInfo, count)
                        else @abort(readSet, abortInfo, startStamp, count, validateLoopABCD / exh)
                    | WithK(tv:tvar,x:any,next:item,ws:item,abortK:any,_:item) =>
                        if Equal(#0(tv), x)
                        then 
                            if Equal(abortK, enum(0))
                            then apply validateLoopABCD(next, abortInfo, count)            (*update checkpoint*)
                            else apply validateLoopABCD(next, rs, I32Add(count, 1))
                        else
                            if Equal(abortK, enum(0))
                            then @abort(readSet, abortInfo, startStamp, count, validateLoopABCD / exh)
                            else @abort(readSet, rs, startStamp, count, validateLoopABCD / exh)
                end
            let currentTime : stamp = @get-stamp(/exh)
            do #0(startStamp) := currentTime
            apply validateLoopABCD(#HEAD(readSet), NilItem, 0)
        ;

        define inline @getNumK(rs : read_set) : int = return(#NUMK(rs));

        define @short-path-len(readSet : read_set / exh:exh) : () = 
            fun lenLoop(i:item, count:int) : int = 
                case i 
                   of NilItem => return(count)
                    | WithK(tv:tvar, _:any, _:item, ws:item, k:cont(any), next:item) => 
                        apply lenLoop(next, I32Add(count, 1))
                    | WithoutK(tv:tvar, _:any, next:item) => 
                        do ccall M_Print("short-path-len: Impossible\n") return(~1)
                end
            let l : int = apply lenLoop(#LASTK(readSet), 0)
            do ccall M_Print_Int("Short path length is %d\n", l)
            return();

        define inline @filterRS(readSet : read_set / exh : exh) : () = 
            fun dropKs(l:item, n:int) : int =   (*drop every other continuation*)
                case l
                   of NilItem => return(n)
                    | WithK(_:any,_:any,_:item,_:item,_:cont(any),next:item) =>
                        case next
                           of NilItem => return(n)
                            | WithK(_:any,_:any,_:item,_:item,_:cont(any),nextNext:item) =>
                                (* NOTE: if compiled with -debug, this will generate warnings
                                 * that we are updating a bogus local pointer, however, given the
                                 * nature of the data structure, we do preserve the heap invariants*)
                                let l : mutWithK = (mutWithK) l
                                let next : mutWithK = (mutWithK) next
                                do #KPOINTER(next) := enum(0):any
                                do #NEXTK(l) := nextNext
                                apply dropKs(nextNext, I32Sub(n, 1))
                            | _ => 
                                let casted : [any, any, any] = ([any, any, any])l
                                do ccall M_Print_Long("filterRS (inner case): Impossible, tag is %lu\n", #0(casted)) 
                                throw exh(Fail(@"filterRS: impossible"))
                        end
                    | _ => 
                        let casted : [any, any, any] = ([any, any, any])l
                        do ccall M_Print_Long("filterRS: Impossible, tag is %lu\n", #0(casted)) 
                        throw exh(Fail(@"filterRS: impossible"))
                end
            let x :int = apply dropKs(#LASTK(readSet), #NUMK(readSet))
            do #NUMK(readSet) := x
            return();

        define @checkHeader(tv:any) : () = 
            if I64Lt(tv, 50:long)
            then return()
            else
                let header : any = ArrLoad(tv, ~1)
                if I64Gt(header, 1048576)
                then
                    do ccall M_Print_Long("pointing to a forwarding pointer: %lu\n", header)
                    return()
                else return()
        ;            

        define @rs-len(readSet : read_set / exh:exh) : unit =
            fun lenLoop(i:item, count:int) : int = 
                case i 
                   of NilItem => return(count)
                    | WithK(tv:tvar, _:any, next:item, ws:item, k:cont(any), _:item) => 
                        apply lenLoop(next, I32Add(count, 1))
                    | WithoutK(tv:tvar, _:any, next:item) => 
                        do if I64Gt(next, 1000:long)
                            then
                                let nextHeader : any = ArrLoad(next, ~1)
                                if I64Eq(nextHeader, 196611)
                                then return()
                                else do ccall M_Print_Long("Current value is %d, next header is not right\n", #0(tv)) return()
                            else return()
                        
                        apply lenLoop(next, I32Add(count, 1))
                end
            let l : int = apply lenLoop(#HEAD(readSet), 0)
            do ccall M_Print_Int("Read set length is %d\n", l)
            return(UNIT);

        (*Note that these next two defines, rely on the fact that a heap limit check will not get
         *inserted within the body*)
        (*Add a checkpointed read to the read set*)
        define @insert-with-k(tv:any, v:any, k:cont(any), ws:item, readSet : read_set, stamp : ![long,int,int,long] / exh:exh) : () = 
            let newItem : item = WithK(tv, v, NilItem, ws, k, #LASTK(readSet))
            let vp : vproc = host_vproc
            let nurseryBase : long = vpload(NURSERY_BASE, vp)
            let limitPtr : long = vpload(LIMIT_PTR, vp)
            let lastAddr : any = (any) #TAIL(readSet)
            let casted : ![any,any,item,item] = (![any,any,item,item])lastAddr
            if I64Gte(lastAddr, nurseryBase)
            then
                if I64Lt(lastAddr, limitPtr)
                then (*last item is still in nursery*)
                    do #NEXT(casted) := newItem
                    do #TAIL(readSet) := newItem
                    do #LASTK(readSet) := newItem
                    do #NUMK(readSet) := I32Add(#NUMK(readSet), 1)
                    return()
                else (*not in nursery, add last item to remember set*)
                    let newRS : read_set = alloc(#HEAD(readSet), newItem, newItem, I32Add(#NUMK(readSet), 1))
                    let rs : any = vpload(REMEMBER_SET, vp)
                    let newRemSet : [![any,any,item,item], int, long, any] = alloc(casted, NEXT, #3(stamp), rs)
                    do vpstore(REMEMBER_SET, vp, newRemSet)
                    do #NEXT(casted) := newItem
                    do FLS.@set-key(READ_SET, newRS / exh)
                    return()
            else (*not in nursery, add last item to remember set*)
                let newRS : read_set = alloc(#HEAD(readSet), newItem, newItem, I32Add(#NUMK(readSet), 1))
                let rs : any = vpload(REMEMBER_SET, vp)
                let newRemSet : [![any,any,item,item], int, long, any] = alloc(casted, NEXT, #3(stamp), rs)
                do vpstore(REMEMBER_SET, vp, newRemSet)
                do #NEXT(casted) := newItem
                do FLS.@set-key(READ_SET, newRS / exh)
                return()
        ;

        define @printHeader(tv:any) : () = 
            let header : any = ArrLoad(tv, ~1)
            do ccall M_Print_Long("Header is %lu\n", header)
            return();

        (*add a non checkpointed read to the read set*)
    	define @insert-without-k(tv:any, v:any, readSet : read_set, stamp : ![long,int,int,long] / exh:exh) : () =
    		let newItem : item = WithoutK(tv, v, NilItem)
    		let vp : vproc = host_vproc
    		let nurseryBase : long = vpload(NURSERY_BASE, vp)
            let limitPtr : long = vpload(LIMIT_PTR, vp)
            let lastAddr : any = (any) #TAIL(readSet)
            let casted : ![any,any,any,item] = (![any,any,any,item]) lastAddr
            if I64Gte(lastAddr, nurseryBase)
            then
                if I64Lt(lastAddr, limitPtr)
                then (*last item is still in nursery*)
                    do #NEXT(casted) := newItem
                    do #TAIL(readSet) := newItem
                    return()
                else (*not in nursery, add last item to remember set*)
                    let newRS : read_set = alloc(#HEAD(readSet), newItem, #LASTK(readSet), #NUMK(readSet))
                    let rs : any = vpload(REMEMBER_SET, vp)
                    let newRemSet : [![any,any,item,item], int, long, any] = alloc(casted, NEXT, #3(stamp), rs)
                    do vpstore(REMEMBER_SET, vp, newRemSet)
                    do #NEXT(casted) := newItem
                    do FLS.@set-key(READ_SET, newRS / exh)
                    return()
            else (*not in nursery, add last item to remember set*)
                let newRS : read_set = alloc(#HEAD(readSet), newItem, #LASTK(readSet), #NUMK(readSet))
                let rs : any = vpload(REMEMBER_SET, vp)
                let newRemSet : [![any,any,item,item], int, long, any] = alloc(casted, NEXT, #3(stamp), rs)
                do vpstore(REMEMBER_SET, vp, newRemSet)
                do #NEXT(casted) := newItem
                do FLS.@set-key(READ_SET, newRS / exh)
                return()
        ;
        
    )
    val registerPrintFun : ('a -> unit) -> unit = _prim(@registerPrintFun)
end



