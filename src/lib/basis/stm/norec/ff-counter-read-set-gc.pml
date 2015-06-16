(* read-set.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Chronologically ordered read sets for NoRec
 *)

 

structure FFReadSetCounterGC = 
struct

#define TAG 0
#define TVAR 1
#define CONTENTS 2
#define NEXT 3
#define WRITESET 4
#define KPOINTER 5
#define NEXTK 6

#define HEAD 0
#define TAIL 1
#define LASTK 2
#define NUMK 3

#define START_TIMER let vp : vproc = host_vproc do ccall GenTimerStart(vp)
#define STOP_TIMER let vp : vproc = host_vproc do ccall GenTimerStop(vp)
   
(*                read set tag convention
 *  --------------------------------------------------| 
 *  | -- 32 bits -- | -- 31 bits -- | --    1 bit  -- |
 *  |   thread id   |remember count | with/without K  |
 *  --------------------------------------------------|
 * Note that the stored thread ID should already be shifted to the left by 32 bits
 * and that the count should be shifted 1 bit to the left, we can then increment
 * the count by adding 2 each time
 *)

#define NilItem enum(0):any
#define WithK(prevTag, tvar, contents, next, ws, k, nextK) alloc(I64OrB(prevTag, 1:long), tvar, contents, next, ws, k, nextK)
#define WithoutK(prevTag, tvar, contents, next) (item)alloc(I64AndB(prevTag, 18446744073709551614:long), tvar, contents, next)
#define TOWITHOUTK(tag) I64AndB(tag, 18446744073709551614:long)

#define GET_COUNT(tag) I64LSh(I64AndB(tag, 4294967295:long), 1:long)

#define Kind(tv) I64AndB(SELECT(0, tv), 1:long)

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

        extern void M_Print_Long2(void *, void *, void *);
        extern void M_IncCounter(void *, int , long);
        extern void * fastForward(void*, void*, void*, void*, void*, void*, void*, void*) __attribute__((alloc));
        extern void M_PruneRemSetAll(void*, long);
        extern void M_PrintAllocPtr(void*, void*) __attribute__((alloc));

    	typedef stamp = VClock.stamp;
        typedef tvar = ![any, long, long]; (*contents, lock, version stamp*)


        typedef item = ![long,    (*0: tag*)
                         tvar,    (*1: tvar*)
                         any,     (*2: contents read*)
                         any,     (*3: next pointer*)
                         any,     (*4: write set*)
                         any,     (*5: continuation*)
                         any];    (*6: next checkpoint pointer*)

        typedef read_set = ![item,      (*0: first element of the read set*) 
                             item,      (*1: last element of the read set*)
                             item,      (*2: last checkpoint (element on short path)*)
                             long];     (*3: number of checkpoints in read set*)


        
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

        define inline @cleanRemSetAll(threadId : long) : () =
            let vp : vproc = host_vproc
            do ccall M_PruneRemSetAll(vp, threadId)
            return();

        define @getPrintFunPtr = getPrintFunPtr;

        define @registerPrintFun(f : fun(any / exh -> unit) / exh:exh) : unit = 
            let funBox : ![fun(any / exh -> unit)] = @getPrintFunPtr(UNIT / exh)
            let f : fun(any / exh -> unit) = promote(f)
            do #0(funBox) := f
            return(UNIT);

    	define @new(id : long) : read_set = 
            let dummyTRef : ![any,long,long] = alloc(enum(0), 0:long, 0:long)
            let dummy : item = WithoutK(id, dummyTRef, enum(0), NilItem)
    		let rs : read_set = alloc(dummy, dummy, NilItem, 0:long)
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

        define inline @decCounts(readSet : read_set / exh : exh) : () = 
            fun decLoop(i:item) : () = 
                if Equal(i, NilItem)
                then return()
                else
                    case Kind(i)
                       of 1:long =>
                            let _ : long = I64FetchAndAdd(&1(#TVAR(i)), ~1:long)
                            apply decLoop(#NEXTK(i)) 
                        | _ => 
                            do ccall M_Print_Long("decCounts: impossible! tag is %lu\n", Kind(i)) 
                            throw exh(Fail(@"decCounts: impossible\n"))
                    end
            if Equal(readSet, enum(0))
            then return()
            else apply decLoop(#LASTK(readSet))
        ;

        define @short-path-len(readSet : read_set / exh:exh) : () = 
            fun lenLoop(i:item, count:long) : long = 
                if Equal(i, NilItem)
                then return(count)
                else
                    case Kind(i) 
                       of 1:long => apply lenLoop(#NEXTK(i), I64Add(count, 1:long))
                        | _ => 
                            do ccall M_Print("short-path-len: Impossible\n") 
                            return(0:long)
                    end
            let l : long = apply lenLoop(#LASTK(readSet), 0:long)
            if I64Eq(l, #NUMK(readSet))
            then return()
            else 
                do ccall M_Print_Long2("Short path length is %d, should be %d\n", l, #NUMK(readSet))
                return()
        ;

        define inline @incCounts(readSet : read_set, sentinel : item / exh : exh) : () =
            fun incLoop(shortPath : item, i:int) : () = 
                if Equal(shortPath, NilItem)
                then do ccall M_Print_Long("incCounts: This should be impossible, sentinel is %p\n", sentinel) return()
                else
                    case Kind(shortPath)
                       of 1:long => 
                            let _ : long = I64FetchAndAdd(&1(#TVAR(shortPath)), 1:long)
                            if Equal(#NEXTK(shortPath), sentinel)
                            then 
                                do #NEXTK(shortPath) := NilItem
                                return()
                            else apply incLoop(#NEXTK(shortPath), I32Add(i, 1))
                        | _ => 
                            do ccall M_Print_Long("incCounts: Impossible! tag is %lu\n", Kind(shortPath)) 
                            return()
                    end
            let lastK : item = #LASTK(readSet)
            if Equal(lastK, sentinel)
            then do FLS.@set-key(FF_KEY, enum(0) / exh) return()  (*no checkpoints after violation*)
            else 
                do apply incLoop(lastK, 1)   (*increment reference counts for checkpoints after violation*)
                FLS.@set-key(FF_KEY, readSet / exh)
        ;

        define @abortABCD(readSet : read_set, checkpoint : item, startStamp : ![stamp, int, int, long], count:long, revalidate : fun(item, item, long / -> ) / exh:exh) : () = 
            if Equal(checkpoint, NilItem)
            then
                (*<FF>*)
                let oldFFInfo : read_set = FLS.@get-key(FF_KEY / exh)
                do @decCounts(oldFFInfo / exh)
                do @incCounts(readSet, (item)NilItem / exh)
                (*</FF>*)
                let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                throw abortK()
            else
                do #NEXT(checkpoint) := NilItem
                fun getLoop() : any = 
                    let v : any = #0(#TVAR(checkpoint))
                    let t : long = VClock.@get(/exh)
                    if I64Eq(t, #0(startStamp))
                    then return(v)
                    else
                        let currentTime : stamp = @get-stamp(/exh)
                        do #0(startStamp) := currentTime
                        do apply revalidate(#HEAD(readSet), NilItem, 0:long)
                        apply getLoop()
                let current : any = apply getLoop()
                let newRS : read_set = alloc(#HEAD(readSet), checkpoint, checkpoint, count)
                do FLS.@set-key(READ_SET, newRS / exh)
                do FLS.@set-key(WRITE_SET, #WRITESET(checkpoint) / exh)
                (*<FF>*)
                let oldFFInfo : read_set = FLS.@get-key(FF_KEY / exh)
                do @decCounts(oldFFInfo / exh)
                do @incCounts(readSet, checkpoint / exh)
                (*</FF>*)
                let captureFreq : int = FLS.@get-counter2()
                do FLS.@set-counter(captureFreq)
                BUMP_PABORT
                let abortK : cont(any) = #KPOINTER(checkpoint)
                throw abortK(current)
        ;

        define @validate(readSet : read_set, startStamp:![stamp, int, int, long] / exh:exh) : () = 
            let vp : vproc = host_vproc
            fun validateLoopABCD(rs : item, abortInfo : item, count:long) : () =
                if Equal(rs, NilItem)
                then 
                    let currentTime : stamp = VClock.@get(/exh)
                    if I64Eq(currentTime, #0(startStamp))
                    then return() (*no one committed while validating*)
                    else  (*someone else committed, so revalidate*)
                        let currentTime : stamp = @get-stamp(/exh)
                        do #0(startStamp) := currentTime
                        apply validateLoopABCD(#HEAD(readSet), NilItem, 0:long)
                else
                    case Kind(rs) 
                       of 0:long => 
                            if Equal(#0(#TVAR(rs)), #CONTENTS(rs))
                            then apply validateLoopABCD(#NEXT(rs), abortInfo, count)
                            else @abortABCD(readSet, abortInfo, startStamp, count, validateLoopABCD / exh)
                        | _ => (*1:long*) 
                            if Equal(#0(#TVAR(rs)), #CONTENTS(rs))
                            then apply validateLoopABCD(#NEXT(rs), rs, I64Add(count, 1:long))
                            else @abortABCD(readSet, rs, startStamp, I64Add(count, 1:long), validateLoopABCD / exh)
                    end
            let currentTime : stamp = @get-stamp(/exh)
            do #0(startStamp) := currentTime
            do apply validateLoopABCD(#HEAD(readSet), NilItem, 0:long)
            return()
        ;

        define @ff-finish(readSet : read_set, checkpoint : item, i:long / exh:exh) : () =
            do #NEXT(checkpoint) := NilItem
            let newRS : read_set = alloc(#0(readSet), checkpoint, checkpoint, i)
            do FLS.@set-key(READ_SET, newRS / exh)
            do FLS.@set-key(WRITE_SET, #WRITESET(checkpoint) / exh)
            BUMP_KCOUNT
            let k : cont(any) = #KPOINTER(checkpoint)
            throw k(#CONTENTS(checkpoint))
        ;

        define @ff-validate(readSet : read_set, oldRS : item, myStamp : ![long,int,int,long] / exh:exh) : () = 
            fun ffLoop(rs:item, i:long, checkpoint : item) : () = 
                if Equal(rs, NilItem)
                then @ff-finish(readSet, checkpoint, i / exh)
                else
                    case Kind(rs) 
                       of 0:long => 
                            if Equal(#0(#TVAR(rs)), #CONTENTS(rs))
                            then apply ffLoop(#NEXT(rs), i, checkpoint)
                            else @ff-finish(readSet, checkpoint, i / exh)
                        | _ => (*1:long*)
                            if Equal(#0(#TVAR(rs)), #CONTENTS(rs))
                            then apply ffLoop(#NEXT(rs), I64Add(i, 1:long), rs)
                            else 
                                let newRS : read_set = alloc(#0(readSet), rs, rs, I64Add(i, 1:long))
                                fun getLoop() : any = 
                                    let v : any = #0(#TVAR(rs))
                                    let t : long = VClock.@get(/exh)
                                    if I64Eq(t, #0(myStamp))
                                    then return(v)
                                    else
                                        do @validate(newRS, myStamp / exh)
                                        apply getLoop()
                                do FLS.@set-key(READ_SET, newRS / exh)
                                do FLS.@set-key(WRITE_SET, #WRITESET(rs) / exh)
                                do #NEXT(rs) := NilItem
                                let current : any = apply getLoop()
                                let k : cont(any) = #KPOINTER(rs)
                                throw k(current)
                    end
            apply ffLoop(oldRS, #NUMK(readSet), oldRS)
        ;

        define @fast-forward(readSet : read_set, writeSet : any, tv:tvar, retK:cont(any), myStamp : ![long, int, int, long] / exh:exh) : () = 
            let ffInfo : read_set = FLS.@get-key(FF_KEY / exh)
            if Equal(ffInfo, enum(0))
            then return()
            else (*we should only allocate the checkRS closure if we are going to actually use it*)
                fun checkRS(rs:item, i:long) : () = 
                    if Equal(rs, NilItem)
                    then return()   
                    else 
                        INC_FF(1:long)
                        if Equal(tv, #TVAR(rs))
                        then (*tvars are equal*)
                            let res : int = ccall M_PolyEq(#KPOINTER(rs), retK)
                            if I32Eq(res, 1)
                            then (*continuations are equal*)
                                if Equal(#WRITESET(rs), writeSet) 
                                then (*continuations, write sets, and tvars are equal, fast forward...*)
                                    do @decCounts(ffInfo / exh)  (*decrement counts for everything on short path of ffInfo*)
                                    do FLS.@set-key(FF_KEY, enum(0) / exh)  (*null out fast forward info*)
                                    (*hook the two read sets together*)
                                    do #NEXTK(rs) := (any) #LASTK(readSet) 
                                    let currentLast : item = #TAIL(readSet)
                                    do #NEXT(currentLast) := (any)  rs
                                    (*add to remember set*)
                                    let vp : vproc = host_vproc
                                    let rememberSet : any = vpload(REMEMBER_SET, vp)
                                    let newRemSet : [item, int, any] = alloc(rs, NEXTK, rememberSet)
                                    do vpstore(REMEMBER_SET, vp, newRemSet)
                                    let currentCount : long = GET_COUNT(#TAG(currentLast))
                                    let ffCount : long = GET_COUNT(#TAG(rs))
                                    if I64Eq(currentCount, ffCount)
                                    then @ff-validate(readSet, rs, myStamp / exh)
                                    else @ff-validate(readSet, rs, myStamp / exh)
                                else apply checkRS(#NEXTK(rs), I64Add(i, 1:long))
                            else apply checkRS(#NEXTK(rs), I64Add(i, 1:long))
                        else apply checkRS(#NEXTK(rs), I64Add(i, 1:long))
                apply checkRS(#LASTK(ffInfo), 1:long)
        ;

        define inline @getNumK(rs : read_set) : long = return(#NUMK(rs));

        define inline @filterRS(readSet : read_set / exh : exh) : () = 
            let vp : vproc = host_vproc
            fun dropKs(l:item, n:long) : long =   (*drop every other continuation*)
                if Equal(l, NilItem)
                then return(n)
                else
                    let next : item = #NEXTK(l)
                    if Equal(next, NilItem)
                    then return(n)
                    else
                        let rs : any = vpload(REMEMBER_SET, vp)
                        let newRemSet : [item, int, any] = alloc(l, NEXTK, rs)
                        do vpstore(REMEMBER_SET, vp, newRemSet)
                        do #KPOINTER(next) := enum(0):any
                        do #TAG(next) := TOWITHOUTK(#TAG(next))
                        let nextNext : item = #NEXTK(next)
                        do #NEXTK(l) := (any) nextNext
                        apply dropKs(nextNext, I64Sub(n, 1:long))
            let x : long = apply dropKs(#LASTK(readSet), #NUMK(readSet))
            do #NUMK(readSet) := x
            return();

        (*Note that these next two defines, rely on the fact that a heap limit check will not get
         *inserted within the body*)
        (*Add a checkpointed read to the read set*)
        define @insert-with-k(tv:tvar, v:any, k:cont(any), ws:any, readSet : read_set / exh:exh) : () = 
            let newItem : item = WithK(#TAG(#TAIL(readSet)), tv, v, NilItem, ws, k, #LASTK(readSet))
            let vp : vproc = host_vproc
            let nurseryBase : long = vpload(NURSERY_BASE, vp)
            let limitPtr : long = vpload(LIMIT_PTR, vp)
            let last : item = #TAIL(readSet)
            let lastAddr : any = (any) last
            if I64Gte(lastAddr, nurseryBase)
            then
                if I64Lt(lastAddr, limitPtr)
                then (*last item is still in nursery*)
                    do #NEXT(last) := (any) newItem
                    do #TAIL(readSet) := newItem
                    do #LASTK(readSet) := newItem
                    do #NUMK(readSet) := I64Add(#NUMK(readSet), 1:long)
                    return()
                else (*not in nursery, add last item to remember set*)
                    let rs : any = vpload(REMEMBER_SET, vp)
                    let newRemSet : [item, int, any] = alloc(last, NEXT, rs)
                    do vpstore(REMEMBER_SET, vp, newRemSet)
                    let newRS : read_set = alloc(#HEAD(readSet), newItem, newItem, I64Add(#NUMK(readSet), 1:long))
                    do #0(newItem) := I64Add(#0(newItem), 2:long)
                    do #NEXT(last) := (any) newItem
                    do FLS.@set-key(READ_SET, newRS / exh)
                    return()
            else (*not in nursery, add last item to remember set*)
                let rs : any = vpload(REMEMBER_SET, vp)
                let newRemSet : [item, int, any] = alloc(last, NEXT, rs)
                do vpstore(REMEMBER_SET, vp, newRemSet)
                let newRS : read_set = alloc(#HEAD(readSet), newItem, newItem, I64Add(#NUMK(readSet), 1:long))
                do #0(newItem) := I64Add(#0(newItem), 2:long)
                do #NEXT(last) := (any) newItem
                do FLS.@set-key(READ_SET, newRS / exh)
                return()
        ;

        define @printHeader(tv:any) : () = 
            let header : any = ArrLoad(tv, ~1)
            do ccall M_Print_Long("Header is %lu\n", header)
            return();

        (*add a non checkpointed read to the read set*)
    	define @insert-without-k(tv:any, v:any, readSet : read_set / exh:exh) : () =
    		let newItem : item = WithoutK(#TAG(#TAIL(readSet)), tv, v, NilItem)
    		let vp : vproc = host_vproc
    		let nurseryBase : long = vpload(NURSERY_BASE, vp)
            let limitPtr : long = vpload(LIMIT_PTR, vp)
            let last : item = #TAIL(readSet)
            let lastAddr : any = (any) last
            if I64Gte(lastAddr, nurseryBase)
            then
                if I64Lt(lastAddr, limitPtr)
                then (*last item is still in nursery*)
                    do #NEXT(last) := (any) newItem
                    do #TAIL(readSet) := newItem
                    return()
                else (*not in nursery, add last item to remember set*)
                    let newRS : read_set = alloc(#HEAD(readSet), newItem, #LASTK(readSet), #NUMK(readSet))
                    let rs : any = vpload(REMEMBER_SET, vp)
                    let newRemSet : [item, int, any] = alloc(last, NEXT, rs)
                    do vpstore(REMEMBER_SET, vp, newRemSet)
                    do #0(newItem) := I64Add(#0(newItem), 2:long)
                    do #NEXT(last) := (any) newItem
                    do FLS.@set-key(READ_SET, newRS / exh)
                    return()
            else (*not in nursery, add last item to remember set*)
                let newRS : read_set = alloc(#HEAD(readSet), newItem, #LASTK(readSet), #NUMK(readSet))
                let rs : any = vpload(REMEMBER_SET, vp)
                let newRemSet : [item, int, any] = alloc(last, NEXT, rs)
                do vpstore(REMEMBER_SET, vp, newRemSet)
                do #NEXT(last) := (any) newItem
                do #0(newItem) := I64Add(#0(newItem), 2:long)
                do FLS.@set-key(READ_SET, newRS / exh)
                return()
        ;

    )
    val registerPrintFun : ('a -> unit) -> unit = _prim(@registerPrintFun)

    type 'a read_set = _prim(read_set)

end



