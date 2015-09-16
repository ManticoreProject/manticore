(* read-set.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Chronologically ordered read sets for NoRec
 *)

 

structure FFReadSetMergeWriteSets = 
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

(*read set tags*)

#define WITHKTAG 0:long
#define WITHOUTKTAG 1:long
#define LOCALTAG 2:long

#define START_TIMER let vp : vproc = host_vproc do ccall GenTimerStart(vp)
#define STOP_TIMER let vp : vproc = host_vproc do ccall GenTimerStop(vp)
   
#define NIL_ITEM enum(0):any

    datatype 'a witem = Write of 'a * 'a * 'a | NilItem

    _primcode(
        define @allocDummy(x : unit / exh:exh) : any = 
            let dummyTRef : [any] = alloc(enum(0))
            let dummyTRef : [any] = promote(dummyTRef)
            return(dummyTRef);
    )
    
    val allocDummy : unit -> 'a = _prim(@allocDummy)
    val dummy = allocDummy()
    fun getDummy() = dummy


    _primcode(

        extern void M_Print_Long2(void *, void *, void *);
        extern void M_IncCounter(void *, int , long);
        extern void * fastForward(void*, void*, void*, void*, void*, void*, void*, void*) __attribute__((alloc));
        extern void M_PruneRemSetAll(void*, long, void*);
        extern void M_PrintAllocPtr(void*, void*) __attribute__((alloc));
        extern void examineWriteSets(void*, void*);

    	typedef stamp = VClock.stamp;
        typedef tvar = ![any, long, long]; (*contents, lock, version stamp*)


        typedef item = ![long,    (*0: tag*)
                         tvar,    (*1: tvar*)
                         any,     (*2: contents read*)
                         any,     (*3: next pointer*)
                         witem,   (*4: write set*)
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

        define @getDummy = getDummy;

    	define @new() : read_set = 
            let dummyTRef : ![any,long,long] = alloc(enum(0), 0:long, 0:long)
            let dummy : item = (item)alloc(WITHOUTKTAG, dummyTRef, enum(0), NIL_ITEM)
    		let rs : read_set = alloc(dummy, dummy, NIL_ITEM, 0:long)
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

        define @check-rs(readSet : read_set, context : any / exh : exh) : () = 
            fun checkLoop(rs:item) : () =
                if Equal(rs, NIL_ITEM)
                then return ()
                else
                    case #TAG(rs) 
                       of WITHKTAG => do ccall M_Print("check-rs: found WITHKTAG\n") apply checkLoop(#NEXTK(rs))
                        | _ => 
                            do ccall M_Print_Long("check-rs: impossible! tag is %lu\n", #TAG(rs))
                            throw exh(Fail(@"check-rs: impossible\n"))
                    end
            do ccall M_Print_Long("%s: Checking...\n", context)
            do apply checkLoop(#LASTK(readSet))
            do ccall M_Print_Long("%s: Done checking...\n\n", context)
            return()
        ;

        define inline @decCounts(readSet : read_set / exh : exh) : () = 
            fun decLoop(i:item) : () = 
                if Equal(i, NIL_ITEM)
                then return()
                else
                    case #TAG(i)
                       of WITHKTAG =>
                            let _ : long = I64FetchAndAdd(&1(#TVAR(i)), ~1:long)
                            apply decLoop(#NEXTK(i)) 
                        | _ => 
                            do ccall M_Print_Long("decCounts: impossible! tag is %lu\n", #TAG(i))
                            throw exh(Fail(@"decCounts: impossible\n"))
                    end
            if Equal(readSet, enum(0))
            then return()
            else apply decLoop(#LASTK(readSet))
        ;

        define @short-path-len(readSet : read_set / exh:exh) : () = 
            fun lenLoop(i:item, count:long) : long = 
                if Equal(i, NIL_ITEM)
                then return(count)
                else
                    case #TAG(i)
                       of WITHKTAG => apply lenLoop(#NEXTK(i), I64Add(count, 1:long))
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
                if Equal(shortPath, NIL_ITEM)
                then do ccall M_Print_Long("incCounts: This should be impossible, sentinel is %p\n", sentinel) return()
                else
                    case #TAG(shortPath)
                       of WITHKTAG => 
                            let _ : long = I64FetchAndAdd(&1(#TVAR(shortPath)), 1:long)
                            if Equal(#NEXTK(shortPath), sentinel)
                            then 
                                do #NEXTK(shortPath) := NIL_ITEM
                                return()
                            else apply incLoop(#NEXTK(shortPath), I32Add(i, 1))
                        | _ => 
                            do ccall M_Print_Long("incCounts: Impossible! tag is %lu\n", #TAG(shortPath)) 
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
            if Equal(checkpoint, NIL_ITEM)
            then
                let oldFFInfo : read_set = FLS.@get-key(FF_KEY / exh)
                do @decCounts(oldFFInfo / exh)
                do @incCounts(readSet, (item)NIL_ITEM / exh)
                let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                throw abortK()
            else
                do #NEXT(checkpoint) := NIL_ITEM
                fun getLoop() : any = 
                    let v : any = #0(#TVAR(checkpoint))
                    let t : long = VClock.@get(/exh)
                    if I64Eq(t, #0(startStamp))
                    then return(v)
                    else
                        let currentTime : stamp = @get-stamp(/exh)
                        do #0(startStamp) := currentTime
                        do apply revalidate(#HEAD(readSet), NIL_ITEM, 0:long)
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
                if Equal(rs, NIL_ITEM)
                then 
                    let currentTime : stamp = VClock.@get(/exh)
                    if I64Eq(currentTime, #0(startStamp))
                    then return() (*no one committed while validating*)
                    else  (*someone else committed, so revalidate*)
                        let currentTime : stamp = @get-stamp(/exh)
                        do #0(startStamp) := currentTime
                        apply validateLoopABCD(#HEAD(readSet), NIL_ITEM, 0:long)
                else
                    case #TAG(rs)
                       of WITHOUTKTAG => 
                            if Equal(#0(#TVAR(rs)), #CONTENTS(rs))
                            then apply validateLoopABCD(#NEXT(rs), abortInfo, count)
                            else @abortABCD(readSet, abortInfo, startStamp, count, validateLoopABCD / exh)
                        | WITHKTAG => (*1:long*) 
                            if Equal(#0(#TVAR(rs)), #CONTENTS(rs))
                            then apply validateLoopABCD(#NEXT(rs), rs, I64Add(count, 1:long))
                            else @abortABCD(readSet, rs, startStamp, I64Add(count, 1:long), validateLoopABCD / exh)
                        | x:long => (*LOCALTAG: no need to validate local reads, only in fast forwarding*)
                            apply validateLoopABCD(#NEXT(rs), abortInfo, count)
                    end
            let currentTime : stamp = @get-stamp(/exh)
            do #0(startStamp) := currentTime
            do apply validateLoopABCD(#HEAD(readSet), NIL_ITEM, 0:long)
            return()
        ;

        define @ff-finish(readSet : read_set, checkpoint : item, i:long / exh:exh) : () =
            do #NEXT(checkpoint) := NIL_ITEM
            let newRS : read_set = alloc(#0(readSet), checkpoint, checkpoint, i)
            do FLS.@set-key(READ_SET, newRS / exh)
            do FLS.@set-key(WRITE_SET, #WRITESET(checkpoint) / exh)
            BUMP_KCOUNT
            let k : cont(any) = #KPOINTER(checkpoint)
            throw k(#CONTENTS(checkpoint))
        ;

        (*this should only be called if the write sets are equal!*)
        define @ff-validate(readSet : read_set, oldRS : item, myStamp : ![long,int,int,long] / exh:exh) : () = 
            fun ffLoop(rs:item, i:long, checkpoint : item) : () = 
                if Equal(rs, NIL_ITEM)
                then @ff-finish(readSet, checkpoint, i / exh)
                else
                    case #TAG(rs)
                       of WITHOUTKTAG => 
                            if Equal(#0(#TVAR(rs)), #CONTENTS(rs))
                            then apply ffLoop(#NEXT(rs), i, checkpoint)
                            else @ff-finish(readSet, checkpoint, i / exh)
                        | WITHKTAG => 
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
                                do #NEXT(rs) := NIL_ITEM
                                let current : any = apply getLoop()
                                let k : cont(any) = #KPOINTER(rs)
                                throw k(current)
                        | x:long => (*LOCALTAG*)
                            apply ffLoop(#NEXT(rs), i, checkpoint)  (*no need to check consistency because write sets match*)
                    end
            apply ffLoop(oldRS, #NUMK(readSet), oldRS)
        ;

        (*this should only be called if the write sets are NOT equal!*)
        define @ff-validate2(readSet : read_set, oldRS : item, myStamp : ![long,int,int,long], ws : witem / exh:exh) : () = 
            fun checkWS2(ws : witem, tv : tvar) : any = 
                case ws 
                   of Write(tv':tvar, x:any, next:witem) => 
                        if Equal(tv, tv')
                        then return(alloc(x))
                        else apply checkWS2(next, tv)
                    | NilItem => return(enum(0))
                end
            fun ffLoop(rs:item, i:long, checkpoint : item) : () = 
                if Equal(rs, NIL_ITEM)
                then @ff-finish(readSet, checkpoint, i / exh)
                else
                    case #TAG(rs)
                       of WITHOUTKTAG => 
                            let inWS : any = apply checkWS2(ws, #TVAR(rs))
                            if Equal(inWS, enum(0))
                            then (*still not a local read*)
                                if Equal(#0(#TVAR(rs)), #CONTENTS(rs))
                                then apply ffLoop(#NEXT(rs), i, checkpoint)
                                else @ff-finish(readSet, checkpoint, i / exh)
                            else @ff-finish(readSet, checkpoint, i / exh)
                        | WITHKTAG => 
                            let inWS : any = apply checkWS2(ws, #TVAR(rs))
                            if Equal(inWS, enum(0))
                            then 
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
                                    do #NEXT(rs) := NIL_ITEM
                                    let current : any = apply getLoop()
                                    let k : cont(any) = #KPOINTER(rs)
                                    throw k(current)
                            else
                                (*previous non-local read is now in write sets, abort here.*)
                                let newRS : read_set = alloc(#0(readSet), rs, rs, I64Add(i, 1:long))
                                do FLS.@set-key(READ_SET, newRS / exh)
                                do FLS.@set-key(WRITE_SET, #WRITESET(rs) / exh)
                                do #NEXT(rs) := NIL_ITEM
                                let k : cont(any) = #KPOINTER(rs)
                                let inWS : [any] = ([any]) inWS
                                throw k(#0(inWS))                                
                        | x:long => (*LOCALTAG*)
                            fun checkWS(ws:witem, tv:tvar) : () = 
                                case ws 
                                   of Write(tv':tvar,x:any,next:witem) => 
                                        if Equal(tv, tv')
                                        then 
                                            if Equal(#CONTENTS(rs), x) (*#CONTENTS(rs) is what we read in our previous execution.  x is what we currently have in our write set*)
                                            then apply ffLoop(#NEXT(rs), i, checkpoint)
                                            else @ff-finish(readSet, checkpoint, i / exh)
                                        else apply checkWS(next, tv)
                                    | NilItem => @ff-finish(readSet, checkpoint, i / exh)  (*previous local read is no longer a local read*)
                                end
                            apply checkWS(#WRITESET(rs), #TVAR(rs))
                    end
            apply ffLoop(oldRS, #NUMK(readSet), oldRS)
        ;

        extern void checkWS(void*, void*, void*) __attribute__((alloc));

        define @fast-forward(readSet : read_set, writeSet : witem, tv:tvar, retK:cont(any), myStamp : ![long, int, int, long] / exh:exh) : () = 
            let ffInfo : read_set = FLS.@get-key(FF_KEY / exh)
            if Equal(ffInfo, enum(0))
            then return()
            else 
                fun checkRSMergeWriteSets(rs:item, i:long) : () = 
                    INC_FF(1:long)
                    if Equal(rs, NIL_ITEM)
                    then return()   
                    else 
                        if Equal(tv, #TVAR(rs))
                        then (*tvars are equal*)
                            let res : int = ccall M_PolyEq(#KPOINTER(rs), retK)
                            if I32Eq(res, 1)
                            then (*continuations are equal*)
                                let oldWS : ![any, any, witem] = (![any, any, witem]) #WRITESET(rs)  (*tag, tvar, value, next*)
                                if Equal(oldWS, writeSet) 
                                then (*continuations, write sets, and tvars are equal, fast forward...*)
                                    do @decCounts(ffInfo / exh)  (*decrement counts for everything on short path of ffInfo*)
                                    do FLS.@set-key(FF_KEY, enum(0) / exh)  (*null out fast forward info*)
                                    (*hook the two read sets together*)
                                    do #NEXTK(rs) := (any) #LASTK(readSet) 
                                    let currentLast : item = #TAIL(readSet)
                                    do #NEXT(currentLast) := (any) rs
                                    (*add to remember set*)
                                    let vp : vproc = host_vproc
                                    let rememberSet : any = vpload(REMEMBER_SET, vp)
                                    let newRemSet : [item, int, long, any] = alloc(rs, NEXTK, #3(myStamp), rememberSet)
                                    do vpstore(REMEMBER_SET, vp, newRemSet)
                                    @ff-validate(readSet, rs, myStamp / exh)
                                else 
                                    do @decCounts(ffInfo / exh)  (*decrement counts for everything on short path of ffInfo*)
                                    do FLS.@set-key(FF_KEY, enum(0) / exh)  (*null out fast forward info*)
                                    (*hook the two read sets together*)
                                    do #NEXTK(rs) := (any) #LASTK(readSet)
                                    let currentLast : item = #TAIL(readSet)
                                    do #NEXT(currentLast) := (any) rs
                                    if Equal(oldWS, NilItem)
                                    then
                                        (*add to remember set*)
                                        let vp : vproc = host_vproc
                                        let rememberSet : any = vpload(REMEMBER_SET, vp)
                                        let newRemSet : any = (any)alloc(rs, NEXTK, #3(myStamp), rememberSet)
                                        do vpstore(REMEMBER_SET, vp, newRemSet)
                                        @ff-validate2(readSet, rs, myStamp, writeSet / exh)
                                    else
                                        (*hook write sets together*)
                                        let dummyTRef : any = @getDummy(UNIT / exh)
                                        do #0(oldWS) := dummyTRef (*no one should be able to match with this, but we will be able to write into it in the commit phase*)
                                        do #2(oldWS) := writeSet
                                        (*add to remember set*)
                                        let vp : vproc = host_vproc
                                        let rememberSet : any = vpload(REMEMBER_SET, vp)
                                        let newRemSet : any = (any)alloc(oldWS, 2, #3(myStamp), alloc(rs, NEXTK, #3(myStamp), rememberSet))
                                        do vpstore(REMEMBER_SET, vp, newRemSet)
                                        @ff-validate2(readSet, rs, myStamp, writeSet / exh)
                            else apply checkRSMergeWriteSets(#NEXTK(rs), I64Add(i, 1:long))
                        else apply checkRSMergeWriteSets(#NEXTK(rs), I64Add(i, 1:long))
                apply checkRSMergeWriteSets(#LASTK(ffInfo), 1:long)
        ;

        define inline @getNumK(rs : read_set) : long = return(#NUMK(rs));

        define inline @filterRS(readSet : read_set, stamp : ![long, int, int, long] / exh : exh) : () = 
            let vp : vproc = host_vproc
            fun dropKs(l:item, n:long) : long =   (*drop every other continuation*)
                if Equal(l, NIL_ITEM)
                then return(n)
                else
                    let next : item = #NEXTK(l)
                    if Equal(next, NIL_ITEM)
                    then return(n)
                    else
                        let rs : any = vpload(REMEMBER_SET, vp)
                        let newRemSet : [item, int, long, any] = alloc(l, NEXTK, #3(stamp), rs)
                        do vpstore(REMEMBER_SET, vp, newRemSet)
                        do #KPOINTER(next) := enum(0):any
                        do #TAG(next) := WITHOUTKTAG
                        let nextNext : item = #NEXTK(next)
                        do #NEXTK(l) := (any) nextNext
                        apply dropKs(nextNext, I64Sub(n, 1:long))
            let x : long = apply dropKs(#LASTK(readSet), #NUMK(readSet))
            do #NUMK(readSet) := x
            return();

        (*Note that these next two defines, rely on the fact that a heap limit check will not get
         *inserted within the body*)
        (*Add a checkpointed read to the read set*)
        define @insert-with-k(tv:tvar, v:any, k:cont(any), ws:any, readSet : read_set, stamp : ![long,int,int,long] / exh:exh) : () = 
            let newItem : item = alloc(WITHKTAG, tv, v, NIL_ITEM, ws, k, #LASTK(readSet))
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
                    let newRemSet : [item, int, long, any] = alloc(last, NEXT, #3(stamp), rs)
                    do vpstore(REMEMBER_SET, vp, newRemSet)
                    let newRS : read_set = alloc(#HEAD(readSet), newItem, newItem, I64Add(#NUMK(readSet), 1:long))
                    do #NEXT(last) := (any) newItem
                    do FLS.@set-key(READ_SET, newRS / exh)
                    return()
            else (*not in nursery, add last item to remember set*)
                let rs : any = vpload(REMEMBER_SET, vp)
                let newRemSet : [item, int, long, any] = alloc(last, NEXT, #3(stamp), rs)
                do vpstore(REMEMBER_SET, vp, newRemSet)
                let newRS : read_set = alloc(#HEAD(readSet), newItem, newItem, I64Add(#NUMK(readSet), 1:long))
                do #NEXT(last) := (any) newItem
                do FLS.@set-key(READ_SET, newRS / exh)
                return()
        ;

        define @printHeader(tv:any) : () = 
            let header : any = ArrLoad(tv, ~1)
            do ccall M_Print_Long("Header is %lu\n", header)
            return();

        (*add a non checkpointed read to the read set*)
    	define @insert-without-k(tv:any, v:any, readSet : read_set, stamp : ![long, int, int, long] / exh:exh) : () =
    		let newItem : item = (item)alloc(WITHOUTKTAG, tv, v, NIL_ITEM)
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
                    let newRemSet : [item, int, long, any] = alloc(last, NEXT, #3(stamp), rs)
                    do vpstore(REMEMBER_SET, vp, newRemSet)
                    do #NEXT(last) := (any) newItem
                    do FLS.@set-key(READ_SET, newRS / exh)
                    return()
            else (*not in nursery, add last item to remember set*)
                let newRS : read_set = alloc(#HEAD(readSet), newItem, #LASTK(readSet), #NUMK(readSet))
                let rs : any = vpload(REMEMBER_SET, vp)
                let newRemSet : [item, int, long, any] = alloc(last, NEXT, #3(stamp), rs)
                do vpstore(REMEMBER_SET, vp, newRemSet)
                do #NEXT(last) := (any) newItem
                do FLS.@set-key(READ_SET, newRS / exh)
                return()
        ;

        (*add a non checkpointed read to the read set*)
        define @insert-local-read(tv:any, v:any, readSet : read_set, stamp : ![long, int, int, long], ws : witem / exh:exh) : () =
            let newItem : item = (item)alloc(LOCALTAG, tv, v, NIL_ITEM, ws)
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
                    let newRemSet : [item, int, long, any] = alloc(last, NEXT, #3(stamp), rs)
                    do vpstore(REMEMBER_SET, vp, newRemSet)
                    do #NEXT(last) := (any) newItem
                    do FLS.@set-key(READ_SET, newRS / exh)
                    return()
            else (*not in nursery, add last item to remember set*)
                let newRS : read_set = alloc(#HEAD(readSet), newItem, #LASTK(readSet), #NUMK(readSet))
                let rs : any = vpload(REMEMBER_SET, vp)
                let newRemSet : [item, int, long, any] = alloc(last, NEXT, #3(stamp), rs)
                do vpstore(REMEMBER_SET, vp, newRemSet)
                do #NEXT(last) := (any) newItem
                do FLS.@set-key(READ_SET, newRS / exh)
                return()
        ;

    )
    type 'a read_set = _prim(read_set)

end



