(* read-set.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Chronologically ordered read sets for NoRec
 *)

 

structure FFReadSetMergeWriteSets = 
struct

#define NEXT 3
#define READ_VAL 2
#define NEXTK 6
#define KPOINTER 5
#define READ_VAL 2

#define HEAD 0
#define TAIL 1
#define LASTK 2
#define NUMK 3

#define START_TIMER let vp : vproc = host_vproc do ccall GenTimerStart(vp)
#define STOP_TIMER let vp : vproc = hos

    datatype 'a item = Write of 'a * 'a * 'a | NilItem | WithK of 'a * 'a * 'a * 'a * 'a * 'a
                     | WithoutK of 'a * 'a * 'a | Abort of unit | Local of 'a * 'a * 'a * 'a (*tvar, read value, next, write set*)

    _primcode(
        define @allocDummy(x : unit / exh:exh) : any = 
            let dummyTRef : [any, long] = alloc(enum(123), 0:long)
            let dummyTRef : [any, long] = promote(dummyTRef)
            return(dummyTRef);

        define @print-tags(x:unit / exh:exh) : unit = 
            let withK : item = WithK(enum(0), enum(0), enum(0), enum(0), enum(0), enum(0))
            let withoutK : item = WithoutK(enum(0), enum(0), enum(0))
            let loc : item = Local(enum(0), enum(0), enum(0), enum(0))
            do ccall M_Print_Long2("WithK tag is %lu\nWithoutK tag is %lu\n", #0(([any])withK), #0(([any])withoutK))
            do ccall M_Print_Long("Local tag is %lu\n", #0(([any])loc))
            return(UNIT);
    )
    
    val allocDummy : unit -> 'a = _prim(@allocDummy)
    val dummy = allocDummy()
    fun getDummy() = dummy
    val printTags : unit -> unit = _prim(@print-tags)
    (*val _ = printTags()*)

    _primcode(

        extern void M_Print_Long2(void *, void *, void *);
        extern void M_IncCounter(void *, int , long);
        extern void M_Debug(void*);
        extern void M_Print_Int_Long2(void*, int, long, long);
        extern void * fastForward(void*, void*, void*, void*, void*, void*, void*, void*, void*) __attribute__((alloc));
        extern void * STM_Validate(void*, void *, void *, void *) __attribute__((alloc));

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
        typedef tvar = FullAbortSTM.tvar; (*contents, lock, version stamp*)

        define @getDummy = getDummy;

        (* Put a checkpoint as the first item, that isn't on the short path.  
         * This way when we validate, we will always have a checkpoint to abort to
         * which generalizes the abort process since there is no longer a distinction
         * between full and partial aborts.  Also, we need to have a dummy node at
         * the beginning for ordered read sets anyway
         *)
        define @new(abortK : cont(any)) : read_set = 
            let dummyTRef : ![any,long,long] = alloc(enum(0), 0:long, 0:long)
            let dummy : item = WithK(dummyTRef, enum(0), NilItem, NilItem, abortK, NilItem)
            let rs : read_set = alloc(dummy, dummy, NilItem, 0)
            return(rs)
        ;

        define inline @decCounts(readSet : read_set / exh : exh) : () = 
            let vp : vproc = host_vproc
            let id : int = VProc.@vproc-id(vp)
            fun decLoop(i:item) : () = 
                case i 
                   of NilItem => return()
                    | WithK(tv:tvar, _:any, _:item, _:item, _:cont(any),next:item) => 
                        let old : long = I64FetchAndAdd(&1(tv), ~1:long)
                        apply decLoop(next)
                end
            if Equal(readSet, enum(0))
            then return()
            else do apply decLoop(#LASTK(readSet))
                 return()
        ;   

        define inline @incCounts(readSet : read_set, sentinel : item / exh : exh) : () =
            let vp : vproc = host_vproc
            let id : int = VProc.@vproc-id(vp)
            fun incLoop(shortPath : item, i:int) : () = 
                case shortPath 
                   of NilItem => return()
                    | WithK(tv:tvar, _:any, _:item, _:item, _:cont(any),next:item) => 
                        let old : long = I64FetchAndAdd(&1(tv), 1:long)
                        if Equal(next, sentinel)
                        then 
                            let casted : mutWithK = (mutWithK) shortPath
                            do #NEXTK(casted) := NilItem
                            return()
                        else apply incLoop(next, I32Add(i, 1))
                    | _ => 
                        let casted : [any] = ([any]) shortPath
                        do ccall M_Print_Long("incCounts: Impossible! tag is %lu\n", #0(casted))
                        return()
                end
            let lastK : item = #LASTK(readSet)
            if Equal(lastK, sentinel)
            then do FLS.@set-key(FF_KEY, enum(0) / exh) return()  (*no checkpoints after violation*)
            else 
                do apply incLoop(lastK, 1)   (*increment reference counts for checkpoints after violation*)
                FLS.@set-key(FF_KEY, readSet / exh)
        ;

        define @abortABCD(readSet : read_set, checkpoint : item, startStamp : ![stamp, int, int, long], count:int, 
                          revalidate : fun(item, item, int / -> ), eager : bool/ exh:exh) : () = 
            do ccall M_Print("Aborting\n")
            case checkpoint 
               of NilItem => (*no checkpoint available*)
                    let oldFFInfo : read_set = FLS.@get-key(FF_KEY / exh)
                    do @decCounts(oldFFInfo / exh)
                    do @incCounts(readSet, NilItem / exh)
#ifdef EVENT_LOGGING
                    do 
                        if(eager)
                        then Logging.@log-eager-full-abort() 
                        else Logging.@log-commit-full-abort() 
#endif
                    let abortK : cont(any) = FLS.@get-key(ABORT_KEY / exh)
                    throw abortK(UNIT)
                | WithK(tv:tvar, _:any, next:item, ws:item, abortK:cont(any),_:item) => 
                    let casted : ![any, any, any, item] = (![any, any, any, item]) checkpoint
                    do #NEXT(casted) := NilItem
                    fun getLoop() : any = 
                        let v : any = #0(tv)
                        let t : long = VClock.@get(/exh)
                        if I64Eq(t, #0(startStamp))
                        then return(v)
                        else
                            let currentTime : stamp = NoRecOrderedReadSet.@get-stamp(/exh)
                            do #0(startStamp) := currentTime
                            do apply revalidate(#HEAD(readSet), NilItem, 0)
                            apply getLoop()
                    let current : any = apply getLoop()
                    do #READ_VAL(casted) := current
#ifdef EVENT_LOGGING
                    let freq : int = FLS.@get-counter2()
                    let skipped : int = I32Mul(count, freq)
                    do if(eager)
                        then Logging.@log-eager-partial-abort(skipped) 
                        else Logging.@log-commit-partial-abort(skipped) 
#endif
                    let newRS : read_set = alloc(#HEAD(readSet), checkpoint, checkpoint, count)
                    do FLS.@set-key(READ_SET, newRS / exh)
                    do FLS.@set-key(WRITE_SET, ws / exh)
                    let oldFFInfo : read_set = FLS.@get-key(FF_KEY / exh)
                    do @decCounts(oldFFInfo / exh)
                    do @incCounts(readSet, checkpoint / exh)
                    let captureFreq : int = FLS.@get-counter2()
                    do FLS.@set-counter(captureFreq)
                    BUMP_PABORT
                    throw abortK(current)
            end
        ;

        define @c-validate(readSet : read_set, startStamp:![stamp, int, int, long], eager : bool / exh:exh) : () = 
            let vp : vproc = host_vproc
            let clock : ![long] = VClock.@get-boxed(/exh)
            let res : read_set = ccall STM_Validate(startStamp, clock, readSet, vp)
            if Equal(res, UNIT)
            then return() 
            else 
                let chkpnt : item = #LASTK(res)
                let casted : mutWithK = (mutWithK) chkpnt
                do FLS.@set-key(READ_SET, res / exh)
                do FLS.@set-key(WRITE_SET, #WRITE_SET(casted) / exh)
                let oldFFInfo : read_set = FLS.@get-key(FF_KEY / exh)
                do @decCounts(oldFFInfo / exh)
                do @incCounts(readSet, chkpnt / exh)
                let captureFreq : int = FLS.@get-counter2()
                do FLS.@set-counter(captureFreq)
                let abortK : cont(any) = (cont(any)) #KPOINTER(casted)
                throw abortK(#READ_VAL(casted))
        ;

        define @validate(readSet : read_set, startStamp:![stamp, int, int, long], eager : bool / exh:exh) : () = 
            fun validateLoopABCD(rs : item, abortInfo : item, count:int) : () =
                case rs 
                   of NilItem => (*finished validating*)
                        let currentTime : stamp = VClock.@get(/exh)
                        if I64Eq(currentTime, #0(startStamp))
                        then return() (*no one committed while validating*)
                        else  (*someone else committed, so revalidate*)
                            let currentTime : stamp = NoRecOrderedReadSet.@get-stamp(/exh)
                            do #0(startStamp) := currentTime
                            apply validateLoopABCD(#HEAD(readSet), NilItem, 0)
                    | Local(tv:tvar, x:any, next : item, ws:item) => apply validateLoopABCD(next, abortInfo, count)
                    | WithoutK(tv:tvar, x:any, next:item) =>
                        if Equal(#0(tv), x)
                        then apply validateLoopABCD(next, abortInfo, count)
                        else @abortABCD(readSet, abortInfo, startStamp, count, validateLoopABCD, eager / exh)
                    | WithK(tv:tvar,x:any,next:item,ws:item,abortK:any,_:item) =>
                        if Equal(#0(tv), x)
                        then 
                            if Equal(abortK, enum(0))
                            then apply validateLoopABCD(next, abortInfo, count)            (*update checkpoint*)
                            else apply validateLoopABCD(next, rs, I32Add(count, 1))
                        else
                            if Equal(abortK, enum(0))
                            then @abortABCD(readSet, abortInfo, startStamp, count, validateLoopABCD, eager / exh)
                            else @abortABCD(readSet, rs, startStamp, I32Add(count, 1), validateLoopABCD, eager / exh)
                end
            let currentTime : stamp = NoRecOrderedReadSet.@get-stamp(/exh)
            do #0(startStamp) := currentTime
            apply validateLoopABCD(#HEAD(readSet), NilItem, 0)
        ;

        define @ff-finish(readSet : read_set, checkpoint : item, i:int, j:int / exh:exh) : () =
            case checkpoint 
               of WithK(tv:tvar,x:any,_:item,ws:item,k:cont(any),next:item) => 
                    do Logging.@log-ff(j)
                    let casted : mutWithK = (mutWithK) checkpoint
                    do #NEXT(casted) := NilItem
                    let newRS : read_set = alloc(#0(readSet), checkpoint, checkpoint, i)
                    do FLS.@set-key(READ_SET, newRS / exh)
                    do FLS.@set-key(WRITE_SET, ws / exh)
                    let freq : int = FLS.@get-counter2()
                    do FLS.@set-counter(freq)
                    BUMP_KCOUNT
                    throw k(x)
                | _ => throw exh(Fail(@"Impossible: ff-finish\n"))
            end
        ;

        define @ff-validate(readSet : read_set, oldRS : item, myStamp : ![long,int,int,long] / exh:exh) : () = 
            fun ffLoop(rs:item, i:int, checkpoint : item, j:int) : () = 
                case rs
                   of NilItem => @ff-finish(readSet, checkpoint, i, j / exh)
                    | WithoutK(tv:tvar, x:any, next:item) => 
                        if Equal(#0(tv), x)
                        then apply ffLoop(next, i, checkpoint, I32Add(j, 1))
                        else @ff-finish(readSet, checkpoint, i, j / exh)
                    | Local(tv:tvar, x:any, next:item, ws:item) => apply ffLoop(next, i, checkpoint, I32Add(j, 1))
                    | WithK(tv:tvar,x:any,next:item,ws:item,k:cont(any),_:item) => 
                        if Equal(#0(tv), x)
                        then
                            if Equal(k, enum(0))
                            then apply ffLoop(next, i, checkpoint, I32Add(j, 1))
                            else apply ffLoop(next, I32Add(i, 1), rs, I32Add(j, 1))
                        else 
                            if Equal(k, enum(0))
                            then @ff-finish(readSet, checkpoint, i, j / exh)
                            else
                                let newRS : read_set = alloc(#0(readSet), rs, rs, I32Add(i, 1))
                                fun getLoop() : any = 
                                    let v : any = #0(tv)
                                    let t : long = VClock.@get(/exh)
                                    if I64Eq(t, #0(myStamp))
                                    then return(v)
                                    else
                                        do @validate(newRS, myStamp, true / exh)
                                        apply getLoop()
                                do FLS.@set-key(READ_SET, newRS / exh)
                                do FLS.@set-key(WRITE_SET, ws / exh)
                                let casted : mutWithK = (mutWithK) rs
                                do #NEXT(casted) := NilItem
                                let current : any = apply getLoop()
                                BUMP_KCOUNT
                                do Logging.@log-ff(j)
                                throw k(current)
                end
            apply ffLoop(oldRS, #NUMK(readSet), oldRS, 0)
        ;

        define @ff-validate2(readSet : read_set, oldRS : item, myStamp : ![long,int,int,long] / exh:exh) : () = 
            fun checkLocal(tv:tvar, x:any, ws:item) : bool = 
                case ws 
                   of NilItem => return(false)
                    | Write(tv':tvar, x':any, next:item) => 
                        if Equal(tv, tv')
                        then 
                            if Equal(x, x')
                            then return(true)
                            else return(false)
                        else apply checkLocal(tv, x, next)
                end
            fun ffLoop(rs:item, i:int, checkpoint : item, j:int) : () = 
                case rs
                   of NilItem => @ff-finish(readSet, checkpoint, i, j / exh)
                    | WithoutK(tv:tvar, x:any, next:item) => 
                        if Equal(#0(tv), x)
                        then apply ffLoop(next, i, checkpoint, I32Add(j, 1))
                        else @ff-finish(readSet, checkpoint, i, j / exh)
                    | Local(tv:tvar, x:any, next:item, ws:item) => 
                        let res : bool = apply checkLocal(tv, x, ws)
                        if(res)
                        then apply ffLoop(next, i, checkpoint, I32Add(j, 1))
                        else @ff-finish(readSet, checkpoint, i, j / exh) 
                    | WithK(tv:tvar,x:any,next:item,ws:item,k:cont(any),_:item) => 
                        if Equal(#0(tv), x)
                        then
                            if Equal(k, enum(0))
                            then apply ffLoop(next, i, checkpoint, I32Add(j, 1))
                            else apply ffLoop(next, I32Add(i, 1), rs, I32Add(j, 1))
                        else 
                            if Equal(k, enum(0))
                            then @ff-finish(readSet, checkpoint, i, j / exh)
                            else
                                let newRS : read_set = alloc(#0(readSet), rs, rs, I32Add(i, 1))          
                                fun getLoop() : any = 
                                    let v : any = #0(tv)
                                    let t : long = VClock.@get(/exh)
                                    if I64Eq(t, #0(myStamp))
                                    then return(v)
                                    else
                                        do @validate(newRS, myStamp, true / exh)
                                        apply getLoop()
                                do FLS.@set-key(READ_SET, newRS / exh)
                                do FLS.@set-key(WRITE_SET, ws / exh)
                                let casted : mutWithK = (mutWithK) rs
                                do #NEXT(casted) := NilItem
                                let current : any = apply getLoop()
                                BUMP_KCOUNT
                                do Logging.@log-ff(j)
                                throw k(current)
                end
            apply ffLoop(oldRS, #NUMK(readSet), oldRS, 0)
        ;

        define inline @hook-readsets(ffInfo : read_set, rs : item, readSet : read_set, myStamp : ![long,int,int,long] / exh:exh) : () = 
            do @decCounts(ffInfo / exh)
            do FLS.@null-key(FF_KEY)
            let ffFirstK : mutWithK = (mutWithK) rs
            let vp : vproc = host_vproc
            let rememberSet : any = vpload(REMEMBER_SET, vp)
            let currentLast : item = #TAIL(readSet)
            let newRemSet : [mutWithK, int, long, any] = alloc(currentLast, NEXT, #3(myStamp), alloc(ffFirstK, NEXTK, #3(myStamp), rememberSet))
            do vpstore(REMEMBER_SET, vp, newRemSet)
            do #NEXTK(ffFirstK) := #LASTK(readSet)
            let currentLast : mutWithK = (mutWithK) currentLast
            do #NEXT(currentLast) := rs
            return()
        ;

        define inline @merge-writesets(writeSet : item, oldWS : any, myStamp : ![long,int,int,long] / exh : exh) : () = 
            let oldWS : ![any, tvar, any, item] = (![any, tvar, any, item]) oldWS
            let dummyTRef : any = @getDummy(UNIT / exh)
            do #1(oldWS) := dummyTRef 
            do #3(oldWS) := writeSet
            let vp : vproc = host_vproc
            let rememberSet : any = vpload(REMEMBER_SET, vp)
            let newRemSet : any = (any)alloc(oldWS, 3, #3(myStamp), rememberSet)
            do vpstore(REMEMBER_SET, vp, newRemSet)
            return()
        ;

        define @fast-forward2(readSet : read_set, writeSet : item, tv:tvar, retK:cont(any), myStamp : ![long, int, int, long] / exh:exh) : () = 
            let ffInfo : read_set = FLS.@get-key(FF_KEY / exh)
            if Equal(ffInfo, enum(0))
            then return()
            else 
                let vp : vproc = host_vproc
                let clock : ![long] = VClock.@get-boxed(/exh)
                INC_FF(1:long) 
                let dummy : any = @getDummy(UNIT / exh)
                let res : any = ccall fastForward(readSet, ffInfo, writeSet, tv, retK, myStamp, clock, vp, dummy)
                if Equal(res, UNIT)
                then return()
                else 
                    BUMP_KCOUNT
                    let rs : read_set = (read_set) res
                    do FLS.@null-key(FF_KEY)
                    do FLS.@set-key(READ_SET, rs / exh)
                    let chkpnt : item = #LASTK(rs)
                    case chkpnt 
                       of WithK(tv:tvar,x:any,_:item,ws:item,k:cont(any),next:item) => 
                            do FLS.@set-key(WRITE_SET, ws / exh)
                            throw k(x)
                        | _ => return()
                    end
        ;

        define @match-writesets(ws : item, writeSet : item, ffInfo : read_set, rs : item, readSet : read_set, myStamp : ![long,int,int,long]/ exh:exh) : () = 
            fun keySubset(oldWS : item, newWS : item) : () =
                case oldWS 
                    of Write(tv:tvar, x:any, next:item) => 
                        case newWS 
                           of NilItem => 
                                do @hook-readsets(ffInfo, rs, readSet, myStamp / exh)
                                do @merge-writesets(writeSet, ws, myStamp / exh)
                                @ff-validate2(readSet, rs, myStamp / exh)
                            | Write(tv':tvar,x':any,next':item) => 
                                if Equal(tv,tv')
                                then apply keySubset(next, next')
                                else apply keySubset(next, newWS)
                        end
                     | NilItem => 
                        case newWS 
                           of NilItem => 
                                do @hook-readsets(ffInfo, rs, readSet, myStamp / exh)
                                do @merge-writesets(writeSet, ws, myStamp / exh)
                                @ff-validate2(readSet, rs, myStamp / exh)
                            | _ => return()
                        end
                end
            fun eqModuloSpine(oldWS : item, newWS : item, eq:bool) : () = 
                case oldWS
                   of Write(tv:tvar,x:any,next:item) => 
                        case newWS 
                           of NilItem => 
                                do @hook-readsets(ffInfo, rs, readSet, myStamp / exh)
                                do @merge-writesets(writeSet, ws, myStamp / exh)
                                @ff-validate2(readSet, rs, myStamp / exh)  (*key subset*) 
                            | Write(tv':tvar,x':any,next':item) => 
                                if Equal(tv, tv')
                                then 
                                    if Equal(x, x')
                                    then apply eqModuloSpine(next, next', eq)
                                    else apply eqModuloSpine(next, next', false)
                                else apply keySubset(next, newWS)  (*try and match subsequence*)
                        end
                    | NilItem => 
                        case newWS 
                           of NilItem => 
                                if(eq)
                                then 
                                    do @hook-readsets(ffInfo, rs, readSet, myStamp / exh)
                                    @ff-validate(readSet, rs, myStamp / exh)  (*equal*)
                                else 
                                    do @merge-writesets(writeSet, ws, myStamp / exh)
                                    do @hook-readsets(ffInfo, rs, readSet, myStamp / exh)
                                    @ff-validate2(readSet, rs, myStamp / exh) (*key equal*)
                            | _ => return()
                        end
                end
            do apply eqModuloSpine(ws, writeSet, true) 
            do @decCounts(ffInfo / exh)
            do FLS.@null-key(FF_KEY)
            return()
        ;

        define @fast-forward(readSet : read_set, writeSet : item, tv:tvar, retK:cont(any), myStamp : ![long, int, int, long] / exh:exh) : () = 
            let ffInfo : read_set = FLS.@get-key(FF_KEY / exh)
            if Equal(ffInfo, enum(0))
            then return()
            else (*we should only allocate the checkRS closure if we are going to actually use it*)
                fun checkRSMergeWS(rs:item, i:long) : () = 
                    case rs 
                       of NilItem =>  return()
                        | WithK(tv':tvar,_:any,_:item,ws:item,k:cont(any),next:item) => 
                            if Equal(tv, tv')
                            then (*tvars are equal*)
                                let res : int = ccall M_PolyEq(k, retK)
                                if I32Eq(res, 1)
                                then (*continuations are equal*)
                                    if Equal(ws, writeSet)
                                    then (*continuations, write sets, and tvars are equal, fast forward...*)
                                        do @hook-readsets(ffInfo, rs, readSet, myStamp / exh)
                                        @ff-validate(readSet, rs, myStamp / exh)
                                    else   (*write sets are not pointer equal, try a relaxed version*)
                                        @match-writesets(ws, writeSet, ffInfo, rs, readSet, myStamp / exh)
                                else apply checkRSMergeWS(next, I64Add(i, 1:long))
                            else apply checkRSMergeWS(next, I64Add(i, 1:long))
                    end 
                INC_FF(1:long)
                apply checkRSMergeWS(#LASTK(ffInfo), 1:long)
        ;

        define inline @getNumK(rs : read_set) : int = return(#NUMK(rs));

        define inline @filterRS(readSet : read_set, stamp : ![long,int,int,long] / exh : exh) : () = 
            let vp : vproc = host_vproc
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
                                let rs : any = vpload(REMEMBER_SET, vp)
                                let newRemSet : [item, int, long, any] = alloc(l, NEXTK, #3(stamp), rs)
                                do vpstore(REMEMBER_SET, vp, newRemSet)
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

        (*add a non checkpointed (local) read to the read set*)
        define @insert-local-read(tv:any, v:any, readSet : read_set, stamp : ![long,int,int,long], ws : item / exh:exh) : () =
            let newItem : item = Local(tv, v, NilItem, ws)
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
    type 'a read_set = _prim(read_set)

end



