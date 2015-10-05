(* read-set.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Chronologically ordered read sets for NoRec
 *)

 

structure FFReadSetCounter = 
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

    _primcode(

        extern void M_Print_Long2(void *, void *, void *);
        extern void M_IncCounter(void *, int , long);
        extern void M_Debug(void*);
        extern void M_Print_Int_Long2(void*, int, long, long);
        typedef item = NoRecOrderedReadSet.item;
        extern int M_PolyEq(void*, void*);

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

    	define @new() : read_set = 
            let dummyTRef : ![any,long,long] = alloc(enum(0), 0:long, 0:long)
            let dummy : item = NoRecOrderedReadSet.WithoutK(dummyTRef, enum(0), NoRecOrderedReadSet.NilItem)
    		let rs : read_set = alloc(dummy, dummy, NoRecOrderedReadSet.NilItem, 0)
    		return(rs)
    	;

        define inline @decCounts(readSet : read_set / exh : exh) : () = 
            fun decLoop(i:item) : () = 
                case i 
                   of NoRecOrderedReadSet.NilItem => return()
                    | NoRecOrderedReadSet.WithK(tv:tvar, _:any, _:item, _:item, _:cont(any),next:item) => 
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
                   of NoRecOrderedReadSet.NilItem => return()
                    | NoRecOrderedReadSet.WithK(tv:tvar, _:any, _:item, _:item, _:cont(any),next:item) => 
                        let old : long = I64FetchAndAdd(&1(tv), 1:long)
                        if Equal(next, sentinel)
                        then 
                            let casted : mutWithK = (mutWithK) shortPath
                            do #NEXTK(casted) := NoRecOrderedReadSet.NilItem
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
            let vp : vproc = host_vproc
            let id : int = VProc.@vproc-id(vp)
            case checkpoint 
               of NoRecOrderedReadSet.NilItem => (*no checkpoint available*)
                    let oldFFInfo : read_set = FLS.@get-key(FF_KEY / exh)
                    do @decCounts(oldFFInfo / exh)
                    do @incCounts(readSet, NoRecOrderedReadSet.NilItem / exh)
#ifdef EVENT_LOGGING
                    do 
                        if(eager)
                        then Logging.@log-eager-full-abort() 
                        else Logging.@log-commit-full-abort() 
#endif
                    let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                    throw abortK()
                | NoRecOrderedReadSet.WithK(tv:tvar, _:any, next:item, ws:item, abortK:cont(any),_:item) => 
                    let casted : ![any, any, any, item] = (![any, any, any, item]) checkpoint
                    do #NEXT(casted) := NoRecOrderedReadSet.NilItem
                    fun getLoop() : any = 
                        let v : any = #0(tv)
                        let t : long = VClock.@get(/exh)
                        if I64Eq(t, #0(startStamp))
                        then return(v)
                        else
                            let currentTime : stamp = @get-stamp(/exh)
                            do #0(startStamp) := currentTime
                            do apply revalidate(#HEAD(readSet), NoRecOrderedReadSet.NilItem, 0)
                            apply getLoop()
                    let current : any = apply getLoop()
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

        define @validate(readSet : read_set, startStamp:![stamp, int, int, long], eager : bool / exh:exh) : () = 
            fun validateLoopABCD(rs : item, abortInfo : item, count:int) : () =
                case rs 
                   of NoRecOrderedReadSet.NilItem => (*finished validating*)
                        let currentTime : stamp = VClock.@get(/exh)
                        if I64Eq(currentTime, #0(startStamp))
                        then return() (*no one committed while validating*)
                        else  (*someone else committed, so revalidate*)
                            let currentTime : stamp = @get-stamp(/exh)
                            do #0(startStamp) := currentTime
                            apply validateLoopABCD(#HEAD(readSet), NoRecOrderedReadSet.NilItem, 0)
                    | NoRecOrderedReadSet.WithoutK(tv:tvar, x:any, next:item) =>
                        if Equal(#0(tv), x)
                        then apply validateLoopABCD(next, abortInfo, count)
                        else @abortABCD(readSet, abortInfo, startStamp, count, validateLoopABCD, eager / exh)
                    | NoRecOrderedReadSet.WithK(tv:tvar,x:any,next:item,ws:item,abortK:any,_:item) =>
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
            let currentTime : stamp = @get-stamp(/exh)
            do #0(startStamp) := currentTime
            apply validateLoopABCD(#HEAD(readSet), NoRecOrderedReadSet.NilItem, 0)
        ;

        define @ff-finish(readSet : read_set, checkpoint : item, i:int, j:int / exh:exh) : () =
            case checkpoint 
               of NoRecOrderedReadSet.WithK(tv:tvar,x:any,_:item,ws:item,k:cont(any),next:item) => 
                    do Logging.@log-ff(j)
                    let casted : mutWithK = (mutWithK) checkpoint
                    do #NEXT(casted) := NoRecOrderedReadSet.NilItem
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
                   of NoRecOrderedReadSet.NilItem => @ff-finish(readSet, checkpoint, i, j / exh)
                    | NoRecOrderedReadSet.WithoutK(tv:tvar, x:any, next:item) => 
                        if Equal(#0(tv), x)
                        then apply ffLoop(next, i, checkpoint, I32Add(j, 1))
                        else @ff-finish(readSet, checkpoint, i, j / exh)
                    | NoRecOrderedReadSet.WithK(tv:tvar,x:any,next:item,ws:item,k:cont(any),_:item) => 
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
                                do #NEXT(casted) := NoRecOrderedReadSet.NilItem
                                let current : any = apply getLoop()
                                BUMP_KCOUNT
                                do Logging.@log-ff(j)
                                throw k(current)
                end
            apply ffLoop(oldRS, #NUMK(readSet), oldRS, 0)
        ;

        (*
         * 1 -> maps are pointer equal
         * 2 -> maps are equal modulo the spine
         * 3 -> maps are key equal
         * 4-5 -> new map is a key subset of old map
         *)
        define @ws-check(oldWS : item, newWS : item) : () = 
            fun keySubset(oldWS : item, newWS : item) : () =
                case oldWS 
                    of NoRecOrderedReadSet.Write(tv:tvar, x:any, next:item) => 
                        case newWS 
                           of NoRecOrderedReadSet.NilItem => do Logging.@log-ws-match(4) return()
                            | NoRecOrderedReadSet.Write(tv':tvar,x':any,next':item) => 
                                if Equal(tv,tv')
                                then apply keySubset(next, next')
                                else apply keySubset(next, newWS)
                        end
                     | NoRecOrderedReadSet.NilItem => return() 
                end
            fun eqModuloSpine(oldWS : item, newWS : item, matchType : int) : () = 
                case oldWS 
                   of NoRecOrderedReadSet.Write(tv:tvar,x:any,next:item) => 
                        case newWS 
                           of NoRecOrderedReadSet.NilItem => do Logging.@log-ws-match(4) return()  (*key subset*)
                            | NoRecOrderedReadSet.Write(tv':tvar,x':any,next':item) => 
                                if Equal(tv, tv')
                                then 
                                    if Equal(x, x')
                                    then apply eqModuloSpine(next, next', matchType)
                                    else apply eqModuloSpine(next, next', 3:int)
                                else apply keySubset(next, newWS)  (*try and match subsequence*)
                        end
                    | NoRecOrderedReadSet.NilItem => 
                        case newWS 
                           of NoRecOrderedReadSet.NilItem => do Logging.@log-ws-match(matchType) return()
                            | _ => return()
                        end
                end
            apply eqModuloSpine(oldWS, newWS, 2:int)
        ;

        define @fast-forward(readSet : read_set, writeSet : item, tv:tvar, retK:cont(any), myStamp : ![long, int, int, long] / exh:exh) : () = 
            let ffInfo : read_set = FLS.@get-key(FF_KEY / exh)
            if Equal(ffInfo, enum(0))
            then return()
            else (*we should only allocate the checkRS closure if we are going to actually use it*)
                fun checkRS(rs:item, i:long) : () = 
                    case rs 
                       of NoRecOrderedReadSet.NilItem =>  return()
                        | NoRecOrderedReadSet.WithK(tv':tvar,_:any,_:item,ws:item,k:cont(any),next:item) => 
                            if Equal(tv, tv')
                            then (*tvars are equal*)
                                let res : int = ccall M_PolyEq(k, retK)
                                if I32Eq(res, 1)
                                then (*continuations are equal*)
                                    if Equal(ws, writeSet) 
                                    then (*continuations, write sets, and tvars are equal, fast forward...*)
                                        do Logging.@log-ws-match(1:int)
                                        do @decCounts(ffInfo / exh)  (*decrement counts for everything on short path of ffInfo*)
                                        do FLS.@set-key(FF_KEY, enum(0) / exh)  (*null out fast forward info*)
                                        (*hook the two read sets together*)
                                        let ffFirstK : mutWithK = (mutWithK) rs
                                        do #NEXTK(ffFirstK) := #LASTK(readSet) 
                                        let currentLast : item = #TAIL(readSet)
                                        let currentLast : mutWithK = (mutWithK) currentLast
                                        do #NEXT(currentLast) := rs
                                        (*add to remember set*)
                                        let vp : vproc = host_vproc
                                        let rememberSet : any = vpload(REMEMBER_SET, vp)
                                        let newRemSet : [mutWithK, int, long, any] = alloc(ffFirstK, NEXTK, #3(myStamp), rememberSet)
                                        do vpstore(REMEMBER_SET, vp, newRemSet)
                                        @ff-validate(readSet, rs, myStamp / exh)
                                    else apply checkRS(next, I64Add(i, 1:long))
                                else apply checkRS(next, I64Add(i, 1:long))
                            else apply checkRS(next, I64Add(i, 1:long))
                        | _ => throw exh(Fail("checkRS: impossible\n"))
                    end
                INC_FF(1:long)
                apply checkRS(#LASTK(ffInfo), 1:long)
        ;

    )
    type 'a read_set = _prim(read_set)

end



