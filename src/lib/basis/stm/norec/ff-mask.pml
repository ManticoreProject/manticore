(* read-set.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Chronologically ordered read sets for NoRec
 *)

 

structure FFMask = 
struct

#define READ_SET_BOUND 21

#define NEXT 3
#define NEXTK 6
#define READ_VAL 2
#define KPOINTER 5
#define HEAD 0
#define TAIL 1
#define LASTK 2
#define NUMK 3

#define START_TIMER let vp : vproc = host_vproc do ccall GenTimerStart(vp)
#define STOP_TIMER let vp : vproc = host_vproc do ccall GenTimerStop(vp)

    _primcode(

        typedef item = NoRecOrderedReadSet.item;

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
        

        define inline @unset-masks(readSet : read_set, myStamp : ![long,int,int,long]) : () =
            let rawId : long = I64NotB(#3(myStamp))
            fun unMaskLoop(i:item) : () =
                case i 
                   of NoRecOrderedReadSet.NilItem => return()
                    | NoRecOrderedReadSet.WithK(tv:tvar, _:any, _:item, _:item, _:cont(any), next:item) => 
                        let stamp : stamp = #1(tv)
                        let new : stamp = I64AndB(stamp, rawId)
                        let old : stamp = CAS(&1(tv), stamp, new)
                        if I64Eq(stamp, old)
                        then apply unMaskLoop(next)
                        else apply unMaskLoop(i)
                end
            if Equal(readSet, enum(0))
            then return()
            else apply unMaskLoop(#LASTK(readSet))
        ;

        define inline @set-masks(readSet : read_set, sentinel : item, myStamp : ![long,int,int,long] / exh:exh) : () = 
            let rawId : long = #3(myStamp)
            fun maskLoop(i:item) : () = 
                case i 
                   of NoRecOrderedReadSet.NilItem => return()
                    | NoRecOrderedReadSet.WithK(tv:tvar, _:any, _:item, _:item, _:cont(any), next:item) => 
                        let stamp : stamp = #1(tv)
                        let new : stamp = I64OrB(stamp, rawId)
                        let old : stamp = CAS(&1(tv), stamp, new)
                        if I64Eq(stamp, old)
                        then 
                            if Equal(sentinel, next)
                            then 
                                let casted : mutWithK = (mutWithK) i
                                do #NEXTK(casted) := NoRecOrderedReadSet.NilItem
                                return()
                            else apply maskLoop(next)
                        else apply maskLoop(i)
                end
            let lastK : item = #LASTK(readSet)
            if Equal(lastK, sentinel)
            then FLS.@null-key(FF_KEY)  (*no checkpoints after violation*)
            else 
                do apply maskLoop(lastK)   (*increment reference counts for checkpoints after violation*)
                FLS.@set-key(FF_KEY, readSet / exh)
        ;

        define @abortABCD(readSet : read_set, checkpoint : item, startStamp : ![stamp, int, int, long], count:int, 
                          revalidate : fun(item, item, int / -> ), eager : bool/ exh:exh) : () = 
            let vp : vproc = host_vproc
            let id : int = VProc.@vproc-id(vp)
            case checkpoint 
               of NoRecOrderedReadSet.NilItem => (*no checkpoint available*)
                    let oldFFInfo : read_set = FLS.@get-key(FF_KEY / exh)
                    do @unset-masks(oldFFInfo, startStamp)
                    do @set-masks(readSet, NoRecOrderedReadSet.NilItem, startStamp / exh)
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
                        do FenceRead()
                        let t : long = VClock.@get(/exh)
                        if I64Eq(t, #0(startStamp))
                        then return(v)
                        else
                            let currentTime : stamp = NoRecFull.@get-stamp(/exh)
                            do #0(startStamp) := currentTime
                            do apply revalidate(#HEAD(readSet), NoRecOrderedReadSet.NilItem, 0)
                            apply getLoop()
                    let current : any = apply getLoop()
                    do #READ_VAL(casted) := current (*this came out of a tref, so it must have been promoted*)
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
                    do @unset-masks(oldFFInfo, startStamp)
                    do @set-masks(readSet, checkpoint, startStamp / exh)
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
                        do FenceRead()
                        let currentTime : stamp = VClock.@get(/exh)
                        if I64Eq(currentTime, #0(startStamp))
                        then return() (*no one committed while validating*)
                        else  (*someone else committed, so revalidate*)
                            let currentTime : stamp = NoRecFull.@get-stamp(/exh)
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
            let currentTime : stamp = NoRecFull.@get-stamp(/exh)
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
                                    do FenceRead()
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
                                        do @unset-masks(ffInfo, myStamp)
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

        define @getFFNoRecCounter(tv : tvar / exh:exh) : any = 
            let in_trans : [bool] = FLS.@get-key(IN_TRANS / exh)
            do  if(#0(in_trans))
                then return()
                else 
                    do ccall M_Print("Trying to read outside a transaction!\n")
                    let e : exn = Fail(@"Reading outside transaction\n")
                    throw exh(e)
            let myStamp : ![stamp, int, int, long] = FLS.@get-key(STAMP_KEY / exh)
            let readSet : read_set = FLS.@get-key(READ_SET / exh)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            fun chkLog(writeSet : item) : Option.option = (*use local copy if available*)
                case writeSet
                   of NoRecOrderedReadSet.Write(tv':tvar, contents:any, tl:item) =>
                        if Equal(tv', tv)
                        then return(Option.SOME(contents))
                        else apply chkLog(tl)
                    | NoRecOrderedReadSet.NilItem => return (Option.NONE)
                end
            cont retK(x:any) = return(x)
            let rawId : long = #3(myStamp)
            let masked : long = I64AndB(#1(tv), rawId)
            do  if I64Eq(masked, rawId)
                then @fast-forward(readSet, writeSet, tv, retK, myStamp / exh)
                else return()
            let localRes : Option.option = apply chkLog(writeSet)
            case localRes
               of Option.SOME(v:any) => return(v)
                | Option.NONE =>
                    fun getLoop() : any = 
                        let v : any = #0(tv)
                        do FenceRead()
                        let t : long = VClock.@get(/exh)
                        if I64Eq(t, #0(myStamp))
                        then return(v)
                        else
                            do @validate(readSet, myStamp, true / exh)                           
                            apply getLoop()
                    let current : any = apply getLoop()
                    let captureCount : int = FLS.@get-counter()
                    if I32Eq(captureCount, 1)
                    then
                        let kCount : int = NoRecOrderedReadSet.@getNumK(readSet)
                        if I32Lt(kCount, READ_SET_BOUND)
                        then
                            do NoRecOrderedReadSet.@insert-with-k(tv, current, retK, writeSet, readSet, myStamp / exh)
                            let captureFreq : int = FLS.@get-counter2()
                            do FLS.@set-counter(captureFreq)
                            return(current)
                        else
                            do NoRecOrderedReadSet.@insert-with-k(tv, current, retK, writeSet, readSet, myStamp / exh)
                            do FFReadSet.@filterRS(readSet, myStamp / exh)
                            let captureFreq : int = FLS.@get-counter2()
                            let newFreq : int = I32Mul(captureFreq, 2)
                            do FLS.@set-counter(newFreq)
                            do FLS.@set-counter2(newFreq)
                            return(current)
                    else
                        do FLS.@set-counter(I32Sub(captureCount, 1))
                        do NoRecOrderedReadSet.@insert-without-k(tv, current, readSet, myStamp / exh)
                        return(current)
            end 
        ;

        define @commit(stamp : ![stamp, int, int, long] /exh:exh) : () =
            let readSet : NoRecOrderedReadSet.read_set = FLS.@get-key(READ_SET / exh)
            let writeSet : NoRecOrderedReadSet.item = FLS.@get-key(WRITE_SET / exh)
            let counter : ![long] = VClock.@get-boxed(/exh)
            fun lockClock() : () = 
                let current : stamp = #0(stamp)
                let old : long = CAS(&0(counter), current, I64Add(current, 1:long))
                if I64Eq(old, current)
                then return()
                else
                    do @validate(readSet, stamp, false / exh)
                    apply lockClock()
            do apply lockClock()
            fun writeBack(ws:NoRecOrderedReadSet.item) : () = 
                case ws 
                   of NoRecOrderedReadSet.NilItem => return()
                    | NoRecOrderedReadSet.Write(tv:tvar, x:any, next:NoRecOrderedReadSet.item) => 
                        let x : any = promote(x)
                        do #0(tv) := x
                        apply writeBack(next)
                end
            fun reverseWS(ws:NoRecOrderedReadSet.item, new:NoRecOrderedReadSet.item) : NoRecOrderedReadSet.item = 
                case ws 
                   of NoRecOrderedReadSet.NilItem => return(new)
                    | NoRecOrderedReadSet.Write(tv:tvar, x:any, next:NoRecOrderedReadSet.item) => apply reverseWS(next, NoRecOrderedReadSet.Write(tv, x, new))
                end
            let writeSet : NoRecOrderedReadSet.item = apply reverseWS(writeSet, NoRecOrderedReadSet.NilItem)
            do apply writeBack(writeSet)
            do FenceRead()
            do #0(counter) := I64Add(#0(stamp), 2:long) (*unlock clock*)
            let ffInfo : NoRecOrderedReadSet.read_set =  FLS.@get-key(FF_KEY / exh)
            do @unset-masks(ffInfo, stamp)
            return()
        ;

        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#0(in_trans))
            then apply f(UNIT/exh)
            else 
                let stampPtr : ![stamp, int, int, long] = FLS.@get-key(STAMP_KEY / exh)
                do FLS.@set-key(FF_KEY, enum(0) / exh)
                cont enter() = 
                    let freq : int = FLS.@get-counter2()
                    do FLS.@set-counter(freq) (*set counter to frequency*)
                    let rs : read_set = FFReadSetCounter.@new()
                    do FLS.@set-key(READ_SET, rs / exh)  (*initialize STM log*)
                    do FLS.@set-key(WRITE_SET, NoRecOrderedReadSet.NilItem / exh)
                    let stamp : stamp = NoRecFull.@get-stamp(/exh)
                    do #0(stampPtr) := stamp
                    do #0(in_trans) := true
                    cont abortK() = BUMP_FABORT do #0(in_trans) := false throw enter()
                    do FLS.@set-key(ABORT_KEY, abortK / exh)
                    cont transExh(e:exn) = 
                        do ccall M_Print("Warning: exception raised in transaction\n")
                        throw exh(e)
                    let res : any = apply f(UNIT/transExh)
                    do @commit(stampPtr /transExh)
                    let vp : vproc = host_vproc
                    do ccall M_PruneRemSetAll(vp, #3(stampPtr))
                    do #0(in_trans) := false
                    do FLS.@set-key(READ_SET, NoRecOrderedReadSet.NilItem / exh)
                    do FLS.@set-key(WRITE_SET, NoRecOrderedReadSet.NilItem / exh)
                    do FLS.@set-key(FF_KEY, enum(0) / exh)
                    return(res)
                throw enter()
        ;

        define @abort(x : unit / exh : exh) : any = 
            let readSet : read_set = FLS.@get-key(READ_SET / exh)
            let oldFFInfo : read_set = FLS.@get-key(FF_KEY / exh)
            let stamp : ![long,int,int,long] = FLS.@get-key(STAMP_KEY / exh)
            do @unset-masks(oldFFInfo, stamp)
            do @set-masks(readSet, NoRecOrderedReadSet.NilItem, stamp / exh)
            do FLS.@set-key(FF_KEY, readSet / exh)
            let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
            throw abortK()
        ;

    )
    type 'a read_set = _prim(read_set)

    type 'a tvar = 'a FullAbortSTM.tvar
    val get : 'a tvar -> 'a = _prim(@getFFNoRecCounter)
    val new : 'a -> 'a tvar = NoRecFFCounter.new
    val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val put : 'a tvar * 'a -> unit = NoRecFF.put
    val abort : unit -> 'a = _prim(@abort)
 
end



