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
        typedef tvar = ![any, long, long]; (*contents, lock, version stamp*)

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
                        let _ : long = I64FetchAndAdd(&1(tv), ~1:long)
                        apply decLoop(next)
                    | _ => 
                        let casted : [any] = ([any]) i
                        do ccall M_Print_Long("decCounts: impossible! tag is %lu\n", #0(casted)) 
                        throw exh(Fail(@"decCounts: impossible\n"))
                end
            if Equal(readSet, enum(0))
            then return()
            else apply decLoop(#LASTK(readSet))
        ;

        define inline @incCounts(readSet : read_set, sentinel : item / exh : exh) : () =
            fun incLoop(shortPath : item, i:int) : () = 
                case shortPath 
                   of NoRecOrderedReadSet.NilItem => do ccall M_Print_Long("incCounts: This should be impossible, sentinel is %p\n", sentinel) return()
                    | NoRecOrderedReadSet.WithK(tv:tvar, _:any, _:item, _:item, _:cont(any),next:item) => 
                        let _ : long = I64FetchAndAdd(&1(tv), 1:long)
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
                          revalidate : fun(item, item, int / -> ) / exh:exh) : () = 
            case checkpoint 
               of NoRecOrderedReadSet.NilItem => (*no checkpoint available*)
                    (*<FF>*)
                    let oldFFInfo : read_set = FLS.@get-key(FF_KEY / exh)
                    do @decCounts(oldFFInfo / exh)
                    do @incCounts(readSet, NoRecOrderedReadSet.NilItem / exh)
                    (*</FF>*)
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
                    let newRS : read_set = alloc(#HEAD(readSet), checkpoint, checkpoint, count)
                    do FLS.@set-key(READ_SET, newRS / exh)
                    do FLS.@set-key(WRITE_SET, ws / exh)
                    (*<FF>*)
                    let oldFFInfo : read_set = FLS.@get-key(FF_KEY / exh)
                    do @decCounts(oldFFInfo / exh)
                    do @incCounts(readSet, checkpoint / exh)
                    (*</FF>*)
                    let captureFreq : int = FLS.@get-counter2()
                    do FLS.@set-counter(captureFreq)
                    BUMP_PABORT
                    throw abortK(current)
            end
        ;

        define @validate(readSet : read_set, startStamp:![stamp, int, int, long] / exh:exh) : () = 
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
                        else @abortABCD(readSet, abortInfo, startStamp, count, validateLoopABCD / exh)
                    | NoRecOrderedReadSet.WithK(tv:tvar,x:any,next:item,ws:item,abortK:any,_:item) =>
                        if Equal(#0(tv), x)
                        then 
                            if Equal(abortK, enum(0))
                            then apply validateLoopABCD(next, abortInfo, count)            (*update checkpoint*)
                            else apply validateLoopABCD(next, rs, I32Add(count, 1))
                        else
                            if Equal(abortK, enum(0))
                            then @abortABCD(readSet, abortInfo, startStamp, count, validateLoopABCD / exh)
                            else @abortABCD(readSet, rs, startStamp, I32Add(count, 1), validateLoopABCD / exh)
                end
            let currentTime : stamp = @get-stamp(/exh)
            do #0(startStamp) := currentTime
            apply validateLoopABCD(#HEAD(readSet), NoRecOrderedReadSet.NilItem, 0)
        ;

        define @ff-finish(readSet : read_set, checkpoint : item, i:int / exh:exh) : () =
            case checkpoint 
               of NoRecOrderedReadSet.WithK(tv:tvar,x:any,_:item,ws:item,k:cont(any),next:item) => 
                    let casted : mutWithK = (mutWithK) checkpoint
                    do #NEXT(casted) := NoRecOrderedReadSet.NilItem
                    let newRS : read_set = alloc(#0(readSet), checkpoint, checkpoint, i)
                    do FLS.@set-key(READ_SET, newRS / exh)
                    do FLS.@set-key(WRITE_SET, ws / exh)
                    BUMP_KCOUNT
                    throw k(x)
                | _ => throw exh(Fail(@"Impossible: ff-finish\n"))
            end
        ;

        define @ff-validate(readSet : read_set, oldRS : item, myStamp : ![long,int,int,long] / exh:exh) : () = 
            fun ffLoop(rs:item, i:int, checkpoint : item) : () = 
                case rs
                   of NoRecOrderedReadSet.NilItem => @ff-finish(readSet, checkpoint, i / exh)
                    | NoRecOrderedReadSet.WithoutK(tv:tvar, x:any, next:item) => 
                        if Equal(#0(tv), x)
                        then apply ffLoop(next, i, checkpoint)
                        else @ff-finish(readSet, checkpoint, i / exh)
                    | NoRecOrderedReadSet.WithK(tv:tvar,x:any,next:item,ws:item,k:cont(any),_:item) => 
                        if Equal(#0(tv), x)
                        then
                            if Equal(k, enum(0))
                            then apply ffLoop(next, i, checkpoint)
                            else apply ffLoop(next, I32Add(i, 1), rs)
                        else 
                            if Equal(k, enum(0))
                            then @ff-finish(readSet, checkpoint, i / exh)
                            else
                                let newRS : read_set = alloc(#0(readSet), rs, rs, I32Add(i, 1))
                                fun getLoop() : any = 
                                    let v : any = #0(tv)
                                    let t : long = VClock.@get(/exh)
                                    if I64Eq(t, #0(myStamp))
                                    then return(v)
                                    else
                                        do @validate(newRS, myStamp / exh)
                                        apply getLoop()
                                do FLS.@set-key(READ_SET, newRS / exh)
                                do FLS.@set-key(WRITE_SET, ws / exh)
                                let casted : mutWithK = (mutWithK) rs
                                do #NEXT(casted) := NoRecOrderedReadSet.NilItem
                                let current : any = apply getLoop()
                                BUMP_KCOUNT
                                throw k(current)
                end
            apply ffLoop(oldRS, #NUMK(readSet), oldRS)
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
                            INC_FF(1:long)
                            if Equal(tv, tv')
                            then (*tvars are equal*)
                                let res : int = ccall M_PolyEq(k, retK)
                                if I32Eq(res, 1)
                                then (*continuations are equal*)
                                    if Equal(ws, writeSet) 
                                    then (*continuations, write sets, and tvars are equal, fast forward...*)
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
                apply checkRS(#LASTK(ffInfo), 1:long)
        ;

    )
    type 'a read_set = _prim(read_set)

end



