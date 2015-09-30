(* stm.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Software Transactional Memory (TL2) with partial aborts.
 *)
 
structure PartialSTM = 
struct

    (*flat representation for read and write sets*)
    datatype 'a item = Read of 'a * 'a * 'a * 'a | Write of 'a * 'a * 'a | NilItem

    _primcode(
        extern void M_ZeroCounters();

        define @zero-counters(x:unit / e : exh) : unit = 
            do ccall M_ZeroCounters()
            return(UNIT);
        
        typedef stamp = VClock.stamp;
        typedef tvar = FullAbortSTM.tvar;

        define @new(x:any / exh:exh) : tvar = 
            let tv : tvar = alloc(x, 0:long, 0:long)
            let tv : tvar = promote(tv)
            return(tv)
        ;

        (*
         * If this returns, then the entire read set is valid, and the time stamp
         * will have been updated to what the clock was at prior to validation
         *)
        define @eager-validate(readSet : item, stamp : ![long] / exh:exh) : () = 
            fun validate(rs : item, chkpnt : item, newStamp : long) : () = 
                case rs
                   of Read(tv:tvar, k:cont(any), ws:item, tl:item) =>
                        let owner : long = #CURRENT_LOCK(tv)
                        if I64Lt(owner, #0(stamp))  (*still valid*)
                        then 
                            if I64Eq(I64AndB(owner, 1:long), 1:long)
                            then apply validate(tl, rs, newStamp)
                            else apply validate(tl, chkpnt, newStamp)
                        else apply validate(tl, rs, newStamp) 
                    | NilItem => 
                        case chkpnt 
                           of NilItem => do #0(stamp) := newStamp return()
                            | Read(tv:tvar,abortK:cont(any),ws:item,tl:item) => 
                                let v1 : long = #CURRENT_LOCK(tv)
                                let res : any = #TVAR_CONTENTS(tv)
                                let v2 : long = #CURRENT_LOCK(tv)
                                let v1Lock : long = I64AndB(v1, 1:long)
                                if I64Eq(v1Lock, 0:long)  (*unlocked*)
                                then
                                    if I64Eq(v1, v2)
                                    then 
                                        if I64Lt(v1, newStamp)
                                        then 
                                            do #0(stamp) := newStamp
                                            do FLS.@set-key(READ_SET, chkpnt / exh)
                                            do FLS.@set-key(WRITE_SET, ws / exh)
                                            BUMP_PABORT
                                            throw abortK(res)
                                        else 
                                            do #0(stamp) := newStamp
                                            let newStamp : long = VClock.@inc(2:long / exh)
                                            apply validate(chkpnt, chkpnt, newStamp)
                                    else 
                                        do #0(stamp) := newStamp
                                        let newStamp : long = VClock.@inc(2:long / exh)
                                        apply validate(chkpnt, chkpnt, newStamp)
                                else 
                                    do #0(stamp) := newStamp
                                    let newStamp : long = VClock.@inc(2:long / exh)
                                    apply validate(chkpnt, chkpnt, newStamp)
                        end
                end
            let newStamp : long = VClock.@inc(2:long / exh)
            apply validate(readSet, NilItem, newStamp)
        ;

        (*only allocates the retry loop closure if the first attempt fails*)
        define inline @read-tvar2(tv : tvar, stamp : ![stamp], readSet : item / exh : exh) : any = 
            fun lp(i:int) : any = 
                let v1 : stamp = #CURRENT_LOCK(tv)
                let res : any = #TVAR_CONTENTS(tv)
                let v2 : stamp = #CURRENT_LOCK(tv)
                let v1Lock : long = I64AndB(v1, 1:long)
                if I64Eq(v1Lock, 0:long)  (*unlocked*)
                then
                    if I64Eq(v1, v2)
                    then 
                        if I64Lt(v1, #0(stamp))
                        then return(res)
                        else do @eager-validate(readSet, stamp / exh) apply lp(I32Add(i, 1))
                    else do @eager-validate(readSet, stamp / exh) apply lp(I32Add(i, 1))
                else do @eager-validate(readSet, stamp / exh) apply lp(I32Add(i, 1))
            apply lp(0)
        ;

        define inline @read-tvar(tv : tvar, stamp : ![stamp], readSet : item / exh : exh) : any = 
            let v1 : stamp = #CURRENT_LOCK(tv)
            let res : any = #TVAR_CONTENTS(tv)
            let v2 : stamp = #CURRENT_LOCK(tv)
            let v1Lock : long = I64AndB(v1, 1:long)
            if I64Eq(v1Lock, 0:long)  (*unlocked*)
            then
                if I64Eq(v1, v2)
                then 
                    if I64Lt(v1, #0(stamp))
                    then return(res)
                    else 
                        do @eager-validate(readSet, stamp / exh)
                        @read-tvar2(tv, stamp, readSet / exh)
                else 
                    do @eager-validate(readSet, stamp / exh)
                    @read-tvar2(tv, stamp, readSet / exh)
            else 
                do @eager-validate(readSet, stamp / exh)
                @read-tvar2(tv, stamp, readSet / exh)
        ;

        define @getPartialAbort(tv:tvar / exh:exh) : any = 
            let in_trans : [bool] = FLS.@get-key(IN_TRANS / exh)
            do if(#0(in_trans))
               then return()
               else do ccall M_Print("Trying to read outside a transaction!\n")
                    let e : exn = Fail(@"Reading outside transaction\n")
                    throw exh(e)
            let myStamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
            let readSet : item = FLS.@get-key(READ_SET / exh)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            cont retK(x:any) = return(x)
            fun chkLog(writeSet : item) : Option.option = (*use local copy if available*)
                 case writeSet
                     of Write(tv':tvar, contents : any, tl:item) =>
                         if Equal(tv', tv)
                         then return(Option.SOME(contents))
                         else apply chkLog(tl)                   
                     | NilItem => return (Option.NONE)
                 end
            let localRes : Option.option = apply chkLog(writeSet)
            case localRes
                of Option.SOME(v:any) => return(v)
                 | Option.NONE =>
                    let current : any = @read-tvar(tv, myStamp, readSet / exh)
                    let newRS : item = Read(tv, retK, writeSet, readSet)
                    do FLS.@set-key(READ_SET, newRS / exh)
                    return(current)
            end
        ;

        define @put(arg:[tvar, any] / exh:exh) : unit =
            let tv : tvar = #0(arg)
            let v : any = #1(arg)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            let newWriteSet : item = Write(tv, v, writeSet)
            do FLS.@set-key(WRITE_SET, newWriteSet / exh)
            return(UNIT)
        ;

        define @commit(/exh:exh) : () = 
            let startStamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
            fun release(locks : item) : () = 
                case locks 
                    of Write(tv:tvar, contents:any, tl:item) =>
                        do #CURRENT_LOCK(tv) := #PREV_LOCK(tv)   (*revert to previous lock*)
                        apply release(tl)
                     | NilItem => return()
                end
            let readSet : item = FLS.@get-key(READ_SET / exh)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            let rawStamp: long = #0(startStamp)
            let lockVal : long = I64Add(rawStamp, 1:long)
            fun validate(rs:item, locks:item, chkpnt : item, newStamp : long) : () =
                case rs
                   of Read(tv:tvar, k:cont(any), ws:item, tl:item) =>
                        let owner : long = #CURRENT_LOCK(tv)
                        if I64Lt(owner, rawStamp)
                        then apply validate(tl, locks, chkpnt, newStamp)
                        else
                            if I64Eq(owner, lockVal)
                            then apply validate(tl, locks, chkpnt, newStamp)
                            else apply validate(tl, locks, rs, newStamp)
                    | NilItem => 
                        case chkpnt 
                            of Read(tv:tvar,abortK:cont(any),ws:item,tl:item) => 
                                do apply release(locks)
                                let current : any = @read-tvar(tv, startStamp, readSet / exh)
                                do FLS.@set-key(READ_SET, chkpnt / exh)
                                do FLS.@set-key(WRITE_SET, ws / exh)
                                do #0(startStamp) := newStamp
                                BUMP_PABORT
                                throw abortK(current)
                             | NilItem => return()   
                        end
                end
            fun acquire(ws:item, acquired:item) : item = 
                case ws
                   of Write(tv:tvar, contents:any, tl:item) => 
                        let owner : long = #CURRENT_LOCK(tv)
                        if I64Eq(owner, lockVal)
                        then apply acquire(tl, acquired)  (*we already locked this*)
                        else
                            if I64Eq(I64AndB(owner, 1:long), 0:long)
                            then
                                if I64Lt(owner, lockVal)
                                then
                                    let casRes : long = CAS(&CURRENT_LOCK(tv), owner, lockVal)
                                    if I64Eq(casRes, owner)
                                    then 
                                        do #PREV_LOCK(tv) := owner 
                                        apply acquire(tl, Write(tv, contents, acquired))
                                    else 
                                        do apply release(acquired) 
                                        do @eager-validate(readSet, startStamp / exh)
                                        apply acquire(writeSet, NilItem)
                                else 
                                    do apply release(acquired) 
                                    do @eager-validate(readSet, startStamp / exh)
                                    apply acquire(writeSet, NilItem)
                            else 
                                do apply release(acquired) 
                                do @eager-validate(readSet, startStamp / exh)
                                apply acquire(writeSet, NilItem)
                    | NilItem => return(acquired)
                end
            fun update(writes:item, newStamp : stamp) : () = 
                case writes
                    of Write(tv:tvar, newContents:any, tl:item) =>
                        let newContents : any = promote(newContents)
                        do #TVAR_CONTENTS(tv) := newContents        (*update contents*)
                        do #CURRENT_LOCK(tv) := newStamp           (*unlock and update stamp (newStamp is even)*)
                        apply update(tl, newStamp)          
                     | NilItem => return()
                end
            let locks : item = apply acquire(writeSet, NilItem)
            let newStamp : stamp = VClock.@inc(2:long / exh)
            do apply validate(readSet, locks, NilItem, newStamp)
            do apply update(locks, newStamp)
            return()
        ;

        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#0(in_trans))
            then apply f(UNIT/exh)
            else cont enter() = 
	                 do FLS.@set-key(READ_SET, NilItem / exh)  (*initialize STM log*)
                     do FLS.@set-key(WRITE_SET, NilItem / exh)
                     let newStamp : stamp = VClock.@inc(2:long / exh)
                     let stamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
                     do #0(stamp) := newStamp
                     do #0(in_trans) := true
                     cont abortK() = BUMP_FABORT throw enter()
                     do FLS.@set-key(ABORT_KEY, abortK / exh)
                     cont transExh(e:exn) = 
                        do @commit(/transExh)  (*exception may have been raised because of inconsistent state*)
                        throw exh(e)
                     let res : any = apply f(UNIT/transExh)
                     do @commit(/transExh)
                     do #0(in_trans) := false
                     do FLS.@set-key(READ_SET, NilItem / exh)
                     do FLS.@set-key(WRITE_SET, NilItem / exh)
                     return(res)  
                 throw enter()         
        ;

      define @abort(x : unit / exh : exh) : any = 
         let e : cont() = FLS.@get-key(ABORT_KEY / exh)
         throw e();
         
    )

	type 'a tvar = 'a FullAbortSTM.tvar
	val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@getPartialAbort)
    val new : 'a -> 'a tvar = _prim(@new)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val abort : unit -> 'a = _prim(@abort)

    val zeroCounters : unit -> unit = _prim(@zero-counters)
    val _ = zeroCounters()

    val _ = Ref.set(STMs.stms, ("partial", (get,put,atomic,new,abort))::Ref.get STMs.stms)

end










 
