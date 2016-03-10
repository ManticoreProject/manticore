(* stm.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Software Transactional Memory with partial aborts.
 * We use the most significant bit for the lock.
 * This allows us to check if a timestamp is either locked
 * *or* too new with one instruction.  It is important that ALL
 * tests use unsigned comparison operators
 *)

structure FullAbortSTM =
struct

    (*flat representation for read and write sets*)
    datatype 'a ritem = Read of 'a * 'a | NilRead

    datatype 'a witem = Write of 'a * 'a * 'a | NilWrite

    _primcode(

        extern void M_Print_Int(void *, int);
        extern void M_Print_Int2(void *, int, int);
        extern void M_Print_Long2(void*, long, long);
        extern void M_Print_Long (void *, long);
        extern void M_BumpCounter(void * , int);
        extern int M_SumCounter(int);

        typedef stamp_rec = ![long,long,long,long]; (*current timestamp, lock_val, old time stamp, thread ID*)
        typedef stamp = VClock.stamp;
        typedef tvar = ![any, long, long, long]; (*contents, current version stamp / lock, previous version stamp / lock, ref count (not used here)*)

        define @new(x:any / exh:exh) : tvar = 
            let tv : tvar = alloc(x, 0:long, 0:long, 0:long)
            let tv : tvar = promote(tv)
            return(tv)
        ;

        define inline @full-abort(/exh:exh) noreturn = 
            let k : cont() = FLS.@get-key(ABORT_KEY / exh)
            throw k();

        define @read-tvar(tv : tvar, stamp : stamp_rec / exh : exh) : any = 
            let v1 : stamp = #CURRENT_LOCK(tv)
            let res : any = #TVAR_CONTENTS(tv)
            let v2 : stamp = #CURRENT_LOCK(tv)
            if U64Lte(v1, #START_STAMP(stamp))  (*unlocked and still valid*)
            then
                if U64Eq(v1, v2)
                then return(res)
                else @full-abort(/exh)
            else @full-abort(/exh)
        ;

        define @get(tv : tvar / exh:exh) : any = 
            let in_trans : [bool] = FLS.@get-key(IN_TRANS / exh)
            do if(#0(in_trans))
               then return()
               else do ccall M_Print("Trying to read outside a transaction!\n")
                    let e : exn = Fail(@"Reading outside transaction\n")
                    throw exh(e)
            let myStamp : stamp_rec = FLS.@get-key(STAMP_KEY / exh)
            let readSet : ritem = FLS.@get-key(READ_SET / exh)
            let writeSet : witem = FLS.@get-key(WRITE_SET / exh)
            fun chkLog(writeSet : witem) : Option.option = (*use local copy if available*)
                case writeSet
                    of Write(tv':tvar, contents:any, tl:witem) =>
                        if Equal(tv', tv)
                        then return(Option.SOME(contents))
                        else apply chkLog(tl)                      
                    | NilWrite => return (Option.NONE)
                end
            let localRes : Option.option = apply chkLog(writeSet)
            case localRes
                of Option.SOME(v:any) => return(v)
                 | Option.NONE =>
                    let current : any = @read-tvar(tv, myStamp / exh)
                    let newReadSet : ritem = Read(tv, readSet)
                    do FLS.@set-key(READ_SET, newReadSet / exh)
                    return(current)
            end
        ;

        define @put(arg:[tvar, any] / exh:exh) : unit =
            let in_trans : [bool] = FLS.@get-key(IN_TRANS / exh)
            do if(#0(in_trans))
               then return()
               else do ccall M_Print("Trying to write outside a transaction!\n")
                    let e : exn = Fail(@"Writing outside transaction\n")
                    throw exh(e)
            let tv : tvar = #0(arg)
            let v : any = #1(arg)
            let writeSet : witem = FLS.@get-key(WRITE_SET / exh)
            let newWriteSet : witem = Write(tv, v, writeSet)
            do FLS.@set-key(WRITE_SET, newWriteSet / exh)
            return(UNIT)
        ;

        define @commit(/exh:exh) : () =     
            let startStamp : stamp_rec = FLS.@get-key(STAMP_KEY / exh)
            fun release(locks : witem) : () = 
                case locks 
                    of Write(tv:tvar, contents:any, tl:witem) =>
                        do #CURRENT_LOCK(tv) := #PREV_LOCK(tv)   (*revert to previous lock*)
                        apply release(tl)
                     | NilWrite => return()
                end
            let readSet : ritem = FLS.@get-key(READ_SET / exh)
            let writeSet : witem = FLS.@get-key(WRITE_SET / exh)
            let rawStamp: long = #START_STAMP(startStamp) 
            let lockVal : long = #LOCK_VAL(startStamp)
            fun validate(readSet : ritem, locks : witem) : () = 
                case readSet 
                   of Read(tv:tvar, tl:ritem) =>
                        let owner : long = #CURRENT_LOCK(tv)
                        if U64Eq(owner, lockVal)
                        then apply validate(tl, locks)
                        else 
                            if U64Lte(owner, rawStamp)  (*still valid and unlocked*)
                            then apply validate(tl, locks)
                            else do apply release(locks) @full-abort(/exh)   
                    | NilRead => return()
                end
            fun acquire(writeSet:witem, acquired:witem) : witem = 
                case writeSet 
                   of Write(tv:tvar, contents:any, tl:witem) => 
                        let owner : long = #CURRENT_LOCK(tv)
                        if U64Eq(owner, lockVal)
                        then apply acquire(tl, acquired)  (*we already locked this*)
                        else
                            if U64Lte(owner, rawStamp)
                            then
                                if BCAS(&CURRENT_LOCK(tv), owner, lockVal)
                                then 
                                    do #PREV_LOCK(tv) := owner 
                                    apply acquire(tl, Write(tv, contents, acquired))
                                else do apply release(acquired) @full-abort(/exh) (*CAS failed*)
                            else do apply release(acquired) @full-abort(/exh) (*newer than our timestamp*)
                    | NilWrite => return(acquired)
                end
            fun update(writes:witem, newStamp : stamp) : () = 
                case writes
                    of Write(tv:tvar, newContents:any, tl:witem) =>
                        let newContents : any = promote(newContents)
                        do #TVAR_CONTENTS(tv) := newContents        (*update contents*)
                        do #CURRENT_LOCK(tv) := newStamp           (*unlock and update stamp (newStamp is even)*)
                        apply update(tl, newStamp)
                     | NilWrite => return()
                end
            let locks : witem = apply acquire(writeSet, NilWrite)   
            let newStamp : stamp = VClock.@inc(1:long/exh)
            if U64Eq(newStamp, rawStamp)
            then apply update(locks, I64Add(newStamp, 1:long))
            else 
                do apply validate(readSet, locks)
                apply update(locks, I64Add(newStamp, 1:long))
        ;

        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#0(in_trans))
            then apply f(UNIT/exh)
            else 
                let stampPtr : stamp_rec = FLS.@get-key(STAMP_KEY / exh)
                cont enter() = 
                    do FLS.@set-key(READ_SET, NilRead/ exh)  (*initialize STM log*)
                    do FLS.@set-key(WRITE_SET, NilWrite / exh)
                    let stamp : stamp = VClock.@get(/ exh)
                    do #START_STAMP(stampPtr) := stamp
                    do #LOCK_VAL(stampPtr) := SET_MSB(#THREAD_ID(stampPtr))
                    do #0(in_trans) := true
                    cont abortK() = BUMP_FABORT throw enter()      
                    do FLS.@set-key(ABORT_KEY, (any) abortK / exh)
                    let res : any = apply f(UNIT/exh)
                    do @commit(/exh)
                    do #0(in_trans) := false
                    do FLS.@set-key(READ_SET, NilRead / exh)
                    do FLS.@set-key(WRITE_SET, NilWrite / exh)
                    return(res)
                throw enter()
        ;

        define @abort(x : unit / exh : exh) : any = 
            let e : cont() = FLS.@get-key(ABORT_KEY / exh)
            throw e();
         
    )

	type 'a tvar = _prim(tvar)
	val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = _prim(@new)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val abort : unit -> 'a = _prim(@abort)
end