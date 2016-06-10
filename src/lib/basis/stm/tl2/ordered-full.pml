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

structure OrderedFullAbortTL2 =
struct

    structure RS = TL2OrderedRS

    (*flat representation for read and write sets*)
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

        typedef ritem = RS.ritem;

        typedef read_set = ![int, RS.ritem, RS.ritem, RS.ritem];

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
            let myStamp : stamp_rec = FLS.@get-key(STAMP_KEY / exh)
            let readSet : read_set = FLS.@get-key(READ_SET / exh)
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
                    let newRS : read_set = RS.@insert-without-k(tv, readSet, myStamp / exh)
                    return(current)
            end
        ;

        define @put(arg:[tvar, any] / exh:exh) : unit =
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
            let readSet : read_set = FLS.@get-key(READ_SET / exh)
            let writeSet : witem = FLS.@get-key(WRITE_SET / exh)
            fun acquire(writeSet:witem, acquired:witem) : witem = 
                case writeSet 
                   of Write(tv:tvar, contents:any, tl:witem) => 
                        let owner : long = #CURRENT_LOCK(tv)
                        if U64Eq(owner, #LOCK_VAL(startStamp))
                        then apply acquire(tl, acquired)  (*we already locked this*)
                        else
                            if U64Lte(owner, #START_STAMP(startStamp) )
                            then
                                if BCAS(&CURRENT_LOCK(tv), owner, #LOCK_VAL(startStamp))
                                then 
                                    do #PREV_LOCK(tv) := owner 
                                    apply acquire(tl, Write(tv, contents, acquired))
                                else do apply release(acquired) @full-abort(/exh) (*CAS failed*)
                            else do apply release(acquired) @full-abort(/exh) (*newer than our timestamp*)
                    | NilWrite => return(acquired)
                end
            fun validate(readSet : ritem, locks : witem) : () = 
                case readSet 
                   of RS.WithoutK(tv:tvar, tl:ritem) =>
                        let owner : long = #CURRENT_LOCK(tv)
                        if U64Eq(owner, #LOCK_VAL(startStamp))
                        then apply validate(tl, locks)
                        else 
                            if U64Lte(owner, #START_STAMP(startStamp) )  (*still valid and unlocked*)
                            then apply validate(tl, locks)
                            else do apply release(locks) @full-abort(/exh)   
                    | RS.NilRead => return()
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
            if U64Eq(newStamp, #START_STAMP(startStamp) )
            then apply update(locks, I64Add(newStamp, 1:long))
            else 
                do apply validate(#LONG_PATH(readSet), locks)
                apply update(locks, I64Add(newStamp, 1:long))
        ;

        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#0(in_trans))
            then apply f(UNIT/exh)
            else 
                let stampPtr : stamp_rec = FLS.@get-key(STAMP_KEY / exh)
                cont enter() = 
                    let rs : read_set = RS.@new(enum(0):any / exh)
                    do FLS.@set-key(READ_SET, rs / exh)  (*initialize STM log*)
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
                    do FLS.@set-key(READ_SET, RS.NilRead / exh)
                    do FLS.@set-key(WRITE_SET, NilWrite / exh)
                    return(res)
                throw enter()
        ;

        define @abort(x : unit / exh : exh) : any = 
            let e : cont() = FLS.@get-key(ABORT_KEY / exh)
            throw e();
         
    )

	type 'a tvar = 'a FullAbortSTM.tvar
	val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = FullAbortSTM.new
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val abort : unit -> 'a = _prim(@abort)
end