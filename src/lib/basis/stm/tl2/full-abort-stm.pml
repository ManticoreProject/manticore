(* stm.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Software Transactional Memory with partial aborts.
 *)

structure FullAbortSTM =
struct

    (*flat representation for read and write sets*)
    datatype 'a item = Read of 'a * 'a | Write of 'a * 'a * 'a | NilItem

    _primcode(

        extern void * M_Print_Int(void *, int);
        extern void * M_Print_Int2(void *, int, int);
        extern void M_Print_Long (void *, long);
        extern void M_BumpCounter(void * , int);
        extern int M_SumCounter(int);

        typedef stamp = VClock.stamp;
        typedef tvar = ![any, long, long]; (*contents, current version stamp / lock, previous version stamp / lock*)

        define @new(x:any / exh:exh) : tvar = 
            let tv : tvar = alloc(x, 0:long, 0:long)
            let tv : tvar = promote(tv)
            return(tv)
        ;

        define @full-abort(/exh:exh) noreturn = 
            let k : cont() = FLS.@get-key(ABORT_KEY / exh)
            throw k();

        define @read-tvar(tv : tvar, stamp : ![stamp, int] / exh : exh) : any = 
            let v1 : stamp = #1(tv)
            let res : any = #0(tv)
            let v2 : stamp = #1(tv)
            let v1Lock : long = I64AndB(v1, 1:long)
            if I64Eq(v1Lock, 0:long)  (*unlocked*)
            then
                if I64Eq(v1, v2)
                then 
                    if I64Lt(v1, #0(stamp))
                    then return(res)
                    else @full-abort(/exh)
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
            let myStamp : ![stamp, int] = FLS.@get-key(STAMP_KEY / exh)
            let readSet : item = FLS.@get-key(READ_SET / exh)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            fun chkLog(writeSet : item) : Option.option = (*use local copy if available*)
                case writeSet
                    of Write(tv':tvar, contents:any, tl:item) =>
                        if Equal(tv', tv)
                        then return(Option.SOME(contents))
                        else apply chkLog(tl)                      
                    | NilItem => return (Option.NONE)
                end
            let localRes : Option.option = apply chkLog(writeSet)
            case localRes
                of Option.SOME(v:any) => return(v)
                 | Option.NONE =>
                    let current : any = @read-tvar(tv, myStamp / exh)
                    let newReadSet : item = Read(tv, readSet)
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
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            let newWriteSet : item = Write(tv, v, writeSet)
            do FLS.@set-key(WRITE_SET, newWriteSet / exh)
            return(UNIT)
        ;

        define @commit(/exh:exh) : () =     
            let startStamp : ![stamp, int] = FLS.@get-key(STAMP_KEY / exh)
            fun release(locks : item) : () = 
                case locks 
                    of Write(tv:tvar, contents:any, tl:item) =>
                        do #1(tv) := #2(tv)   (*revert to previous lock*)
                        apply release(tl)
                     | NilItem => return()
                end
            let readSet : item = FLS.@get-key(READ_SET / exh)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            let rawStamp: long = #0(startStamp) (*this should be even*)
            let lockVal : long = I64Add(rawStamp, 1:long)
            fun validate(readSet : item, locks : item) : () = 
                case readSet 
                    of Read(tv:tvar, tl:item) =>
                        let owner : long = #CURRENT_LOCK(tv)
                        if I64Lt(owner, rawStamp)  (*still valid*)
                        then 
                            if I64Eq(I64AndB(owner, 1:long), 1:long)
                            then do apply release(locks) @full-abort(/exh)
                            else apply validate(tl, locks)
                        else 
                            if I64Eq(owner, lockVal) (*we locked it*)
                            then apply validate(tl, locks)
                            else do apply release(locks) @full-abort(/exh)   
                     |NilItem => return()
                end
            fun acquire(writeSet:item, acquired:item) : item = 
                case writeSet 
                   of Write(tv:tvar, contents:any, tl:item) => 
                        let owner : long = #1(tv)
                        if I64Eq(owner, lockVal)
                        then apply acquire(tl, acquired)  (*we already locked this*)
                        else
                            if I64Eq(I64AndB(owner, 1:long), 0:long)
                            then
                                if I64Lt(owner, lockVal)
                                then
                                    let casRes : long = CAS(&1(tv), owner, lockVal)
                                    if I64Eq(casRes, owner)
                                    then 
                                        do #PREV_LOCK(tv) := owner 
                                        apply acquire(tl, Write(tv, contents, acquired))
                                    else do apply release(acquired) @full-abort(/exh) (*CAS failed*)
                                else do apply release(acquired) @full-abort(/exh) (*newer than our timestamp*)
                            else do apply release(acquired) @full-abort(/exh)  (*someone else locked it*)
                    | NilItem => return(acquired)
                end
            fun update(writes:item, newStamp : stamp) : () = 
                case writes
                    of Write(tv:tvar, newContents:any, tl:item) =>
                        let newContents : any = promote(newContents)
                        do #0(tv) := newContents        (*update contents*)
                        do #1(tv) := newStamp           (*unlock and update stamp (newStamp is even)*)
                        apply update(tl, newStamp)          
                     | NilItem => return()
                end
            let locks : item = apply acquire(writeSet, NilItem)   
            let newStamp : stamp = VClock.@inc(2:long/exh)
            do apply validate(readSet, locks)
            do apply update(locks, newStamp)
            return()
        ;

        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
                let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
                if (#0(in_trans))
                then do ccall M_Print("Warning: Nested transaction!\n") apply f(UNIT/exh)
                else let stampPtr : ![stamp, int] = FLS.@get-key(STAMP_KEY / exh)
                     cont enter() = 
                         do FLS.@set-key(READ_SET, NilItem / exh)  (*initialize STM log*)
                         do FLS.@set-key(WRITE_SET, NilItem / exh)
                         let stamp : stamp = VClock.@inc(2:long / exh)
                         do #0(stampPtr) := stamp
                         do #0(in_trans) := true
                         cont abortK() = BUMP_FABORT throw enter()      
                         do FLS.@set-key(ABORT_KEY, (any) abortK / exh)
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

	type 'a tvar = _prim(tvar)
	val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = _prim(@new)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val abort : unit -> 'a = _prim(@abort)

    val _ = Ref.set(STMs.stms, ("full", (get,put,atomic,new,abort))::Ref.get STMs.stms)
end













 
