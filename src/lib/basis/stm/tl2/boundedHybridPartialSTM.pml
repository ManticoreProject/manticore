(* stm.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Software Transactional Memory with partial aborts and a bounded number of continuations
 * held in the log.
 *)

structure BoundedHybridPartialSTM = 
struct 

#define READ_SET_BOUND 21

    datatype 'a item = Write of 'a * 'a * 'a | NilItem | WithK of 'a * 'a * 'a * 'a * 'a
                     | WithoutK of 'a * 'a | Abort of unit

    _primcode(
        define @mk-tag(x:unit / exh:exh) : any = 
            let x : item = WithoutK(enum(0):any, enum(0):any)
            let x : [any] = ([any]) x
            return(#0(x));
    )

    val mkTag : unit -> 'a = _prim(@mk-tag)
    val tag = mkTag()
    fun getTag() = tag

    _primcode(
    
        typedef stamp = VClock.stamp;
        typedef tvar = FullAbortSTM.tvar;
	
        typedef with_k =  ![enum(5),               (*0: tag*)
                            tvar,                  (*1: tvar operated on*)
                            item ,                 (*2: next on long path*)
                            any (*cont(any)*),     (*3: continuation*)
                            item,                  (*4: write set*)
                            item];                 (*5: next read item with a continuation*)

        typedef read_set = [int, item, item];

        define @new(x:any / exh:exh) : tvar = 
            let tv : tvar = alloc(x, 0:long, 0:long)
            let tv : tvar = promote(tv)
            return(tv)
        ;

        define @get-tag = getTag;

        (*
         * If this returns, then the entire read set is valid, and the time stamp
         * will have been updated to what the clock was at, prior to validation
         *)
        define @eager-validate(readSet : item, stamp : ![long, int] / exh:exh) : () = 
            fun eagerValidate(rs : item, chkpnt : item, newStamp : long, kCount : int) : () = 
                case rs
                   of WithK(tv:tvar, next : item, k:cont(any), ws:item, sp:item) =>
                        let owner : long = #CURRENT_LOCK(tv)
                        if I64Lt(owner, #0(stamp))  (*still valid*)
                        then 
                            if I64Eq(I64AndB(owner, 1:long), 1:long)
                            then apply eagerValidate(next, rs, newStamp, 1)
                            else 
                                case chkpnt 
                                   of Abort(x:unit) => apply eagerValidate(next, rs, newStamp, 1) (*we need a checkpoint*)
                                    | _ => apply eagerValidate(next, chkpnt, newStamp, I32Add(kCount, 1))
                                end
                        else apply eagerValidate(next, rs, newStamp, 1) 
                    | WithoutK(tv:tvar, next:item) =>
                        let owner : long = #CURRENT_LOCK(tv)
                        if I64Lt(owner, #0(stamp))
                        then
                            if I64Eq(I64AndB(owner, 1:long), 1:long)
                            then apply eagerValidate(next, Abort(UNIT), newStamp, 0)
                            else apply eagerValidate(next, chkpnt, newStamp, kCount)
                        else apply eagerValidate(next, Abort(UNIT), newStamp, 0)
                    | NilItem => 
                        case chkpnt 
                           of NilItem => do #0(stamp) := newStamp return()  (*everything is still up to date*)
                            | Abort(x:unit) => (*no checkpoint found*)
                                let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                                throw abortK()
                            | WithK(tv:tvar, next:item, abortK:cont(any), ws:item, sp:item) => 
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
                                            let newRS : read_set = alloc(kCount, chkpnt, chkpnt)
                                            do #0(stamp) := newStamp
                                            do FLS.@set-key(READ_SET, newRS / exh)
                                            do FLS.@set-key(WRITE_SET, ws / exh)
                                            BUMP_PABORT
                                            throw abortK(res)
                                        else 
                                            do #0(stamp) := newStamp
                                            let newStamp : long = VClock.@inc(2:long / exh)
                                            apply eagerValidate(chkpnt, chkpnt, newStamp, 0)
                                    else 
                                        do #0(stamp) := newStamp
                                        let newStamp : long = VClock.@inc(2:long / exh)
                                        apply eagerValidate(chkpnt, chkpnt, newStamp, 0)
                                else 
                                    do #0(stamp) := newStamp
                                    let newStamp : long = VClock.@inc(2:long / exh)
                                    apply eagerValidate(chkpnt, chkpnt, newStamp, 0)
                        end
                end
            let newStamp : long = VClock.@inc(2:long / exh)
            apply eagerValidate(readSet, NilItem, newStamp, 0)
        ;

        (*only allocates the retry loop closure if the first attempt fails*)
        define inline @read-tvar2(tv : tvar, stamp : ![stamp, int], readSet : item / exh : exh) : any = 
            fun lp() : any = 
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
                        else do @eager-validate(readSet, stamp / exh) apply lp()
                    else do @eager-validate(readSet, stamp / exh) apply lp()
                else do @eager-validate(readSet, stamp / exh) apply lp()
            apply lp()
        ;

        define inline @read-tvar(tv : tvar, stamp : ![stamp, int], readSet : item / exh : exh) : any = 
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

        define @get(tv:tvar / exh:exh) : any = 
            let in_trans : [bool] = FLS.@get-key(IN_TRANS / exh)
            do if(#0(in_trans))
               then return()
               else do ccall M_Print("Trying to read outside a transaction!\n")
                    let e : exn = Fail(@"Reading outside transaction\n")
                    throw exh(e)
            let myStamp : ![stamp, int] = FLS.@get-key(STAMP_KEY / exh)
            let readSet : read_set = FLS.@get-key(READ_SET / exh)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            fun chkLog(writeSet : item) : Option.option = (*use local copy if available*)
                 case writeSet
                     of Write(tv':tvar, contents:any, tl:item) =>
                         if Equal(tv', tv)
                         then return(Option.SOME(contents))
                         else apply chkLog(tl)
                     | NilItem => return (Option.NONE)
                 end
            cont retK(x:any) = return(x)
            let localRes : Option.option = apply chkLog(writeSet)
            case localRes
               of Option.SOME(v:any) => return(v)
                | Option.NONE => 
                    let current : any = @read-tvar(tv, myStamp, #LONG_PATH(readSet) / exh)
                    let captureCount : int = FLS.@get-counter()
                    if I32Eq(captureCount, 1)
                    then (*capture continuation*)
                        let newSL : item = WithK(tv, #LONG_PATH(readSet), retK, writeSet, #SHORT_PATH(readSet))
                        let n : int = I32Add(#KCOUNT(readSet), 1)
                        let newRS : ![int, item, item] = alloc(n, newSL, newSL)
                        do FLS.@set-key(READ_SET, newRS / exh)
                        if I32Lte(n, READ_SET_BOUND)
                        then (*still room for the one we just added*)
                            let freq : int = FLS.@get-counter2()
                            do FLS.@set-counter(freq)
                            return(current)
                        else (*we just went over the limit, filter...*)
                            fun dropKs(l:item, n:int) : int =   (*drop every other continuation*)
                                case l
                                   of NilItem => return(n)
                                    | WithK(_:tvar,_:item,_:cont(any),_:item,next:item) =>
                                        case next
                                           of NilItem => return(n)
                                            | WithK(_:tvar,_:item,_:cont(any),_:item,nextNext:item) =>
                                            (* NOTE: if compiled with -debug, this will generate warnings
                                             * that we are updating a bogus local pointer, however, given the
                                             * nature of the data structure, we do preserve the heap invariants*)
                                            let withoutKTag : enum(5) = @get-tag(UNIT/exh)
                                            let l : with_k = (with_k) l
                                            let next : with_k = (with_k) next
                                            do #WITHK_K(next) := enum(0):any
                                            do #WITHK_TAG(next) := withoutKTag
                                            do #WITHK_NEXTK(l) := nextNext
                                            apply dropKs(nextNext, I32Sub(n, 1))
                                        end
                                end
                            let kCount : int = apply dropKs(newSL, n)
                            do #KCOUNT(newRS) := kCount
                            let freq : int = FLS.@get-counter2()
                            let newFreq : int = I32Mul(freq, 2)
                            do FLS.@set-counter(newFreq)
                            do FLS.@set-counter2(newFreq)
                            return(current)
                    else
                        let newSL : item = WithoutK(tv, #LONG_PATH(readSet))
                        let newRS : [int, item, item] = alloc(#KCOUNT(readSet), newSL, #SHORT_PATH(readSet))
                        do FLS.@set-counter(I32Sub(captureCount, 1))
                        do FLS.@set-key(READ_SET, newRS / exh)
                        return(current)
            end
        ;

        define @put(arg:[tvar, any] / exh:exh) : unit =
            let in_trans : [bool] = FLS.@get-key(IN_TRANS / exh)
            do 
                if(#0(in_trans))
                then return()
                else 
                    do ccall M_Print("Trying to write outside a transaction!\n")
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
                        do #CURRENT_LOCK(tv) := #PREV_LOCK(tv)         (*unlock*)
                        apply release(tl)
                     | NilItem => return()
                end
            let readSet : [int, item, item] = FLS.@get-key(READ_SET / exh)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            let rawStamp: long = #0(startStamp)
            let lockVal : long = I64Add(rawStamp, 1:long)
            fun validate(rs:item, locks:item, newStamp : stamp, chkpnt : item, i:int) : () = 
                case rs
                   of NilItem => 
                        case chkpnt
                           of NilItem => return() (*no violations detected*)
                            | WithK(tv:tvar,_:item,abortK:cont(any),ws:item,_:item) =>
                                do apply release(locks)
                                do #0(startStamp) := newStamp
                                let current : any = @read-tvar(tv, startStamp, chkpnt / exh)
                                let newRS : [int,item,item] = alloc(i, chkpnt, chkpnt)
                                do FLS.@set-key(READ_SET, newRS / exh)
                                do FLS.@set-key(WRITE_SET, ws / exh)
                                let captureFreq : int = FLS.@get-counter2() 
                                do FLS.@set-counter(captureFreq)
                                BUMP_PABORT
                                throw abortK(current) 
                            | Abort(x:unit) =>
                                do apply release(locks)
                                let abortK :cont() = FLS.@get-key(ABORT_KEY / exh)
                                throw abortK()  (*no checkpoint found*)
                        end                          
                    | WithK(tv:tvar,next:item,k:any,ws:item,nextK:item) => 
                        let owner : long = #CURRENT_LOCK(tv)
                        if I64Lt(owner, rawStamp)
                        then (*older than my timestamp*)
                            if I64Eq(I64AndB(owner, 1:long), 1:long)
                            then apply validate(next, locks, newStamp, rs, 1)
                            else (*not locked, and older than my timestamp*)
                                case chkpnt 
                                   of Abort(x:unit) => apply validate(next, locks, newStamp, rs, 1)
                                    | _ => apply validate(next, locks, newStamp, chkpnt, I32Add(i, 1))
                                end
                        else
                            if I64Eq(owner, lockVal)
                            then 
                                case chkpnt 
                                   of Abort(x:unit) => apply validate(next, locks, newStamp, rs, 1)
                                    | _ => apply validate(next, locks, newStamp, chkpnt, I32Add(i, 1))
                                end
                            else apply validate(next, locks, newStamp, rs, 1)
                    | WithoutK(tv:tvar, next:item) =>
                        let owner : long = #CURRENT_LOCK(tv)
                        if I64Lt(owner, rawStamp)
                        then 
                            if I64Eq(I64AndB(owner, 1:long), 1:long)
                            then apply validate(next, locks, newStamp, Abort(UNIT), 0)
                            else apply validate(next, locks, newStamp, chkpnt, i)
                        else 
                            if I64Eq(owner, lockVal)
                            then apply validate(next, locks, newStamp, chkpnt, i)
                            else apply validate(next, locks, newStamp, Abort(UNIT), 0)
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
                                        do @eager-validate(#LONG_PATH(readSet), startStamp / exh)
                                        apply acquire(writeSet, NilItem)
                                else 
                                    do apply release(acquired) 
                                    do @eager-validate(#LONG_PATH(readSet), startStamp / exh)
                                    apply acquire(writeSet, NilItem)
                            else 
                                do apply release(acquired) 
                                do @eager-validate(#LONG_PATH(readSet), startStamp / exh)
                                apply acquire(writeSet, NilItem)
                    | NilItem => return(acquired)
                end
            fun update(writes:item, newStamp : stamp) : () = 
                case writes
                    of Write(tv:tvar, newContents:any, tl:item) =>
                        let newContents : any = promote(newContents)
                        do #TVAR_CONTENTS(tv) := newContents        (*update contents*)
                        do #CURRENT_LOCK(tv) := newStamp            (*unlock and update stamp (newStamp is even)*)
                        apply update(tl, newStamp)          
                     | NilItem => return()
                end
            let locks : item = apply acquire(writeSet, NilItem)
            let newStamp : stamp = VClock.@inc(2:long / exh)        
            do apply validate(#LONG_PATH(readSet),locks,newStamp,NilItem,0)  
            do apply update(locks, newStamp)
            return()
        ;
        
        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#0(in_trans))
            then apply f(UNIT/exh)
            else cont enter() = 
                     do FLS.@set-key(READ_SET, alloc(0, NilItem, NilItem) / exh)  (*initialize STM log*)
                     do FLS.@set-key(WRITE_SET, NilItem / exh)
                     let newStamp : stamp = VClock.@inc(2:long / exh)
                     let stamp : ![stamp, int] = FLS.@get-key(STAMP_KEY / exh)
                     do #1(stamp) := 0
                     do #0(stamp) := newStamp
                     do #0(in_trans) := true
                     cont abortK() = BUMP_FABORT throw enter()
                     do FLS.@set-key(ABORT_KEY, abortK / exh)
                     cont transExh(e:exn) = 
                        do ccall M_Print("Warning: exception raised in transaction\n")
                        do @commit(/exh) 
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

    type 'a tvar = 'a PartialSTM.tvar
    val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = _prim(@new)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val abort : unit -> 'a = _prim(@abort)
   
    val _ = Ref.set(STMs.stms, ("bounded", (get,put,atomic,new,abort))::Ref.get STMs.stms)

end












 
