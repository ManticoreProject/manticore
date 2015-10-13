(* ff-tl2.pml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Software Transactional Memory with partial aborts, a bounded number of continuations
 * held in the log, an ordered read set, and fast forwarding.
 * 
 * note that this is using the reference counting to limit fast forward checks
 *)

structure FFTL2 = 
struct 

structure RS = TL2OrderedRS

#define READ_SET_BOUND 21

    _primcode(
    
        extern void M_Print_Long2(void*, long, long);

        typedef stamp = VClock.stamp;
        typedef tvar = FullAbortSTM.tvar;
	
        typedef with_k = OrderedTL2.with_k;

        typedef read_set = ![int, RS.ritem, RS.ritem, RS.ritem];

        typedef ff_read_set = ![long, int, RS.ritem];  (*time stamp, first checkpoint, last checkpoint*)

        define @get-tag = BoundedHybridPartialSTM.getTag;

        (*
         * This HLop is responsible for decrementing the reference counts for evertying
         * on the hsort path of the FF read set, if one exists.  This should be called
         * after committing a transaction and when aborting a transction.
         *)
        define inline @decCounts(readSet : ff_read_set / exh : exh) : () = 
            fun decLoop(i:RS.ritem) : () = 
                case i 
                   of RS.NilRead => return()
                    | RS.WithK(tv:tvar, next:RS.ritem, k:cont(any), ws:RS.witem,nextK:RS.ritem) => 
                        let old : long = I64FetchAndAdd(&REF_COUNT(tv), ~1:long)
                        apply decLoop(nextK)
                end
            if Equal(readSet, enum(0))
            then return()
            else apply decLoop(#FF_RS_END(readSet))
        ;

        define inline @mk-exn(s : ml_string / exh :exh) noreturn = 
            do ccall M_Print(#0(s))
            throw exh(Fail(s))
        ;

        (*
         * readSet - the entire read set currently being validated
         * sentinel - the violated entry, where the read set is to be split
         *
         * This HLop is responsible for incrementing the reference counts for every
         * tref on the short path UP TO (but not includeing) the sentinel value.  This 
         * will also then split the read set at this point, and store the portion coming 
         * AFTER the sentinel in the FF_KEY position of FLS.
         *)
        define inline @incCounts(readSet : read_set, sentinel : RS.ritem, ts : stamp, newReadSet : read_set, writeSet : RS.witem / exh : exh) : () =
            fun incLoop(shortPath : RS.ritem, i:int) : ff_read_set = 
                case shortPath 
                   of RS.NilRead => @mk-exn(@"incCounts: shortPath was a NilRead\n" / exh)
                    | RS.WithK(tv:tvar, next:RS.ritem, k:cont(any), ws:RS.witem, nextK:RS.ritem) => 
                        let old : long = I64FetchAndAdd(&REF_COUNT(tv), 1:long)
                        if Equal(nextK, sentinel)
                        then 
                            let casted : with_k = (with_k) shortPath
                            do #R_ENTRY_NEXTK(casted) := RS.NilRead
                            let ff_rs : ff_read_set = alloc(ts, i, #SHORT_PATH(readSet))
                            return(ff_rs)
                        else apply incLoop(nextK, I32Add(i, 1))
                end
            let lastK : RS.ritem = #SHORT_PATH(readSet)
            if Equal(lastK, sentinel)
            then FLS.@set-key3(FF_KEY, enum(0):any, WRITE_SET, writeSet, READ_SET, newReadSet / exh)
            else 
                let ff_rs : ff_read_set = apply incLoop(lastK, 1)  
                FLS.@set-key3(FF_KEY, ff_rs, WRITE_SET, writeSet, READ_SET, newReadSet / exh)
        ;

        (* 
         * We are able to guarantee that "chkpnt" is a WithK entry, since we place a dummy
         * WithK entry at the head of the read set that is NOT on the short path.  This 
         * dummy entry has the full abort continuation stored in it and a tvar that no one 
         * has access to and therefore cannot update.  
         *)
        define inline @abort(readSet : read_set, chkpnt : RS.ritem, kCount : int, revalidate : fun(RS.ritem, RS.ritem, long, int, RS.ritem / -> ), 
                             newStamp : long, stamp : ![long,int,int,long] / exh:exh) : () =
            let chkpnt : with_k = (with_k) chkpnt
            let tv : tvar = #R_ENTRY_TVAR(chkpnt)
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
                        do #R_ENTRY_NEXT(chkpnt) := RS.NilRead  
                        let newRS : read_set = alloc(kCount, #LONG_PATH(readSet), chkpnt, chkpnt)
                        let ffRS : ff_read_set = FLS.@get-key(FF_KEY / exh)
                        do @decCounts(ffRS / exh)
                        (*incCounts will split the read set, and set the FLS appropriately*)
                        do @incCounts(readSet, chkpnt, #UNBOX(stamp), newRS, #R_ENTRY_WS(chkpnt) / exh)
                        do #UNBOX(stamp) := newStamp
                        BUMP_PABORT
                        let k : cont(any) = #R_ENTRY_K(chkpnt)
                        throw k(res)
                    else 
                        do #UNBOX(stamp) := newStamp
                        let newStamp : long = VClock.@inc(2:long / exh)
                        apply revalidate(#LONG_PATH(readSet), RS.NilRead, newStamp, 0, chkpnt)
                else 
                    do #UNBOX(stamp) := newStamp
                    let newStamp : long = VClock.@inc(2:long / exh)
                    apply revalidate(#LONG_PATH(readSet), RS.NilRead, newStamp, 0, chkpnt)
            else 
                do #UNBOX(stamp) := newStamp
                let newStamp : long = VClock.@inc(2:long / exh)
                apply revalidate(#LONG_PATH(readSet), RS.NilRead, newStamp, 0, chkpnt)
        ;

        (*
         * If this returns, then the entire read set is valid, and the time stamp
         * will have been updated to what the clock was at, prior to validation
         * readSet should be the HEAD of the read set
         *)
        define @eager-validate(readSet : read_set, stamp : ![long, int, int, long] / exh:exh) : () =
            fun eagerValidate(rs : RS.ritem, chkpnt : RS.ritem, newStamp : long, kCount : int, sentinel : RS.ritem) : () =
                if Equal(rs, sentinel)
                then 
                    if Equal(sentinel, RS.NilRead)
                    then do #UNBOX(stamp) := newStamp return()  (*this must have been our first pass through*)
                    else @abort(readSet, rs, kCount, eagerValidate, newStamp, stamp / exh) (*we tried reading from "rs" before, but it failed, try again*)
                else
                    case rs 
                       of RS.WithK(tv:tvar, next:RS.ritem, k:cont(any), ws:RS.witem, sp:RS.ritem) => 
                            let owner : long = #CURRENT_LOCK(tv)
                            if I64Lt(owner, #UNBOX(stamp))  (*still valid*)
                            then 
                                if I64Eq(I64AndB(owner, 1:long), 1:long)
                                then 
                                    @abort(readSet, rs, I32Add(kCount, 1), eagerValidate, newStamp, stamp / exh)
                                else apply eagerValidate(next, rs, newStamp, I32Add(kCount, 1), sentinel)
                            else 
                                @abort(readSet, rs, I32Add(kCount, 1), eagerValidate, newStamp, stamp / exh)
                        | RS.WithoutK(tv:tvar, next:RS.ritem) => 
                            let owner : long = #CURRENT_LOCK(tv)
                            if I64Lt(owner, #UNBOX(stamp))
                            then
                                if I64Eq(I64AndB(owner, 1:long), 1:long)
                                then 
                                    @abort(readSet, chkpnt, kCount, eagerValidate, newStamp, stamp / exh)
                                else apply eagerValidate(next, chkpnt, newStamp, kCount, sentinel)
                            else 
                                @abort(readSet, chkpnt, kCount, eagerValidate, newStamp, stamp / exh)
                        | _ => throw exh(Fail(@"fail"))
                    end
            let newStamp : long = VClock.@inc(2:long / exh)
            apply eagerValidate(#LONG_PATH(readSet), RS.NilRead, newStamp, 0, RS.NilRead)
        ;

        (*only allocates the retry loop closure if the first attempt fails*)
        define inline @read-tvar2(tv : tvar, stamp : ![stamp, int, int, long], readSet : read_set / exh : exh) : any = 
            fun lp() : any = 
                let v1 : stamp = #CURRENT_LOCK(tv)
                let res : any = #TVAR_CONTENTS(tv)
                let v2 : stamp = #CURRENT_LOCK(tv)
                let v1Lock : long = I64AndB(v1, 1:long)
                if I64Eq(v1Lock, 0:long)  (*unlocked*)
                then
                    if I64Eq(v1, v2)
                    then 
                        if I64Lt(v1, #UNBOX(stamp))
                        then return(res)
                        else do @eager-validate(readSet, stamp / exh) apply lp()
                    else do @eager-validate(readSet, stamp / exh) apply lp()
                else do @eager-validate(readSet, stamp / exh) apply lp()
            apply lp()
        ;

        define inline @read-tvar(tv : tvar, stamp : ![stamp, int, int, long], readSet : read_set/ exh : exh) : any = 
            let v1 : stamp = #CURRENT_LOCK(tv)
            let res : any = #TVAR_CONTENTS(tv)
            let v2 : stamp = #CURRENT_LOCK(tv)
            let v1Lock : long = I64AndB(v1, 1:long)
            if I64Eq(v1Lock, 0:long)  (*unlocked*)
            then
                if I64Eq(v1, v2)
                then 
                    if I64Lt(v1, #UNBOX(stamp))
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

        define @fast-forward(readSet : read_set, writeSet : RS.witem, tv:tvar, retK:cont(any), myStamp : ![long, int, int, long] / exh:exh) : () = 
            let ffInfo : ff_read_set = FLS.@get-key(FF_KEY / exh)
            if Equal(ffInfo, enum(0))
            then return()
            else (*we should only allocate the checkRS closure if we are going to actually use it*)
                fun checkRS(rs:RS.ritem, i:int) : () = 
                    case rs 
                       of RS.NilRead =>  return()
                        | RS.WithK(tv':tvar,next:RS.ritem,k:cont(any),ws:RS.witem,nextK:RS.ritem) => 
                            if Equal(tv, tv')
                            then (*tvars are equal*)
                                let res : int = ccall M_PolyEq(k, retK)
                                if I32Eq(res, 1)
                                then (*continuations are equal*)
                                    if Equal(ws, writeSet) 
                                    then (*continuations, write sets, and tvars are equal, fast forward...*)
                                        do @decCounts(ffInfo / exh)  (*decrement counts for everything on short path of ffInfo*)
                                        do FLS.@null-key(FF_KEY)  (*null out fast forward info*)
                                        (*hook the two read sets together*)
                                        let ffFirstK : with_k = (with_k) rs
                                        do #R_ENTRY_NEXTK(ffFirstK) := #SHORT_PATH(readSet) 
                                        let currentLast : with_k = (with_k) #TAIL(readSet)
                                        do #R_ENTRY_NEXT(currentLast) := RS.Stamp(#FF_RS_STAMP(ffInfo), rs)
                                        (*add to remember set*)
                                        let vp : vproc = host_vproc
                                        let rememberSet : any = vpload(REMEMBER_SET, vp)
                                        let newRemSet : [with_k, int, long, [with_k, int, long, any]] = 
                                            alloc(currentLast, R_ENTRY_NEXT, #0(myStamp), alloc(ffFirstK, R_ENTRY_NEXTK, #3(myStamp), rememberSet))
                                        do vpstore(REMEMBER_SET, vp, newRemSet)
                                        let lastK : RS.ritem = #FF_RS_END(ffInfo)
                                        let newRS : read_set = alloc(I32Add(#KCOUNT(readSet), #FF_RS_KCOUNT(ffInfo)), #LONG_PATH(readSet), lastK, lastK)
                                        let lastK : with_k = (with_k) lastK
                                        let tv : tvar = #R_ENTRY_TVAR(lastK)
                                        let current : any = @read-tvar(tv, myStamp, newRS / exh)
                                        let captureFreq : int = FLS.@get-counter2()
                                        do FLS.@set-counter(captureFreq)
                                        let abortK : cont(any) = #R_ENTRY_K(lastK)
                                        throw abortK(current)
                                    else apply checkRS(nextK, I32Add(i, 1))
                                else apply checkRS(nextK, I32Add(i, 1))
                            else apply checkRS(nextK, I32Add(i, 1))
                    end
                INC_FF(1:long)
                apply checkRS(#FF_RS_END(ffInfo), 1)
        ;

        define @get(tv:tvar / exh:exh) : any = 
            let in_trans : [bool] = FLS.@get-key(IN_TRANS / exh)
            do if(#UNBOX(in_trans))
               then return()
               else do ccall M_Print("Trying to read outside a transaction!\n")
                    let e : exn = Fail(@"Reading outside transaction\n")
                    throw exh(e)
            let myStamp : ![stamp, int, int, long] = FLS.@get-key(STAMP_KEY / exh)
            let readSet : read_set = FLS.@get-key(READ_SET / exh)
            let writeSet : RS.witem = FLS.@get-key(WRITE_SET / exh)
            fun chkLog(writeSet : RS.witem) : Option.option = (*use local copy if available*)
                 case writeSet
                     of RS.Write(tv':tvar, contents:any, tl:RS.witem) =>
                         if Equal(tv', tv)
                         then return(Option.SOME(contents))
                         else apply chkLog(tl)
                     | RS.NilWrite => return (Option.NONE)
                 end
            cont retK(x:any) = return(x)
            let localRes : Option.option = apply chkLog(writeSet)
            case localRes
               of Option.SOME(v:any) => return(v)
                | Option.NONE => 
                    let current : any = @read-tvar(tv, myStamp, readSet / exh)
                    let captureCount : int = FLS.@get-counter()
                    if I32Eq(captureCount, 1)
                    then (*capture continuation*)
                        let newRS : read_set = TL2OrderedRS.@insert-with-k(tv, retK, writeSet, readSet, myStamp / exh)
                        if I32Lte(#KCOUNT(newRS), READ_SET_BOUND)
                        then (*still room for the one we just added*)
                            let freq : int = FLS.@get-counter2()
                            do FLS.@set-counter(freq)
                            return(current)
                        else (*we just went over the limit, filter...*)
                            fun dropKs(l:RS.ritem, n:int) : int =   (*drop every other continuation*)
                                case l
                                   of RS.NilRead => return(n)
                                    | RS.WithK(_:tvar,_:RS.ritem,_:cont(any),_:RS.witem,next:RS.ritem) =>
                                        case next
                                           of RS.NilRead => return(n)
                                            | RS.WithK(_:tvar,_:RS.ritem,_:cont(any),_:RS.witem,nextNext:RS.ritem) =>
                                            (* NOTE: if compiled with -debug, this will generate warnings
                                             * that we are updating a bogus local pointer, however, given the
                                             * nature of the data structure, we do preserve the heap invariants*)
                                            let withoutKTag : enum(5) = @get-tag(UNIT/exh)
                                            let l : with_k = (with_k) l
                                            let next : with_k = (with_k) next
                                            do #R_ENTRY_K(next) := enum(0):any
                                            do #R_ENTRY_TAG(next) := withoutKTag
                                            do #R_ENTRY_NEXTK(l) := nextNext
                                            apply dropKs(nextNext, I32Sub(n, 1))
                                        end
                                end
                            let kCount : int = apply dropKs(#SHORT_PATH(newRS), #KCOUNT(newRS))
                            do #KCOUNT(newRS) := kCount
                            let freq : int = FLS.@get-counter2()
                            let newFreq : int = I32Mul(freq, 2)
                            do FLS.@set-counter(newFreq)
                            do FLS.@set-counter2(newFreq)
                            return(current)
                    else
                        let newRS : read_set = TL2OrderedRS.@insert-without-k(tv, readSet, myStamp / exh)
                        do FLS.@set-counter(I32Sub(captureCount, 1))
                        return(current)
            end
        ;

        define @put(arg:[tvar, any] / exh:exh) : unit =
            let in_trans : [bool] = FLS.@get-key(IN_TRANS / exh)
            do 
                if(#UNBOX(in_trans))
                then return()
                else 
                    do ccall M_Print("Trying to write outside a transaction!\n")
                    let e : exn = Fail(@"Writing outside transaction\n")
                    throw exh(e)
            let tv : tvar = #0(arg)
            let v : any = #1(arg)
            let writeSet : RS.witem = FLS.@get-key(WRITE_SET / exh)
            let newWriteSet : RS.witem = RS.Write(tv, v, writeSet)
            do FLS.@set-key(WRITE_SET, newWriteSet / exh)
            return(UNIT)
        ;

        define inline @finish-validate(readSet : read_set, chkpnt : RS.ritem, stamp : ![stamp,int,int,long], kCount : int, newStamp : stamp / exh:exh) : () = 
            let chkpnt : with_k = (with_k) chkpnt
            let tv : tvar = #R_ENTRY_TVAR(chkpnt)
            let oldStamp : stamp = #UNBOX(stamp)
            do #UNBOX(stamp) := newStamp
            let current : any = @read-tvar(tv, stamp, readSet / exh)
            let newRS : read_set = alloc(kCount, #LONG_PATH(readSet), chkpnt, chkpnt)
            do #R_ENTRY_NEXT(chkpnt) := RS.NilRead
            let ffRS : ff_read_set = FLS.@get-key(FF_KEY / exh)
            do @decCounts(ffRS / exh)
            (*incCounts will split the read set, and set the FLS appropriately*)
            do @incCounts(readSet, chkpnt, oldStamp, newRS, #R_ENTRY_WS(chkpnt) / exh)
            let captureFreq : int = FLS.@get-counter2()
            do FLS.@set-counter(captureFreq)
            BUMP_PABORT
            let abortK : cont(any) = #R_ENTRY_K(chkpnt)
            throw abortK(current)
        ;


        define @commit(/exh:exh) : () = 
            let startStamp : ![stamp, int, int, long] = FLS.@get-key(STAMP_KEY / exh)
            fun release(locks : RS.witem) : () = 
                case locks 
                    of RS.Write(tv:tvar, contents:any, tl:RS.witem) => 
                        do #CURRENT_LOCK(tv) := #PREV_LOCK(tv)         (*unlock*)
                        apply release(tl)
                     | RS.NilWrite => return()
                end
            let readSet : read_set = FLS.@get-key(READ_SET / exh)
            let writeSet : RS.witem = FLS.@get-key(WRITE_SET / exh)
            fun acquire(ws:RS.witem, acquired:RS.witem, lockVal : long) : RS.witem = 
                case ws
                   of RS.Write(tv:tvar, contents:any, tl:RS.witem) => 
                        let owner : long = #CURRENT_LOCK(tv)
                        if I64Eq(owner, lockVal)
                        then apply acquire(tl, acquired, lockVal)  (*we already locked this*)
                        else
                            if I64Eq(I64AndB(owner, 1:long), 0:long)
                            then
                                if I64Lt(owner, lockVal)
                                then
                                    let casRes : long = CAS(&CURRENT_LOCK(tv), owner, lockVal)
                                    if I64Eq(casRes, owner)
                                    then 
                                        do #PREV_LOCK(tv) := owner 
                                        apply acquire(tl, RS.Write(tv, contents, acquired), lockVal)
                                    else 
                                        do apply release(acquired) 
                                        do @eager-validate(readSet, startStamp / exh)
                                        apply acquire(writeSet, RS.NilWrite, I64Add(#UNBOX(startStamp), 1:long))
                                else 
                                    do apply release(acquired) 
                                    do @eager-validate(readSet, startStamp / exh)
                                    apply acquire(writeSet, RS.NilWrite, I64Add(#UNBOX(startStamp), 1:long))
                            else 
                                do apply release(acquired) 
                                do @eager-validate(readSet, startStamp / exh)
                                apply acquire(writeSet, RS.NilWrite, I64Add(#UNBOX(startStamp), 1:long))
                    | RS.NilWrite=> return(acquired)
                end
            let locks : RS.witem = apply acquire(writeSet, RS.NilWrite, I64Add(#UNBOX(startStamp), 1:long))
            fun validate(rs : RS.ritem, chkpnt : RS.ritem, newStamp : long, kCount : int) : () =
                case rs 
                   of RS.WithK(tv:tvar, next:RS.ritem, k:cont(any), ws:RS.witem, sp:RS.ritem) => 
                        let owner : long = #CURRENT_LOCK(tv)
                        let myStamp : long = #UNBOX(startStamp)
                        if I64Lt(owner, myStamp)  (*still valid*)
                        then 
                            if I64Eq(I64AndB(owner, 1:long), 1:long)
                            then
                                do apply release(locks)
                                @finish-validate(readSet, rs, startStamp, I32Add(kCount, 1), newStamp / exh)
                            else apply validate(next, rs, newStamp, I32Add(kCount, 1))
                        else 
                            if I64Eq(owner, I64Add(myStamp, 1:long))
                            then apply validate(next, rs, newStamp, I32Add(kCount, 1))
                            else
                                
                                do apply release(locks)
                                @finish-validate(readSet, rs, startStamp, I32Add(kCount, 1), newStamp / exh)
                    | RS.WithoutK(tv:tvar, next:RS.ritem) => 
                        let owner : long = #CURRENT_LOCK(tv)
                        let myStamp : long = #UNBOX(startStamp)
                        if I64Lt(owner, myStamp)
                        then
                            if I64Eq(I64AndB(owner, 1:long), 1:long)
                            then
                                do apply release(locks)
                                @finish-validate(readSet, chkpnt, startStamp, kCount, newStamp / exh)
                            else apply validate(next, chkpnt, newStamp, kCount)
                        else
                            if I64Eq(owner, I64Add(myStamp, 1:long))
                            then apply validate(next, chkpnt, newStamp, kCount)
                            else
                                
                                do apply release(locks)
                                @finish-validate(readSet, chkpnt, startStamp, kCount, newStamp / exh)
                    | RS.NilRead => return()
                end
            fun update(writes:RS.witem, newStamp : stamp) : () = 
                case writes
                    of RS.Write(tv:tvar, newContents:any, tl:RS.witem) =>
                        let newContents : any = promote(newContents)
                        do #TVAR_CONTENTS(tv) := newContents        (*update contents*)
                        do #CURRENT_LOCK(tv) := newStamp            (*unlock and update stamp (newStamp is even)*)
                        apply update(tl, newStamp)
                     | RS.NilWrite => return()
                end
            let newStamp : stamp = VClock.@inc(2:long / exh)        
            do apply validate(#LONG_PATH(readSet),RS.NilRead,newStamp,0)  
            do apply update(locks, newStamp)
            let ffRS : ff_read_set = FLS.@get-key(FF_KEY / exh)
            @decCounts(ffRS / exh)
        ;
        
        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#UNBOX(in_trans))
            then apply f(UNIT/exh)
            else 
                do FLS.@null-key(FF_KEY)
                cont enter() = 
                    cont abortK(x:any) = BUMP_FABORT throw enter()
                    let newRS : RS.read_set = RS.@new(abortK / exh)
                    do FLS.@set-key2(WRITE_SET, RS.NilWrite, READ_SET, newRS / exh)
                    let newStamp : stamp = VClock.@inc(2:long / exh)
                    let stamp : ![stamp, int] = FLS.@get-key(STAMP_KEY / exh)
                    do #UNBOX(stamp) := newStamp
                    do #UNBOX(in_trans) := true
                    cont transExh(e:exn) = 
                        do ccall M_Print("Warning: exception raised in transaction\n")
                        do @commit(/exh)
                        throw exh(e)
                    let res : any = apply f(UNIT/transExh)
                    do @commit(/transExh)
                    do #UNBOX(in_trans) := false
                    do FLS.@set-key(READ_SET, RS.NilRead / exh)
                    do FLS.@set-key(WRITE_SET, RS.NilWrite / exh)
                    return(res)
                     
                throw enter()
      ;

        define @force-abort(x : unit / exh : exh) : any = 
            let e : cont() = FLS.@get-key(ABORT_KEY / exh)
            throw e();        

        define @get-ref-count(x : tvar / exh:exh) : ml_int = 
            let c : [int] = alloc(I64ToI32(#REF_COUNT(x)))
            return(c);

    )

    type 'a tvar = 'a PartialSTM.tvar
    val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = FullAbortSTM.new
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val abort : unit -> 'a = _prim(@force-abort)
   
    val getRefCount : 'a tvar -> int = _prim(@get-ref-count)

    val _ = Ref.set(STMs.stms, ("ffTL2", (get,put,atomic,new,abort))::Ref.get STMs.stms)

end












 