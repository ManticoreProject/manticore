(* ff-tl2.pml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Software Transactional Memory with partial aborts, a bounded number of continuations
 * held in the log, an ordered read set, and fast forwarding.
 * 
 * note that this is using bit masking to limit fast forward checks
 *)

structure FFTL2 = 
struct 

structure RS = TL2OrderedRS

#define READ_SET_BOUND 21

    _primcode(
    
        extern void M_Print_Long2(void*, long, long);
        extern void* fastForwardTL2(void*,void*,void*,void*,void*,void*,void*,void*) __attribute__((alloc));
        extern void* ff_tl2_read_tvar(void*,void*,void*,void*,void*);

        typedef stamp = VClock.stamp;
        typedef tvar = FullAbortSTM.tvar;
	
        typedef with_k = OrderedTL2.with_k;

        typedef read_set = ![int, RS.ritem, RS.ritem, RS.ritem];

        typedef ff_read_set = RS.ritem;  

        typedef stamp_rec = TL2OrderedRS.stamp_rec;

        define @get-tag = BoundedHybridPartialSTM.getTag;

        (*
         * This HLop is responsible for clearing the thread's bit for everything
         * on the short path of the FF read set, if one exists.  This should be called
         * after committing a transaction and when aborting a transction.
         *)
        define inline @clear-bits(readSet : ff_read_set, myStamp : stamp_rec) : () =
            let myBit : long = I64LSh(1:long, #THREAD_ID(myStamp))
            let myBitComp : long = I64NotB(myBit)
            fun unMaskLoop(i:RS.ritem) : () =
                case i 
                   of RS.NilRead => return()
                    | RS.WithK(tv:tvar, _:RS.ritem, _:cont(any), _:RS.witem, next:RS.ritem) => 
                        let stamp : stamp = #REF_COUNT(tv)
                        let new : stamp = I64AndB(stamp, myBitComp)
                        let old : stamp = CAS(&REF_COUNT(tv), stamp, new)
                        if I64Eq(stamp, old)
                        then apply unMaskLoop(next)
                        else apply unMaskLoop(i)
                end
            apply unMaskLoop(readSet)
        ;

        (*
         * readSet - the entire read set currently being validated
         * sentinel - the violated entry, where the read set is to be split
         *
         * This HLop is responsible for setting the thread's bit in the tref's mask for every
         * tref on the short path UP TO (but not including) the sentinel value.  This 
         * will also then split the read set at this point, and store the portion coming 
         * AFTER the sentinel in the FF_KEY position of FLS.
         * Note that it is possible that the sentinel does not exist on the short path.
         * This case arises when we perform a full abort with the dummy checkpoint we
         * put at the head of every read set
         *)
        define inline @set-bits(readSet : read_set, sentinel : RS.ritem, myStamp : stamp_rec, newReadSet : read_set, 
                                 writeSet : RS.witem / exh:exh) : () = 
            let myBit : long = I64LSh(1:long, #THREAD_ID(myStamp))
            fun maskLoop(i:RS.ritem) : () = 
                case i 
                   of RS.NilRead => return()
                    | RS.WithK(tv:tvar,_:RS.ritem, _:cont(any), _:RS.witem, next:RS.ritem) => 
                        let stamp : stamp = #REF_COUNT(tv)
                        let new : stamp = I64OrB(stamp, myBit)
                        let old : stamp = CAS(&REF_COUNT(tv), stamp, new)
                        if I64Eq(stamp, old)
                        then 
                            if Equal(sentinel, next)
                            then 
                                let casted : with_k = (with_k) i
                                do #R_ENTRY_NEXTK(casted) := RS.NilRead
                                return()
                            else apply maskLoop(next)
                        else apply maskLoop(i)
                end
            let lastK : RS.ritem = #SHORT_PATH(readSet)
            if Equal(lastK, sentinel)
            then FLS.@set-key3(FF_KEY, enum(0):any, WRITE_SET, writeSet, READ_SET, newReadSet / exh)
            else 
                do apply maskLoop(lastK)
                FLS.@set-key3(FF_KEY, lastK, WRITE_SET, writeSet, READ_SET, newReadSet / exh)
        ;

        define @force-abort(x : unit / exh : exh) : any = 
            let myStamp : stamp_rec = FLS.@get-key(STAMP_KEY / exh)
            let ffInfo : ff_read_set = FLS.@get-key(FF_KEY / exh)
            do @clear-bits(ffInfo, myStamp)
            let rs : read_set = FLS.@get-key(READ_SET / exh)
            do #OLD_STAMP(myStamp) := #CURRENT_STAMP(myStamp)
            do @set-bits(rs, RS.NilRead, myStamp, rs, RS.NilWrite / exh)
            let chkpnt : with_k = (with_k)#LONG_PATH(rs) (*first entry is the full abort continuation*)
            let k : cont(any) = #R_ENTRY_K(chkpnt)
            throw k(enum(0))
        ;


        (* 
         * We are able to guarantee that "chkpnt" is a WithK entry, since we place a dummy
         * WithK entry at the head of the read set that is NOT on the short path.  This 
         * dummy entry has the full abort continuation stored in it and a tvar that no one 
         * has access to and therefore cannot update.  
         *)
        define inline @abort(readSet : read_set, chkpnt : RS.ritem, kCount : int, revalidate : fun(RS.ritem, RS.ritem, long, int, RS.ritem / -> ), 
                             newStamp : long, stamp : stamp_rec / exh:exh) : () =
            let chkpnt : with_k = (with_k) chkpnt
            let tv : tvar = #R_ENTRY_TVAR(chkpnt)
            let v1 : long = #CURRENT_LOCK(tv)
            do FenceRead()
            let res : any = #TVAR_CONTENTS(tv)
            do FenceRead()
            let v2 : long = #CURRENT_LOCK(tv)
            if I64Eq(I64AndB(v1, 1:long), 0:long)  (*unlocked*)
            then
                if I64Eq(v1, v2)
                then 
                    if I64Lte(v1, newStamp)
                    then 
                        do #R_ENTRY_NEXT(chkpnt) := RS.NilRead  
                        let newRS : read_set = alloc(kCount, #LONG_PATH(readSet), chkpnt, chkpnt)
                        let ffRS : ff_read_set = FLS.@get-key(FF_KEY / exh)
                        do @clear-bits(ffRS, stamp)
                        (*incCounts will split the read set, and set the FLS appropriately*)
                        do @set-bits(readSet, chkpnt, stamp, newRS, #R_ENTRY_WS(chkpnt) / exh)
                        (*do FLS.@set-key3(FF_KEY, enum(0):any, WRITE_SET, #R_ENTRY_WS(chkpnt), READ_SET, newRS / exh)*)
                        do #UNBOX(stamp) := newStamp
                        BUMP_PABORT
                        let k : cont(any) = #R_ENTRY_K(chkpnt)
                        throw k(res)
                    else 
                        do #CURRENT_STAMP(stamp) := newStamp
                        let newStamp : long = VClock.@get(/ exh)
                        apply revalidate(#LONG_PATH(readSet), RS.NilRead, newStamp, 0, chkpnt)
                else 
                    do #CURRENT_STAMP(stamp) := newStamp
                    let newStamp : long = VClock.@get(/ exh)
                    apply revalidate(#LONG_PATH(readSet), RS.NilRead, newStamp, 0, chkpnt)
            else 
                do #CURRENT_STAMP(stamp) := newStamp
                let newStamp : long = VClock.@get(/ exh)
                apply revalidate(#LONG_PATH(readSet), RS.NilRead, newStamp, 0, chkpnt)
        ;

        (*
         * If this returns, then the entire read set is valid, and the time stamp
         * will have been updated to what the clock was at, prior to validation
         *)
        define @eager-validate(readSet : read_set, stamp : stamp_rec / exh:exh) : () =
            fun eagerValidate(rs : RS.ritem, chkpnt : RS.ritem, newStamp : long, kCount : int, sentinel : RS.ritem) : () =
                if Equal(rs, sentinel)
                then 
                    if Equal(sentinel, RS.NilRead)
                    then 
                        do #CURRENT_STAMP(stamp) := newStamp 
                        return()  (*this must have been our first pass through*)
                    else @abort(readSet, rs, kCount, eagerValidate, newStamp, stamp/ exh) (*we tried reading from "rs" before, but it failed, try again*)
                else
                    case rs 
                       of RS.WithK(tv:tvar, next:RS.ritem, k:cont(any), ws:RS.witem, sp:RS.ritem) => 
                            let owner : long = #CURRENT_LOCK(tv)
                            if I64Lte(owner, #UNBOX(stamp))  (*still valid*)
                            then 
                                if I64Eq(I64AndB(owner, 1:long), 1:long)
                                then 
                                    @abort(readSet, rs, I32Add(kCount, 1), eagerValidate, newStamp, stamp / exh)
                                else apply eagerValidate(next, rs, newStamp, I32Add(kCount, 1), sentinel)
                            else 
                                @abort(readSet, rs, I32Add(kCount, 1), eagerValidate, newStamp, stamp / exh)
                        | RS.WithoutK(tv:tvar, next:RS.ritem) => 
                            let owner : long = #CURRENT_LOCK(tv)
                            if I64Lte(owner, #UNBOX(stamp))
                            then
                                if I64Eq(I64AndB(owner, 1:long), 1:long)
                                then 
                                    @abort(readSet, chkpnt, kCount, eagerValidate, newStamp, stamp / exh)
                                else apply eagerValidate(next, chkpnt, newStamp, kCount, sentinel)
                            else 
                                @abort(readSet, chkpnt, kCount, eagerValidate, newStamp, stamp / exh)
                        | _ => throw exh(Fail(@"fail"))
                    end
            let newStamp : long = VClock.@get(/ exh)
            let oldOldStamp : stamp = #OLD_STAMP(stamp)
            do #OLD_STAMP(stamp) := #CURRENT_STAMP(stamp)
            do apply eagerValidate(#LONG_PATH(readSet), RS.NilRead, newStamp, 0, RS.NilRead)
            do #OLD_STAMP(stamp) := oldOldStamp
            return()
        ;

        (*only allocates the retry loop closure if the first attempt fails*)
        define inline @read-tvar2(tv : tvar, stamp : stamp_rec, readSet : read_set / exh : exh) : any = 
            fun lp() : any = 
                let v1 : stamp = #CURRENT_LOCK(tv)
                do FenceRead()
                let res : any = #TVAR_CONTENTS(tv)
                do FenceRead()
                let v2 : stamp = #CURRENT_LOCK(tv)
                let v1Lock : long = I64AndB(v1, 1:long)
                if I64Eq(v1Lock, 0:long)  (*unlocked*)
                then
                    if I64Eq(v1, v2)
                    then 
                        if I64Lte(v1, #UNBOX(stamp))
                        then return(res)
                        else do @eager-validate(readSet, stamp / exh) apply lp()
                    else do @eager-validate(readSet, stamp / exh) apply lp()
                else do @eager-validate(readSet, stamp / exh) apply lp()
            apply lp()
        ;

        define inline @read-tvar(tv : tvar, stamp : stamp_rec, readSet : read_set/ exh : exh) : any = 
            let v1 : stamp = #CURRENT_LOCK(tv)
            do FenceRead()
            let res : any = #TVAR_CONTENTS(tv)
            do FenceRead()
            let v2 : stamp = #CURRENT_LOCK(tv)
            let v1Lock : long = I64AndB(v1, 1:long)
            if I64Eq(v1Lock, 0:long)  (*unlocked*)
            then
                if I64Eq(v1, v2)
                then 
                    if I64Lte(v1, #UNBOX(stamp))
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

        define inline @c-read-tvar(tv:tvar, stamp : stamp_rec, readSet : read_set / exh:exh) : any = 
            let vp : vproc = host_vproc
            let clock : ![long] = VClock.@get-boxed(/exh)
            let ff_rs : RS.ritem = #SHORT_PATH(readSet) (*if the read fails, set this to the FF RS, C will set it up correctly*)
            let res : any = ccall ff_tl2_read_tvar(tv, vp, stamp, clock, readSet)
            if Equal(res, readSet)
            then 
                let newVal : any = (any)#TAIL(readSet) (*the result to be passed to the abort continuation*)
                let chkpnt : with_k = (with_k) #SHORT_PATH(readSet)
                let ws : RS.witem = #R_ENTRY_WS(chkpnt)
                let oldFFRS : ff_read_set = FLS.@get-key(FF_KEY / exh)
                do @clear-bits(oldFFRS, stamp)
                do 
                    if Equal(ff_rs, chkpnt)
                    then FLS.@set-key2(FF_KEY, enum(0), WRITE_SET, ws / exh)
                    else FLS.@set-key2(FF_KEY, ff_rs, WRITE_SET, ws / exh)
                do #TAIL(readSet) := (RS.ritem) chkpnt
                let k : cont(any) = #R_ENTRY_K(chkpnt)
                BUMP_PABORT
                throw k(newVal)
            else return(res)
        ;

        define @fast-forward(readSet : read_set, writeSet : RS.witem, tv:tvar, retK:cont(any), myStamp : stamp_rec / exh:exh) : () = 
            let ffInfo : ff_read_set = FLS.@get-key(FF_KEY / exh)
            if Equal(ffInfo, enum(0))
            then return()
            else 
                let vp : vproc = host_vproc
                let clock : ![long] = VClock.@get-boxed(/exh)
                INC_FF(1:long) 
                         (*[num continuations, head, tail, value to be applied]*)
                let res : ![int, RS.ritem, RS.ritem, any] = ccall fastForwardTL2(readSet, ffInfo, writeSet, tv, retK, myStamp, clock, vp)
                if Equal(res, UNIT)
                then return()
                else 
                    BUMP_KCOUNT
                    do @clear-bits(ffInfo, myStamp)
                    let current : any = #3(res)
                    do #3(res) := (any)#2(res)
                    case #2(res) 
                       of RS.WithK(tv:tvar,next:RS.ritem,k:cont(any),ws:RS.witem,nextK:RS.ritem) => 
                            do FLS.@set-key3(FF_KEY, enum(0), WRITE_SET, ws, READ_SET, res / exh)
                            throw k(current)
                        | _ => throw exh(Fail("@Impossible!"))
                    end
        ;

        define @get(tv:tvar / exh:exh) : any = 
            let in_trans : [bool] = FLS.@get-key(IN_TRANS / exh)
            do if(#UNBOX(in_trans))
               then return()
               else do ccall M_Print("Trying to read outside a transaction!\n")
                    let e : exn = Fail(@"Reading outside transaction\n")
                    throw exh(e)
            let myStamp : stamp_rec = FLS.@get-key(STAMP_KEY / exh)
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
            let myBit : long = I64LSh(1:long, #THREAD_ID(myStamp))
            let masked : long = I64AndB(#REF_COUNT(tv), myBit)
            do  if I64Eq(masked, myBit)
                then @fast-forward(readSet, writeSet, tv, retK, myStamp / exh)
                else return()  
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
                            fun dropKsFFTL2(l:RS.ritem, n:int) : int =   (*drop every other continuation*)
                                case l
                                   of RS.NilRead => return(n)
                                    | RS.WithK(_:tvar,_:RS.ritem,_:cont(any),_:RS.witem,next:RS.ritem) =>
                                        case next
                                           of RS.NilRead => return(n)
                                            | RS.WithK(_:tvar,_:RS.ritem,_:cont(any),_:RS.witem,nextNext:RS.ritem) =>
                                            let vp : vproc = host_vproc
                                            let rs : any = vpload(REMEMBER_SET, vp)
                                            let newRemSet : [RS.ritem, int, long, any] = alloc(l, R_ENTRY_NEXTK, #3(myStamp), rs)
                                            do vpstore(REMEMBER_SET, vp, newRemSet)
                                            let withoutKTag : enum(5) = @get-tag(UNIT/exh)
                                            let l : with_k = (with_k) l
                                            let next : with_k = (with_k) next
                                            do #R_ENTRY_K(next) := enum(0):any
                                            do #R_ENTRY_TAG(next) := withoutKTag
                                            do #R_ENTRY_NEXTK(l) := nextNext
                                            apply dropKsFFTL2(nextNext, I32Sub(n, 1))
                                        end
                                end
                            let kCount : int = apply dropKsFFTL2(#SHORT_PATH(newRS), #KCOUNT(newRS))
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

        define inline @finish-validate(readSet : read_set, chkpnt : RS.ritem, stamp : stamp_rec, kCount : int, newStamp : stamp / exh:exh) : () = 
            let chkpnt : with_k = (with_k) chkpnt
            let tv : tvar = #R_ENTRY_TVAR(chkpnt)
            do #OLD_STAMP(stamp) := #CURRENT_STAMP(stamp)
            do #CURRENT_STAMP(stamp) := newStamp
            do #R_ENTRY_NEXT(chkpnt) := RS.NilRead
            let current : any = @read-tvar(tv, stamp, readSet / exh)
            let newRS : read_set = alloc(kCount, #LONG_PATH(readSet), chkpnt, chkpnt)
            let ffRS : ff_read_set = FLS.@get-key(FF_KEY / exh)
            do @clear-bits(ffRS, stamp)
            (*incCounts will split the read set, and set the FLS appropriately*)
            do @set-bits(readSet, chkpnt, stamp, newRS, #R_ENTRY_WS(chkpnt) / exh)
            let captureFreq : int = FLS.@get-counter2()
            do FLS.@set-counter(captureFreq)
            BUMP_PABORT
            let abortK : cont(any) = #R_ENTRY_K(chkpnt)
            throw abortK(current)
        ;


        define @commit(/exh:exh) : () = 
            let startStamp : stamp_rec = FLS.@get-key(STAMP_KEY / exh)
            fun release(locks : RS.witem) : () = 
                case locks 
                    of RS.Write(tv:tvar, contents:any, tl:RS.witem) => 
                        do #CURRENT_LOCK(tv) := #PREV_LOCK(tv)         (*unlock*)
                        apply release(tl)
                     | RS.NilWrite => return()
                end
            let readSet : read_set = FLS.@get-key(READ_SET / exh)
            let writeSet : RS.witem = FLS.@get-key(WRITE_SET / exh)
            let lockVal : long = I64Add(I64LSh(#THREAD_ID(startStamp), 1:long), 1:long)
            fun acquire(ws:RS.witem, acquired:RS.witem) : RS.witem = 
                case ws
                   of RS.Write(tv:tvar, contents:any, tl:RS.witem) => 
                        let owner : long = #CURRENT_LOCK(tv)
                        if I64Eq(owner, lockVal)
                        then apply acquire(tl, acquired)  (*we already locked this*)
                        else
                            if I64Eq(I64AndB(owner, 1:long), 0:long)
                            then
                                if I64Lte(owner, #UNBOX(startStamp))
                                then
                                    let casRes : long = CAS(&CURRENT_LOCK(tv), owner, lockVal)
                                    if I64Eq(casRes, owner)
                                    then 
                                        do #PREV_LOCK(tv) := owner 
                                        apply acquire(tl, RS.Write(tv, contents, acquired))
                                    else 
                                        do apply release(acquired) 
                                        do @eager-validate(readSet, startStamp / exh)
                                        apply acquire(writeSet, RS.NilWrite)
                                else 
                                    do apply release(acquired) 
                                    do @eager-validate(readSet, startStamp / exh)
                                    apply acquire(writeSet, RS.NilWrite)
                            else 
                                do apply release(acquired) 
                                do @eager-validate(readSet, startStamp / exh)
                                apply acquire(writeSet, RS.NilWrite)
                    | RS.NilWrite=> return(acquired)
                end
            let locks : RS.witem = apply acquire(writeSet, RS.NilWrite)
            fun validate(rs : RS.ritem, chkpnt : RS.ritem, newStamp : long, kCount : int) : () =
                case rs 
                   of RS.WithK(tv:tvar, next:RS.ritem, k:cont(any), ws:RS.witem, sp:RS.ritem) => 
                        let owner : long = #CURRENT_LOCK(tv)
                        if I64Eq(owner, lockVal)
                        then apply validate(next, rs, newStamp, I32Add(kCount, 1))
                        else 
                            if I64Lte(owner, #UNBOX(startStamp))  (*still valid*)
                            then 
                                if I64Eq(I64AndB(owner, 1:long), 1:long)
                                then
                                    do apply release(locks)
                                    @finish-validate(readSet, rs, startStamp, I32Add(kCount, 1), newStamp / exh)
                                else apply validate(next, rs, newStamp, I32Add(kCount, 1))
                            else 
                                do apply release(locks)
                                @finish-validate(readSet, rs, startStamp, I32Add(kCount, 1), newStamp / exh)
                    | RS.WithoutK(tv:tvar, next:RS.ritem) => 
                        let owner : long = #CURRENT_LOCK(tv)
                        if I64Eq(owner, lockVal)
                        then apply validate(next, chkpnt, newStamp, kCount)
                        else 
                            if I64Lte(owner, #CURRENT_STAMP(startStamp))
                            then
                                if I64Eq(I64AndB(owner, 1:long), 1:long)
                                then
                                    do apply release(locks)
                                    @finish-validate(readSet, chkpnt, startStamp, kCount, newStamp / exh)
                                else apply validate(next, chkpnt, newStamp, kCount)
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
            if I64Eq(newStamp, #UNBOX(startStamp))
            then 
                do apply update(locks, I64Add(newStamp, 2:long))
                let ffRS : ff_read_set = FLS.@get-key(FF_KEY / exh)
                @clear-bits(ffRS, startStamp)
            else 
                let newStamp : stamp = I64Add(newStamp, 2:long)
                do apply validate(#LONG_PATH(readSet),RS.NilRead,newStamp,0)
                do apply update(locks, newStamp)
                let ffRS : ff_read_set = FLS.@get-key(FF_KEY / exh)
                @clear-bits(ffRS, startStamp)
        ;
        
        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#UNBOX(in_trans))
            then apply f(UNIT/exh)
            else 
                cont enter() = 
                    let freq : int = FLS.@get-counter2()
                    do FLS.@set-counter(freq) (*set counter to frequency*)
                    cont abortK(x:any) = BUMP_FABORT throw enter()
                    let newRS : RS.read_set = RS.@new(abortK / exh)
                    do FLS.@set-key2(WRITE_SET, RS.NilWrite, READ_SET, newRS / exh)
                    let newStamp : stamp = VClock.@get(/ exh)
                    let stamp : stamp_rec = FLS.@get-key(STAMP_KEY / exh)
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
                    do FLS.@null-key(FF_KEY)
                    return(res)
                throw enter()
      ;

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












 
