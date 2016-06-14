(* ordered-tl2.pml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Software Transactional Memory with partial aborts, a bounded number of continuations
 * held in the log, and an ordered read set.
 *)

structure OrderedTL2 = 
struct 

structure RS = TL2OrderedRS

#define READ_SET_BOUND 21

    _primcode(
    
        extern void M_Print_Long2(void*, long, long);

        typedef stamp = VClock.stamp;
        typedef tvar = FullAbortSTM.tvar;
	
        typedef with_k =  ![enum(5),               (*0: tag*)
                            tvar,                  (*1: tvar operated on*)
                            RS.ritem ,             (*2: head of long path*)
                            any (*cont(any)*),     (*3: continuation*)
                            RS.witem,              (*4: write set*)
                            RS.ritem];             (*5: next read item with a continuation*)

        typedef read_set = ![int, RS.ritem, RS.ritem, RS.ritem];

        typedef stamp_rec = RS.stamp_rec;  

        define @get-tag = BoundedHybridPartialSTM.getTag;

        (* 
         * We are able to guarantee that "chkpnt" is a WithK entry, since we place a dummy
         * WithK entry at the head of the read set that is NOT on the short path.  This 
         * dummy entry has the full abort continuation stored in it and a tvar that no one 
         * has access to and therefore cannot update.  
         *)
        define inline @abort(head : RS.ritem, chkpnt : RS.ritem, kCount : int, revalidate : fun(RS.ritem, RS.ritem, long, int, bool / -> ), 
                             newStamp : long, stamp : stamp_rec / exh:exh) : () =
            let chkpnt : with_k = (with_k) chkpnt
            let freq : int = FLS.@get-counter2()
            do FLS.@set-counter(freq)
            do #R_ENTRY_NEXT(chkpnt) := RS.NilRead
            let tv : tvar = #R_ENTRY_TVAR(chkpnt)
            let v1 : long = #CURRENT_LOCK(tv)
            let res : any = #TVAR_CONTENTS(tv)
            let v2 : long = #CURRENT_LOCK(tv)
            let v1Lock : long = I64AndB(v1, 1:long)
            if U64Lte(v1, newStamp)
            then 
                if I64Eq(v1, v2)
                then 
                    let newRS : read_set = alloc(kCount, head, chkpnt, chkpnt)
                    do #START_STAMP(stamp) := newStamp
                    do FLS.@set-key(READ_SET, newRS / exh)
                    do FLS.@set-key(WRITE_SET, #R_ENTRY_WS(chkpnt) / exh)
                    BUMP_PABORT
                    let k : cont(any) = #R_ENTRY_K(chkpnt)
                    throw k(res)
                 else 
                    do #START_STAMP(stamp) := newStamp
                    let newStamp : long = VClock.@get(/ exh)
                    apply revalidate(head, RS.NilRead, newStamp, 0, true)
            else 
                do #START_STAMP(stamp) := newStamp
                let newStamp : long = VClock.@get(/ exh)
                apply revalidate(head, RS.NilRead, newStamp, 0, true)
        ;

        (*
         * If this returns, then the entire read set is valid, and the time stamp
         * will have been updated to what the clock was at, prior to validation
         * readSet should be the HEAD of the read set
         *)
        define @eager-validate(head : RS.ritem, stamp : stamp_rec / exh:exh) : () =
            fun eagerValidate(rs : RS.ritem, chkpnt : RS.ritem, newStamp : long, kCount : int, revalidating:bool) : () =
                case rs 
                   of RS.WithK(tv:tvar, next:RS.ritem, k:cont(any), ws:RS.witem, sp:RS.ritem) => 
                        if U64Lte(#CURRENT_LOCK(tv), #START_STAMP(stamp))  (*still valid*)
                        then apply eagerValidate(next, rs, newStamp, I32Add(kCount, 1), revalidating)
                        else @abort(head, rs, I32Add(kCount, 1), eagerValidate, newStamp, stamp / exh)
                    | RS.WithoutK(tv:tvar, next:RS.ritem) => 
                        if U64Lte(#CURRENT_LOCK(tv), #START_STAMP(stamp))
                        then apply eagerValidate(next, chkpnt, newStamp, kCount, revalidating)
                        else @abort(head, chkpnt, kCount, eagerValidate, newStamp, stamp / exh)
                    | RS.NilRead => 
                        if(revalidating)
                        then @abort(head, chkpnt, kCount, eagerValidate, newStamp, stamp / exh)
                        else do #START_STAMP(stamp) := newStamp return()
                end
            let newStamp : long = VClock.@get(/ exh)
            apply eagerValidate(head, RS.NilRead, newStamp, 0, false)
        ;

        (*only allocates the retry loop closure if the first attempt fails*)
        define inline @read-tvar2(tv : tvar, stamp : stamp_rec, readSet : RS.ritem / exh : exh) : any = 
            fun lp() : any = 
                let v1 : stamp = #CURRENT_LOCK(tv)
                let res : any = #TVAR_CONTENTS(tv)
                let v2 : stamp = #CURRENT_LOCK(tv)
                if U64Lte(v1, #START_STAMP(stamp))
                then
                    if I64Eq(v1, v2)
                    then return(res)
                    else do @eager-validate(readSet, stamp / exh) apply lp()
                else do @eager-validate(readSet, stamp / exh) apply lp()
            apply lp()
        ;

        define inline @read-tvar(tv : tvar, stamp : stamp_rec, readSet : RS.ritem / exh : exh) : any = 
            let v1 : stamp = #CURRENT_LOCK(tv)
            let res : any = #TVAR_CONTENTS(tv)
            let v2 : stamp = #CURRENT_LOCK(tv)
            if U64Lte(v1, #START_STAMP(stamp))
            then
                if I64Eq(v1, v2)
                then return(res)
                else 
                    do @eager-validate(readSet, stamp / exh)
                    @read-tvar2(tv, stamp, readSet / exh)
            else 
                do @eager-validate(readSet, stamp / exh)
                @read-tvar2(tv, stamp, readSet / exh)
        ;

        define @get(tv:tvar / exh:exh) : any = 
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
            let localRes : Option.option = apply chkLog(writeSet)
            case localRes
               of Option.SOME(v:any) => return(v)
                | Option.NONE => 
                    let current : any = @read-tvar(tv, myStamp, #LONG_PATH(readSet) / exh)
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
            let tv : tvar = #0(arg)
            let v : any = #1(arg)
            let writeSet : RS.witem = FLS.@get-key(WRITE_SET / exh)
            let newWriteSet : RS.witem = RS.Write(tv, v, writeSet)
            do FLS.@set-key(WRITE_SET, newWriteSet / exh)
            return(UNIT)
        ;

        define inline @finish-validate(head : RS.ritem, chkpnt : RS.ritem, stamp : stamp_rec, kCount : int / exh:exh) : () = 
            let chkpnt : with_k = (with_k) chkpnt
            let tv : tvar = #R_ENTRY_TVAR(chkpnt)
            let current : any = @read-tvar(tv, stamp, head / exh)
            let newRS : read_set = alloc(kCount, head, chkpnt, chkpnt)
            do FLS.@set-key(READ_SET, newRS / exh)
            do FLS.@set-key(WRITE_SET, #R_ENTRY_WS(chkpnt) / exh)
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
            fun acquire(ws:RS.witem, acquired:RS.witem) : RS.witem = 
                case ws
                   of RS.Write(tv:tvar, contents:any, tl:RS.witem) => 
                        let owner : long = #CURRENT_LOCK(tv)
                        if U64Eq(owner, #LOCK_VAL(startStamp))
                        then apply acquire(tl, acquired)  (*we already locked this*)
                        else
                            if U64Lte(owner, #0(startStamp))
                            then
                                if BCAS(&CURRENT_LOCK(tv), owner, #LOCK_VAL(startStamp))
                                then 
                                    do #PREV_LOCK(tv) := owner 
                                    apply acquire(tl, RS.Write(tv, contents, acquired))
                                else 
                                    do apply release(acquired) 
                                    do @eager-validate(#LONG_PATH(readSet), startStamp / exh)
                                    apply acquire(writeSet, RS.NilWrite)
                            else 
                                do apply release(acquired) 
                                do @eager-validate(#LONG_PATH(readSet), startStamp / exh)
                                apply acquire(writeSet, RS.NilWrite)
                    | RS.NilWrite=> return(acquired)
                end
            let locks : RS.witem = apply acquire(writeSet, RS.NilWrite)
            fun validate(rs : RS.ritem, chkpnt : RS.ritem, newStamp : long, kCount : int) : () =
                case rs 
                   of RS.WithK(tv:tvar, next:RS.ritem, k:cont(any), ws:RS.witem, sp:RS.ritem) => 
                        let owner : long = #CURRENT_LOCK(tv)
                        if I64Eq(owner, #LOCK_VAL(startStamp))
                        then apply validate(next, rs, newStamp, I32Add(kCount, 1))
                        else 
                            if U64Lte(owner, #START_STAMP(startStamp))  (*still valid*)
                            then apply validate(next, rs, newStamp, I32Add(kCount, 1))
                            else 
                                do apply release(locks)
                                do #START_STAMP(startStamp) := newStamp
                                @finish-validate(#LONG_PATH(readSet), rs, startStamp, I32Add(kCount, 1) / exh)
                    | RS.WithoutK(tv:tvar, next:RS.ritem) => 
                        let owner : long = #CURRENT_LOCK(tv)
                        if I64Eq(owner, #LOCK_VAL(startStamp))
                        then apply validate(next, chkpnt, newStamp, kCount)
                        else 
                            if U64Lte(owner, #START_STAMP(startStamp))
                            then apply validate(next, chkpnt, newStamp, kCount)
                            else
                                do apply release(locks)
                                do #START_STAMP(startStamp) := newStamp
                                @finish-validate(#LONG_PATH(readSet), chkpnt, startStamp, kCount / exh)
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
            if I64Eq(newStamp, #0(startStamp))
            then apply update(locks, I64Add(newStamp, 2:long))
            else 
                let newStamp : stamp = I64Add(newStamp, 2:long)
                do apply validate(#LONG_PATH(readSet),RS.NilRead,newStamp,0)  
                apply update(locks, newStamp)
        ;
        
        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#UNBOX(in_trans))
            then apply f(UNIT/exh)
            else 
                cont enter() = 
                    cont abortK(x:any) = BUMP_FABORT throw enter()
                    let newRS : RS.read_set = RS.@new(abortK / exh)
                    do FLS.@set-key(READ_SET, newRS / exh)  (*initialize STM log*)
                    do FLS.@set-key(WRITE_SET, RS.NilWrite / exh)
                    let newStamp : stamp = VClock.@get(/ exh)
                    let stamp : stamp_rec = FLS.@get-key(STAMP_KEY / exh)
                    do #START_STAMP(stamp) := newStamp
                    do #LOCK_VAL(stamp) := SET_MSB(#THREAD_ID(stamp))
                    do #UNBOX(in_trans) := true
                    let res : any = apply f(UNIT/exh)
                    do @commit(/exh)
                    do #UNBOX(in_trans) := false
                    do FLS.@set-key(READ_SET, RS.NilRead / exh)
                    do FLS.@set-key(WRITE_SET, RS.NilWrite / exh)
                    return(res)
                     
                throw enter()
      ;

      define @force-abort(x : unit / exh : exh) : any = 
         let e : cont() = FLS.@get-key(ABORT_KEY / exh)
         throw e();        

    )

    type 'a tvar = 'a PartialSTM.tvar
    val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = FullAbortSTM.new
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val abort : unit -> 'a = _prim(@force-abort)
   
end












 
