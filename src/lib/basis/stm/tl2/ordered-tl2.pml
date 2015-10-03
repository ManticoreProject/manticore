(* ordered-tl2.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
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
    
        typedef stamp = VClock.stamp;
        typedef tvar = FullAbortSTM.tvar;
	
        typedef with_k =  ![enum(5),               (*0: tag*)
                            tvar,                  (*1: tvar operated on*)
                            RS.ritem ,                 (*2: next on long path*)
                            any (*cont(any)*),     (*3: continuation*)
                            RS.witem,                  (*4: write set*)
                            RS.ritem];                 (*5: next read item with a continuation*)

        typedef read_set = ![int, RS.ritem, RS.ritem, RS.ritem];

        define @new(x:any / exh:exh) : tvar = 
            let tv : tvar = alloc(x, 0:long, 0:long)
            let tv : tvar = promote(tv)
            return(tv)
        ;

        define @get-tag = BoundedHybridPartialSTM.getTag;

        define inline @abort(head : RS.ritem, chkpnt : RS.ritem, kCount : int, revalidate : fun(RS.ritem, RS.ritem, long, int / -> ), 
                             newStamp : long, stamp : ![long,int,int,long] / exh:exh) : () =
            do ccall M_Print("Aborting\n")
            case chkpnt 
               of RS.NilRead => 
                    let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                    do ccall M_Print_Int("Fully aborting, kCount = %d\n", kCount)
                    throw abortK()
                | RS.WithK(tv:tvar, next:RS.ritem, k : cont(any), ws:RS.witem, sp:RS.ritem) => 
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
                                let newRS : read_set = alloc(kCount, head, chkpnt, chkpnt)
                                do #0(stamp) := newStamp
                                do FLS.@set-key(READ_SET, newRS / exh)
                                do FLS.@set-key(WRITE_SET, ws / exh)
                                BUMP_PABORT
                                throw k(res)
                            else 
                                do #0(stamp) := newStamp
                                let newStamp : long = VClock.@inc(2:long / exh)
                                apply revalidate(head, RS.NilRead, newStamp, 0)
                        else 
                            do #0(stamp) := newStamp
                            let newStamp : long = VClock.@inc(2:long / exh)
                            apply revalidate(head, RS.NilRead, newStamp, 0)
                    else 
                        do #0(stamp) := newStamp
                        let newStamp : long = VClock.@inc(2:long / exh)
                        apply revalidate(head, RS.NilRead, newStamp, 0)
            end
        ;

        (*
         * If this returns, then the entire read set is valid, and the time stamp
         * will have been updated to what the clock was at, prior to validation
         * readSet should be the HEAD of the read set
         *)
        define @eager-validate(readSet : RS.ritem, stamp : ![long, int, int, long] / exh:exh) : () =
            fun eagerValidate(rs : RS.ritem, chkpnt : RS.ritem, newStamp : long, kCount : int) : () =
                case rs 
                   of RS.WithK(tv:tvar, next:RS.ritem, k:cont(any), ws:RS.witem, sp:RS.ritem) => 
                        let owner : long = #CURRENT_LOCK(tv)
                        if I64Lt(owner, #0(stamp))  (*still valid*)
                        then 
                            if I64Eq(I64AndB(owner, 1:long), 1:long)
                            then 
                                do ccall M_Print("eager withK, locked\n") 
                                @abort(readSet, rs, I32Add(kCount, 1), eagerValidate, newStamp, stamp / exh)
                            else apply eagerValidate(next, rs, newStamp, I32Add(kCount, 1))
                        else 
                            do ccall M_Print("eager withK, out of date\n") 
                            @abort(readSet, rs, I32Add(kCount, 1), eagerValidate, newStamp, stamp / exh)
                    | RS.WithoutK(tv:tvar, next:RS.ritem) => 
                        let owner : long = #CURRENT_LOCK(tv)
                        if I64Lt(owner, #0(stamp))
                        then
                            if I64Eq(I64AndB(owner, 1:long), 1:long)
                            then 
                                do ccall M_Print("eager withoutK, locked\n") 
                                @abort(readSet, chkpnt, kCount, eagerValidate, newStamp, stamp / exh)
                            else apply eagerValidate(next, chkpnt, newStamp, kCount)
                        else 
                            do ccall M_Print("eager withoutK, out of date\n") 
                            @abort(readSet, chkpnt, kCount, eagerValidate, newStamp, stamp / exh)
                    | RS.NilRead => return()
                end
            let newStamp : long = VClock.@inc(2:long / exh)
            apply eagerValidate(readSet, RS.NilRead, newStamp, 0)
        ;

        (*only allocates the retry loop closure if the first attempt fails*)
        define inline @read-tvar2(tv : tvar, stamp : ![stamp, int, int, long], readSet : RS.ritem / exh : exh) : any = 
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

        define inline @read-tvar(tv : tvar, stamp : ![stamp, int, int, long], readSet : RS.ritem / exh : exh) : any = 
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
            let writeSet : RS.witem = FLS.@get-key(WRITE_SET / exh)
            let newWriteSet : RS.witem = RS.Write(tv, v, writeSet)
            do FLS.@set-key(WRITE_SET, newWriteSet / exh)
            return(UNIT)
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
            let rawStamp: long = #0(startStamp)
            let lockVal : long = I64Add(rawStamp, 1:long)
            fun acquire(ws:RS.witem, acquired:RS.witem) : RS.witem = 
                case ws
                   of RS.Write(tv:tvar, contents:any, tl:RS.witem) => 
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
                                        apply acquire(tl, RS.Write(tv, contents, acquired))
                                    else 
                                        do apply release(acquired) 
                                        do @eager-validate(#LONG_PATH(readSet), startStamp / exh)
                                        apply acquire(writeSet, RS.NilWrite)
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
                        if I64Lt(owner, #0(startStamp))  (*still valid*)
                        then 
                            if I64Eq(I64AndB(owner, 1:long), 1:long)
                            then
                                do apply release(locks) 
                                do ccall M_Print("Commit time withK, locked\n")
                                @abort(#LONG_PATH(readSet), rs, I32Add(kCount, 1), validate, newStamp, startStamp / exh)
                            else apply validate(next, rs, newStamp, I32Add(kCount, 1))
                        else 
                            do apply release(locks) 
                            do ccall M_Print("Commit time withK, out of date\n")
                            @abort(#LONG_PATH(readSet), rs, I32Add(kCount, 1), validate, newStamp, startStamp / exh)
                    | RS.WithoutK(tv:tvar, next:RS.ritem) => 
                        let owner : long = #CURRENT_LOCK(tv)
                        if I64Lt(owner, #0(startStamp))
                        then
                            if I64Eq(I64AndB(owner, 1:long), 1:long)
                            then 
                                do apply release(locks)
                                do ccall M_Print("Commit time withoutK, locked\n")
                                @abort(#LONG_PATH(readSet), chkpnt, kCount, validate, newStamp, startStamp / exh)
                            else apply validate(next, chkpnt, newStamp, kCount)
                        else 
                            do apply release(locks)
                            do ccall M_Print_Long("Commit time withoutK, out of date\n")
                            @abort(#LONG_PATH(readSet), chkpnt, kCount, validate, newStamp, startStamp / exh)
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
            return()
        ;
        
        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#0(in_trans))
            then apply f(UNIT/exh)
            else 
                cont enter() = 
                    cont abortK(x:any) = BUMP_FABORT throw enter()
                    let newRS : RS.read_set = RS.@new(abortK / exh)
                    do FLS.@set-key(READ_SET, newRS / exh)  (*initialize STM log*)
                    do FLS.@set-key(WRITE_SET, RS.NilWrite / exh)
                    let newStamp : stamp = VClock.@inc(2:long / exh)
                    let stamp : ![stamp, int] = FLS.@get-key(STAMP_KEY / exh)
                    do #1(stamp) := 0
                    do #0(stamp) := newStamp
                    do #0(in_trans) := true
                    cont transExh(e:exn) = 
                        do ccall M_Print("Warning: exception raised in transaction\n")
                        do @commit(/exh) 
                        throw exh(e)
                    let res : any = apply f(UNIT/transExh)
                    do @commit(/transExh)
                    do ccall M_Print("Committing\n")
                    do #0(in_trans) := false
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
    val new : 'a -> 'a tvar = _prim(@new)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val abort : unit -> 'a = _prim(@force-abort)
   
    val _ = Ref.set(STMs.stms, ("orderedTL2", (get,put,atomic,new,abort))::Ref.get STMs.stms)

end












 
