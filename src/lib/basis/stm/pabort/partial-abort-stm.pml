(* stm.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Software Transactional Memory with partial aborts.
 *)
 
structure PartialSTM = (* :
    sig
	
*)
struct

    (*flat representation for read and write sets*)
    datatype 'a item = Read of 'a * 'a * 'a * 'a | Write of 'a * 'a * 'a | NilItem

    _primcode(

        extern void * M_Print_Int(void *, int);
        extern void * M_Print_Int2(void *, int, int);
        extern void M_Print_Long (void *, long);
        extern void M_BumpCounter(void * , int);
        extern int M_SumCounter(int);
        extern void M_ZeroCounters();

        define @zero-counters(x:unit / e : exh) : unit = 
            do ccall M_ZeroCounters()
            return(UNIT);
        
        typedef stamp = VClock.stamp;
        typedef tvar = ![any, long, stamp]; (*contents, lock, version stamp*)

        define @new(x:any / exh:exh) : tvar = 
            let tv : tvar = alloc(x, 0:long, 0:long)
            let tv : tvar = promote(tv)
            return(tv)
        ;

        define @unsafe-get(tv : tvar / exh:exh) : any = 
            return(#0(tv));

        define @force-abort(rs : item, startStamp : ![stamp] / exh:exh) : () = 
            let rawStamp : stamp = #0(startStamp)
            cont done(newStamp:stamp) = do #0(startStamp) := newStamp return()
            fun validate(readSet:item, newStamp : stamp, abortItem : item) : () =
                case readSet
                    of Read(tv:tvar, k:cont(any), ws:item, tl:item) =>
                        if I64Eq(#1(tv), 0:long)
                        then if I64Lt(#2(tv), rawStamp)
                             then apply validate(tl, newStamp, abortItem)
                             else apply validate(tl, newStamp, readSet)
                        else if I64Eq(#1(tv), rawStamp)
                             then if I64Lt(#2(tv), rawStamp)
                                  then apply validate(tl, newStamp, abortItem)
                                  else apply validate(tl, newStamp, readSet)
                             else apply validate(tl, newStamp, readSet) 
                    | NilItem => case abortItem
                                of Read(tv:tvar, abortK:cont(any), ws:item, tl:item) => 
				                    let current : any = #0(tv)
				                    let stamp : stamp = #2(tv)
                                    do if I64Eq(#1(tv), 0:long) 
                                       then return()
                                       else let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                                            throw abortK()
                                    do if I64Lt(newStamp, stamp)
                                       then let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                                            throw abortK()
                                       else return()
                                    do FLS.@set-key(READ_SET, abortItem / exh)
                                    do FLS.@set-key(WRITE_SET, ws / exh)
                                    do #0(startStamp) := newStamp
                                    BUMP_PABORT 
                                    throw abortK(current)
                                | NilItem => throw done(newStamp)
                             end
                end             
            let newStamp : stamp = VClock.@bump(/exh)
            do apply validate(rs, newStamp, NilItem)
            do #0(startStamp) := newStamp
            return();

                             

        define @get(tv:tvar / exh:exh) : any = 
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
                    let current : any = 
                        fun getCurrentLoop() : any = 
                            let c : any = #0(tv)
                            let stamp : stamp = #2(tv)
                            if I64Eq(#1(tv), 0:long)
                            then if I64Lt(stamp, #0(myStamp))
                                 then return(c)
                                 else do @force-abort(readSet, myStamp / exh) (*if this returns, it updates myStamp*)
                                      apply getCurrentLoop()
                            else do Pause() apply getCurrentLoop()
                        apply getCurrentLoop()
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
            let vp : vproc = SchedulerAction.@atomic-begin()
            let startStamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
            fun release(locks : item) : () = 
                case locks 
                    of Write(tv:tvar, contents:any, tl:item) =>
                        do #1(tv) := 0:long         (*unlock*)
                        apply release(tl)
                     | NilItem => do SchedulerAction.@atomic-end(vp) return()
                end
            let readSet : item = FLS.@get-key(READ_SET / exh)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            let rawStamp: long = #0(startStamp)
            fun validate(readSet:item, locks:item, newStamp : stamp, abortItem : item) : () =
                case readSet
                    of Read(tv:tvar, k:cont(any), ws:item, tl:item) =>
                        if I64Eq(#1(tv), 0:long)
                        then if I64Lt(#2(tv), rawStamp)
                             then apply validate(tl, locks, newStamp, abortItem)
                             else apply validate(tl, locks, newStamp, readSet)
                        else if I64Eq(#1(tv), rawStamp)
                             then if I64Lt(#2(tv), rawStamp)
                                  then apply validate(tl, locks, newStamp, abortItem)
                                  else apply validate(tl, locks, newStamp, readSet)
                             else apply validate(tl, locks, newStamp, readSet) 
                    | NilItem => case abortItem
                                of Read(tv:tvar, abortK:cont(any), ws:item, tl:item) => 
                                    do apply release(locks)
                                    let current : any = 
                                        fun getCurrentLoop() : any = 
                                            let c : any = #0(tv)
                                            let stamp : stamp = #2(tv)
                                            if I64Eq(#1(tv), 0:long)
                                            then if I64Lt(stamp, #0(startStamp))
                                                 then return(c)
                                                 else do @force-abort(readSet, startStamp / exh) (*if this returns, it updates myStamp*)
                                                      apply getCurrentLoop()
                                            else do Pause() apply getCurrentLoop()
                                        apply getCurrentLoop()
                                    do FLS.@set-key(READ_SET, abortItem / exh)
                                    do FLS.@set-key(WRITE_SET, ws / exh)
                                    do #0(startStamp) := newStamp
                                    BUMP_PABORT 
                                    throw abortK(current)
                                | NilItem => return()
                             end
               end
            fun acquire(ws:item, acquired : item) : item = 
                case ws
                    of Write(tv:tvar, contents:any, tl:item) =>
                        let casRes : long = CAS(&1(tv), 0:long, rawStamp) (*lock it*)
                        if I64Eq(casRes, 0:long)  (*locked for first time*)
                        then apply acquire(tl, Write(tv, contents, acquired))
                        else if I64Eq(casRes, rawStamp)    (*already locked it*)
                             then apply acquire(tl, acquired)
                             else (*release, but don't end atomic*)
                                  fun release(locks : item) : () = 
                                    case locks 
                                        of Write(tv:tvar, contents:any, tl:item) =>
                                            do #1(tv) := 0:long         (*unlock*)
                                            apply release(tl)
                                         | NilItem => return()
                                    end
                                  do apply release(acquired) 
                                  apply acquire(writeSet, NilItem)
                     |NilItem => return(acquired)
                end
            fun update(writes:item, newStamp : stamp) : () = 
                case writes
                    of Write(tv:tvar, newContents:any, tl:item) =>
                        let newContents : any = promote(newContents)
                        do #2(tv) := newStamp            (*update version stamp*)
                        do #0(tv) := newContents         (*update contents*)
                        do #1(tv) := 0:long              (*unlock*)
                        apply update(tl, newStamp)       (*update remaining*)
                     | NilItem => return()
                end
            let locks : item = apply acquire(writeSet, NilItem)
            let newStamp : stamp = VClock.@bump(/exh)
            do apply validate(readSet, locks, newStamp, NilItem)
            do apply update(locks, newStamp)
            do SchedulerAction.@atomic-end(vp)
            return()
        ;

        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#0(in_trans))
            then apply f(UNIT/exh)
            else cont enter() = 
	                 do FLS.@set-key(READ_SET, NilItem / exh)  (*initialize STM log*)
                     do FLS.@set-key(WRITE_SET, NilItem / exh)
                     let newStamp : stamp = VClock.@bump(/exh)
                     let stamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
                     do #0(stamp) := newStamp
                     do #0(in_trans) := true
                     cont abortK() = BUMP_FABORT do #0(in_trans) := false throw enter()
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

      define @timeToString = Time.toString;
      
      define @print-stats(x:unit / exh:exh) : unit = 
        PRINT_PABORT_COUNT
        PRINT_FABORT_COUNT
        PRINT_COMBINED
        return(UNIT);

      define @abort(x : unit / exh : exh) : any = 
         let e : cont() = FLS.@get-key(ABORT_KEY / exh)
         throw e();
         
      define @tvar-eq(arg : [tvar, tvar] / exh : exh) : bool = 
         if Equal(#0(arg), #1(arg))
         then return(true)
         else return(false);       
    )

    	type 'a tvar = _prim(tvar)
    	val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = _prim(@new)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val printStats : unit -> unit = _prim(@print-stats)
    val abort : unit -> 'a = _prim(@abort)
    val unsafeGet : 'a tvar -> 'a = _prim(@unsafe-get)
    val same : 'a tvar * 'b tvar -> bool = _prim(@tvar-eq)
    val zeroCounters : unit -> unit = _prim(@zero-counters)
    val _ = zeroCounters()

    val _ = Ref.set(STMs.stms, ("partial", (get,put,atomic,new,printStats,abort,unsafeGet,same))::Ref.get STMs.stms)

end










 
