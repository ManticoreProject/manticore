(* stm.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Software Transactional Memory with partial aborts.
 *)

structure HybridPartialSTM = 
struct 

#ifndef NDEBUG
#define PDebug(msg)  do ccall M_Print(msg)  
#define PDebugInt(msg, v)  do ccall M_Print_Int(msg, v)  
#define PDebugInt2(msg, v1, v2)  do ccall M_Print_Int2(msg, v1, v2)  
#define PDebugLong(msg, v) do ccall M_Print_Long(msg, v)
#else
#define PDebug(msg) 
#define PDebugInt(msg, v)   
#define PDebugInt2(msg, v1, v2) 
#define PDebugLong(msg, v) 
#endif

#define READ_THRESH 512

    datatype 'a item = Read of 'a * 'a * 'a * 'a | Write of 'a * 'a * 'a | NilItem

    _primcode(

        extern void * M_Print_Int(void *, int);
        extern void * M_Print_Int2(void *, int, int);
        extern void M_Print_Long (void *, long);
        extern void M_StartTimer();
        extern void M_StopTimer();
        extern long M_GetTimeAccum();
        
        typedef stamp = VClock.stamp;
        typedef tvar = ![any, long, stamp]; (*contents, lock, version stamp*)

        typedef readItem = [tvar,                   (*0: tvar operated on*)
                            (*cont(any)*) any,      (*1: abort continuation (enum(0) if no continuation)*)
                            List.list];             (*2: write list*)

        typedef writeItem = [tvar,    (*0: tvar operated on*)
                             any];    (*1: contents of local copy*)

        define @new(x:any / exh:exh) : tvar = 
            let tv : tvar = alloc(x, 0:long, 0:long)
            let tv : tvar = promote(tv)
            return(tv)
        ;

        define @get(tv:tvar / exh:exh) : any = 
            let myStamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
            let readSet : item = FLS.@get-key(READ_SET / exh)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            cont retK(x:any) = return(x)
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
                    (*must have exclusive access when reading for first time*)
                     fun lk() : () = 
                         let swapRes : long = CAS(&1(tv), 0:long, #0(myStamp))
                         if I64Eq(swapRes, 0:long)
                         then return()
                         else do Pause() apply lk()
                     do apply lk()
                     let current : any = #0(tv)
                     do #1(tv) := 0:long
                     let numReads : int = FLS.@get-counter()
                     let newReadSet : item = 
                        if I32Lt(numReads, READ_THRESH) 
                        then do FLS.@set-counter(I32Add(numReads, 1))
                             return(Read(tv, enum(0):any, NilItem, readSet))
                        else do FLS.@set-counter(0)
                             return(Read(tv, (any) retK, writeSet, readSet))
                     do FLS.@set-key(READ_SET, newReadSet / exh)
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
                        do #1(tv) := 0:long         (*unlock*)
                        apply release(tl)
                     | NilItem => return()
                end
            let readSet : item = FLS.@get-key(READ_SET / exh)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            let rawStamp: long = #0(startStamp)
            fun validate(readSet:item, locks:item, newStamp : stamp, abortInfo : item) : () = 
                case readSet
                    of Read(tv:tvar, abortK:any, ws:item, tl:item) =>
                        if I64Lt(#2(tv), rawStamp)  (*still valid*)
                        then if Equal(abortK, enum(0))    (*no checkpoint here*)
                             then apply validate(tl, locks, newStamp, abortInfo)
                             else case abortInfo
                                    of NilItem => apply validate(tl, locks, newStamp, abortInfo)
                                     | Read(tv:tvar,abortK:any,ws:item,_:item) =>
                                        if Equal(abortK, enum(0))
                                        then apply validate(tl, locks, newStamp, readSet)  (*use this continuation*)
                                        else apply validate(tl, locks, newStamp, abortInfo)
                                 end
                        else apply validate(tl, locks, newStamp, readSet)  (*read is out of date*)
                    | NilItem => 
                        case abortInfo
                            of NilItem => return() (*no violations detected*)
                             | Read(tv:tvar,abortK:any,ws:item,tl:item) => 
                                 if Equal(abortK, enum(0)) 
                                 then do apply release(locks)
                                      let abortK : cont() = FLS.@get-key(ABORT_KEY / exh) (*no checkpoint info*)
                                      throw abortK()
                                 else do apply release(locks)
                                      fun lk() : () = 
                                        let old : long = CAS(&1(tv), 0:long, newStamp)
                                        if I64Eq(old, 0:long)
                                        then let current : any = #0(tv)
                                             do #1(tv) := 0:long
                                             do FLS.@set-key(READ_SET, abortInfo / exh)
                                             do FLS.@set-key(WRITE_SET, ws / exh)
                                             do #0(startStamp) := newStamp
                                             BUMP_PABORT 
                                             let abortK : cont(any) = (cont(any)) abortK
                                             throw abortK(current)
                                        else do Pause() apply lk()
                                      apply lk()
                      end
                end        
            fun acquire(writeSet:item, acquired : item) : item = 
                case writeSet
                    of Write(tv:tvar, contents:any, tl:item) =>
                        let casRes : long = CAS(&1(tv), 0:long, rawStamp) (*lock it*)
                        if I64Eq(casRes, 0:long)  (*locked for first time*)
                        then apply acquire(tl, Write(tv, contents, acquired))
                        else if I64Eq(casRes, rawStamp)    (*already locked it*)
                             then apply acquire(tl, acquired)
                             else let newStamp : stamp = VClock.@bump(/exh)
                                  do apply validate(readSet, acquired, newStamp, NilItem)  (*figure out where to abort to*)
                                  apply acquire(writeSet, acquired)
                     |NilItem => return(acquired)
                end
            fun update(writes:item, newStamp : stamp) : () = 
                case writes
                    of Write(tv:tvar, newContents:any, tl:item) =>
                        let newContents : any = promote(newContents)
                        do #2(tv) := newStamp            (*update version stamp*)
                        do #0(tv) := newContents         (*update contents*)
                        do #1(tv) := 0:long              (*unlock*)
                        apply update(tl, newStamp)      
                     | NilItem => return()
                end
            let locks : item = apply acquire(writeSet, NilItem)
            let newStamp : stamp = VClock.@bump(/exh)
            do apply validate(readSet, locks, newStamp, NilItem)
            do apply update(locks, newStamp)
            return()
        ;

        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            cont enter() = 
                let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
                if (#0(in_trans))
                then do ccall M_Print ("WARNING: entering nested transaction\n") apply f(UNIT/exh)
                else do FLS.@set-key(READ_SET, NilItem / exh)  (*initialize STM log*)
                     do FLS.@set-key(WRITE_SET, NilItem / exh)
                     let stamp : stamp = VClock.@bump(/exh)
                     let stamp : [stamp] = alloc(stamp)
                     let stamp : [stamp] = promote(stamp)
                     do FLS.@set-key(STAMP_KEY, stamp / exh)
                     do #0(in_trans) := true           
                     cont abortK() = BUMP_FABORT do #0(in_trans) := false throw enter()
                     do FLS.@set-key(ABORT_KEY, abortK / exh)
                     cont transExh(e:exn) = 
                        do @commit(/exh)  (*exception may have been raised because of inconsistent state*)
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
    )

    	type 'a tvar = _prim(tvar)
    	val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = _prim(@new)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val printStats : unit -> unit = _prim(@print-stats)
end












 
