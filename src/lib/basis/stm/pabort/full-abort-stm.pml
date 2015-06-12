(* stm.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Software Transactional Memory with partial aborts.
 *)

structure FullAbortSTM = (* :
    sig
	
*)
struct

    (*flat representation for read and write sets*)
    datatype 'a item = Read of 'a * 'a | Write of 'a * 'a * 'a | NilItem

    _primcode(

        extern void * M_Print_Int(void *, int);
        extern void * M_Print_Int2(void *, int, int);
        extern void M_Print_Long (void *, long);
        
        typedef itemType = int; (*correponds to the above #define's*)
        typedef stamp = VClock.stamp;
        typedef tvar = ![any, long, stamp]; (*contents, lock, version stamp*)

        typedef readItem = [tvar]; 

        typedef writeItem = [tvar,    (*0: tvar operated on*)
                             any];    (*1: contents of local copy*)

        define @new(x:any / exh:exh) : tvar = 
            let tv : tvar = alloc(x, 0:long, 0:long)
            let tv : tvar = promote(tv)
            return(tv)
        ;

        define @unsafe-get(tv : tvar / exh:exh) : any = 
            return(#0(tv));

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
                    let current : any = #0(tv)
                    let stamp : stamp = #1(tv)
                    do if I64Eq(#1(tv), 0:long)
                       then return()
                       else fun lp() : () = 
                                if I64Eq(#1(tv), 0:long)
                                then return()
                                else do Pause() apply lp()
                            do apply lp()
                            let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                            throw abortK()
                    do if I64Lt(#0(myStamp), stamp)
                       then let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                            throw abortK()
                       else return()
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
            let vp : vproc = SchedulerAction.@atomic-begin()
            let startStamp : ![stamp, int] = FLS.@get-key(STAMP_KEY / exh)
            fun release(locks : item) : () = 
                case locks 
                    of Write(tv:tvar, contents:any, tl:item) =>
                        do #1(tv) := 0:long         (*unlock*)
                        apply release(tl)
                     | NilItem => do SchedulerAction.@atomic-end(vp)
                                  return()
                end
            let readSet : item = FLS.@get-key(READ_SET / exh)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            let rawStamp: long = #0(startStamp)
            fun validate(readSet : item, locks : item, newStamp : stamp) : () = 
                case readSet 
                    of Read(tv:tvar, tl:item) =>
                        if I64Lt(#2(tv), rawStamp)  (*still valid*)
                        then apply validate(tl, locks, newStamp)
                        else do apply release(locks)
                             let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                             throw abortK()     
                     |NilItem => return()
                end
            fun acquire(writeSet:item, acquired : item) : item = 
                case writeSet
                    of Write(tv:tvar, contents:any, tl:item) =>
                        let casRes : long = CAS(&1(tv), 0:long, rawStamp) (*lock it*)
                        if I64Eq(casRes, 0:long)  (*locked for first time*)
                        then apply acquire(tl, Write(tv, contents, acquired))
                        else if I64Eq(casRes, rawStamp)    (*already locked it*)
                             then apply acquire(tl, acquired)
                             else do apply release(acquired)    
                                  fun lp() : () = 
                                    if I64Eq(#1(tv), 0:long)
                                    then return()
                                    else do Pause() apply lp()
                                  do apply lp()
                                  let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                                  throw abortK()
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
            let plusOne : stamp = I64Add(rawStamp, 1:long)
            do apply validate(readSet, locks, newStamp)
            do apply update(locks, newStamp)
            do SchedulerAction.@atomic-end(vp)
            return()
        ;

        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
                let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
                if (#0(in_trans))
                then apply f(UNIT/exh)
                else let stampPtr : ![stamp, int] = FLS.@get-key(STAMP_KEY / exh)
                     do #1(stampPtr) := 0
                     cont enter() = 
                         do FLS.@set-key(READ_SET, NilItem / exh)  (*initialize STM log*)
                         do FLS.@set-key(WRITE_SET, NilItem / exh)
                         let stamp : stamp = VClock.@bump(/exh)
                         do #0(stampPtr) := stamp
                         do #0(in_trans) := true
                         cont abortK() = do #1(stampPtr) := I32Add(#1(stampPtr), 1) BUMP_FABORT do #0(in_trans) := false do #1(stampPtr) := I32Add(#1(stampPtr), 1) throw enter()      
                         do FLS.@set-key(ABORT_KEY, (any) abortK / exh)
                         cont transExh(e:exn) = 
                            do @commit(/transExh)  (*exception may have been raised because of inconsistent state*)
                            throw exh(e)
                         let res : any = apply f(UNIT/transExh)
                         do @commit(/transExh)
                       (*  do ccall M_Print_Int("Aborted this transaction %d times\n", #1(stampPtr)) *)
                         do #0(in_trans) := false
                         do FLS.@set-key(READ_SET, NilItem / exh)
                         do FLS.@set-key(WRITE_SET, NilItem / exh)
                         return(res)
                     throw enter()
        ;

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

    	type 'a tvar = 'a PartialSTM.tvar 
    	val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = _prim(@new)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val printStats : unit -> unit = _prim(@print-stats)
    val abort : unit -> 'a = _prim(@abort)
    val unsafeGet : 'a tvar -> 'a = _prim(@unsafe-get)
    val same : 'a tvar * 'b tvar -> bool = _prim(@tvar-eq)

    val _ = Ref.set(STMs.stms, ("full", (get,put,atomic,new,printStats,abort,unsafeGet,same))::Ref.get STMs.stms)
end













 
