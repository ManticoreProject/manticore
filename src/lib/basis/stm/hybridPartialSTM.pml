(* stm.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Software Transactional Memory with partial aborts.
 *)

#define Read 0
#define Write 1

structure HybridPartialSTM = (* :
    sig
	
*)
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

#define COUNT

#ifdef COUNT
#define BUMP_PABORT do ccall M_BumpCounter(0)
#define PRINT_PABORT_COUNT let counter : int = ccall M_GetCounter(0) \
                          do ccall M_Print_Int("Partially aborted %d transactions\n", counter)
#define BUMP_FABORT do ccall M_BumpCounter(1)
#define PRINT_FABORT_COUNT let counter : int = ccall M_GetCounter(1) \
                           do ccall M_Print_Int("Fully aborted %d transactions\n", counter)                          
#else
#define BUMP_PABORT
#define PRINT_PABORT_COUNT
#define BUMP_FABORT
#define PRINT_FABORT_COUNT
#endif

#define CHKPT 0
#define NOCHKPT 1

#define READ_THRESH 512

    _primcode(

        extern void * M_Print_Int(void *, int);
        extern void * M_Print_Int2(void *, int, int);
        extern void M_Print_Long (void *, long);
        extern void M_BumpCounter(int);
        extern int M_GetCounter(int);
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

        define @nonCPRead(tv:tvar / exh:exh) : any = 
            let myStamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
            let readSet : List.list = FLS.@get-key(READ_SET / exh)
            let writeSet : List.list = FLS.@get-key(WRITE_SET / exh)
            fun chkLog(writeSet : List.list) : Option.option = (*use local copy if available*)
                 case writeSet
                     of CONS(hd:writeItem, tl:List.list) =>
                         if Equal(#0(hd), tv)
                         then return(Option.SOME(#1(hd)))
                         else apply chkLog(tl)
                     | nil => return (Option.NONE)
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
                     let item : readItem = alloc(tv, enum(0):any, nil)
                     let newReadSet : List.list = CONS(item, readSet)
                     do FLS.@set-key(READ_SET, newReadSet / exh)
                     return(current)
            end
        ;

        define @get(tv:tvar / exh:exh) : any = 
            let myStamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
            let readSet : List.list = FLS.@get-key(READ_SET / exh)
            let writeSet : List.list = FLS.@get-key(WRITE_SET / exh)
            cont retK(x:any) = return(x)
            fun chkLog(writeSet : List.list) : Option.option = (*use local copy if available*)
                 case writeSet
                     of CONS(hd:writeItem, tl:List.list) =>
                         if Equal(#0(hd), tv)
                         then return(Option.SOME(#1(hd)))
                         else apply chkLog(tl)
                     | nil => return (Option.NONE)
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
                     let item : readItem = 
                        if I32Lt(numReads, READ_THRESH) 
                        then do FLS.@set-counter(I32Add(numReads, 1))
                             return(alloc(tv, enum(0):any , writeSet))
                        else do FLS.@set-counter(0)
                             return(alloc(tv, (any) retK, nil))
                     let newReadSet : List.list = CONS(item, readSet)
                     do FLS.@set-key(READ_SET, newReadSet / exh)
                     return(current)
            end
        ;

        define @put(arg:[tvar, any] / exh:exh) : unit =
            let tv : tvar = #0(arg)
            let v : any = #1(arg)
            let item : writeItem = alloc(tv, v)
            let writeSet : List.list = FLS.@get-key(WRITE_SET / exh)
            let newWriteSet : List.list = CONS(item, writeSet)
            do FLS.@set-key(WRITE_SET, newWriteSet / exh)
            return(UNIT)
        ;

        define @commit(/exh:exh) : () = 
            let startStamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
            fun release(locks : List.list) : () = 
                case locks 
                    of CONS(hd:writeItem, tl:List.list) =>
                        let tv:tvar = #0(hd)
                        do #1(tv) := 0:long         (*unlock*)
                        apply release(tl)
                     | nil => return()
                end
            let readSet : List.list = FLS.@get-key(READ_SET / exh)
            let writeSet : List.list = FLS.@get-key(WRITE_SET / exh)
            let rawStamp: long = #0(startStamp)
            fun validate(readSet:List.list, locks:List.list, newStamp : stamp, abortInfo : [any,any,any], newRS : List.list) : () = 
                case readSet
                    of CONS(hd:readItem, tl:List.list) =>
                        let tv : tvar = #0(hd)
                        if I64Lt(#2(tv), rawStamp)  (*still valid*)
                        then if Equal(#1(abortInfo), enum(0))           (*need chkpoint info*)
                             then if Equal(#1(hd), enum(0))         (*dont' have chkpoint info*)
                                  then apply validate(tl, locks, newStamp, abortInfo, newRS)
                                  else apply validate(tl, locks, newStamp, hd, tl)  (*this is the nearest valid read with chkpoint info*)
                             else apply validate(tl, locks, newStamp, abortInfo, newRS)
                        else apply validate(tl, locks, newStamp, hd, tl)
                    | nil => 
                        if Equal(#1(abortInfo), enum(1))
                        then return() (*no violations detected*)
                        else if Equal(#1(abortInfo), enum(0))  (*no abort continuation, restart...*)
                             then do apply release(locks)
                                  let e : exn = Fail(@"__ABORT_EXCEPTION__") (*no checkpoint info*)
                                  throw exh(e)
                             else do apply release(locks)  
                                  let tv : tvar = (tvar) #0(abortInfo)
                                  fun lk() : () = 
                                    let old : long = CAS(&1(tv), 0:long, newStamp)
                                    if I64Eq(old, 0:long)
                                    then let current : any = #0(tv)
                                         do #1(tv) := 0:long
                                         let newRS : List.list = CONS(abortInfo, newRS)
                                         do FLS.@set-key(READ_SET, newRS / exh)
                                         do FLS.@set-key(WRITE_SET, #2(abortInfo) / exh)
                                         do #0(startStamp) := newStamp
                                         BUMP_PABORT 
                                         let abortK : cont(any) = (cont(any)) #1(abortInfo)
                                         throw abortK(current)
                                    else do Pause() apply lk()
                                  apply lk()
                end        
            fun acquire(writeSet:List.list, acquired : List.list) : List.list = 
                case writeSet
                    of CONS(hd:writeItem, tl:List.list) =>
                        let tv : tvar = #0(hd)
                        let casRes : long = CAS(&1(tv), 0:long, rawStamp) (*lock it*)
                        if I64Eq(casRes, 0:long)  (*locked for first time*)
                        then apply acquire(tl, CONS(hd, acquired))
                        else if I64Eq(casRes, rawStamp)    (*already locked it*)
                             then apply acquire(tl, acquired)
                             else let newStamp : stamp = VClock.@bump(/exh)
                                  do apply validate(readSet, acquired, newStamp, alloc(enum(1),enum(1),enum(1)), nil)  (*figure out where to abort to*)
                                  apply acquire(writeSet, acquired)
                     |nil => return(acquired)
                end
            fun update(writes:List.list, newStamp : stamp) : () = 
                case writes
                    of CONS(hd:writeItem, tl:List.list) =>
                        let tv : tvar = #0(hd)           (*pull out the tvar*)
                        let newContents : any = #1(hd)   (*get the local contents*)
                        let newContents : any = promote(newContents)
                        do #2(tv) := newStamp            (*update version stamp*)
                        do #0(tv) := newContents         (*update contents*)
                        do #1(tv) := 0:long              (*unlock*)
                        apply update(tl, newStamp)       (*update remaining*)
                     | nil => return()
                end
            let locks : List.list = apply acquire(writeSet, nil)
            let newStamp : stamp = VClock.@bump(/exh)
            do apply validate(readSet, locks, newStamp, alloc(enum(1),enum(1),enum(1)), nil)
            do apply update(locks, newStamp)
            return()
        ;

        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            cont enter() = 
                let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
                if (#0(in_trans))
                then do ccall M_Print ("WARNING: entering nested transaction\n") apply f(UNIT/exh)
                else do FLS.@set-key(READ_SET, nil / exh)  (*initialize STM log*)
                     do FLS.@set-key(WRITE_SET, nil / exh)
                     let stamp : stamp = VClock.@bump(/exh)
                     let stamp : [stamp] = alloc(stamp)
                     let stamp : [stamp] = promote(stamp)
                     do FLS.@set-key(STAMP_KEY, stamp / exh)
                     do #0(in_trans) := true           
                     cont abortK(e:exn) = 
                        case e  (*Check that the exception received was because of an aborted TX*)
                            of Fail(s:ml_string) => 
                                 let arg : [ml_string, ml_string] = alloc(@"__ABORT_EXCEPTION__", s)
                                 let res : bool = String.@same(arg / exh)
                                 if(res) 
                                 then BUMP_FABORT 
                                      do #0(in_trans) := false 
                                      throw enter()
                                 else throw exh(e)
                             | _ => throw exh(e)
                        end
                     let res : any = apply f(UNIT/abortK)
                     do @commit(/abortK)
                     do #0(in_trans) := false
                     do FLS.@set-key(READ_SET, nil / exh)
                     do FLS.@set-key(WRITE_SET, nil / exh)
                     return(res)  
            throw enter()    
        ;

      define @timeToString = Time.toString;
      
      define @print-stats(x:unit / exh:exh) : unit = 
        PRINT_PABORT_COUNT
        PRINT_FABORT_COUNT
        return(UNIT);
    )

    	type 'a tvar = _prim(tvar)
    	val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val nonCPGet : 'a tvar -> 'a = _prim(@nonCPRead)
    val new : 'a -> 'a tvar = _prim(@new)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val printStats : unit -> unit = _prim(@print-stats)
end












 
