(* stm.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Software Transactional Memory with partial aborts.
 *)

structure BoundedHybridPartialSTM = 
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
#define BUMP_ABORT do ccall M_BumpCounter(0)
#define PRINT_ABORT_COUNT let counter : int = ccall M_GetCounter(0) \
                          do ccall M_Print_Int("Aborted %d transactions\n", counter)
#else
#define BUMP_ABORT
#define PRINT_ABORT_COUNT
#endif

#define READ_SET_BOUND 100

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

        typedef readItem = ![tvar,                  (*0: tvar operated on*)
                            (*cont(any)*) any,      (*1: abort continuation (enum(0) if no continuation)*)
                            List.list,              (*2: write list*)
                            any,                    (*3: next read item*)
                            any];                   (*4: next read item with a continuation*)

        typedef writeItem = [tvar,    (*0: tvar operated on*)
                             any];    (*1: contents of local copy*)

        typedef skipList = any;

        define @new(x:any / exh:exh) : tvar = 
            let tv : tvar = alloc(x, 0:long, 0:long)
            let tv : tvar = promote(tv)
            return(tv)
        ;

        define @get-aux(tv:tvar, retK : cont(any) / exh:exh) : any = 
            let myStamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
            let readSet : [int, skipList, skipList] = FLS.@get-key(READ_SET / exh)
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
                of Option.SOME(v:any) => throw retK(v)
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
                     let sl : skipList = #1(readSet)
                     do if I32Lt(#0(readSet), READ_SET_BOUND)    (*still have room for more*)
                        then let captureCount : int = FLS.@get-counter()
                             if I32Eq(captureCount, 0)  (*capture a continuation*)
                             then let nextCont : skipList = #2(readSet)
                                  let newSL : skipList = (any) alloc(tv, (any) retK, writeSet, sl, nextCont)
                                  let captureFreq : int = FLS.@get-counter2()
                                  do FLS.@set-counter(captureFreq)
                                  let n : int = I32Add(#0(readSet), 1)  (*update number of conts*)
                                  let newRS : [int, skipList, skipList] = alloc(n, newSL, newSL)
                                  FLS.@set-key(READ_SET, newRS / exh)
                             else let n : int = #0(readSet)          (*don't capture cont*)
                                  let nextCont : skipList = #2(readSet)
                                  let newSL : skipList = (skipList) alloc(tv, enum(0):any, nil, sl, nextCont)
                                  let newRS : [int,skipList,skipList] = alloc(n, newSL, nextCont)
                                  FLS.@set-key(READ_SET, newRS / exh)
                        else fun dropKs(l:skipList, n:int) : int =   (*drop every other continuation*)
                                if Equal(l, nil)
                                then return(n)
                                else let l : readItem = (readItem) l
                                     let next : skipList = #4(l)
                                     if Equal(next, nil)
                                     then return(n)
                                     else let next : readItem = (readItem) next
                                          let nextNext : skipList = #4(next)
                                          do #1(next) := enum(0):any (*null out continuation*)
                                          do #4(l) := nextNext
                                          apply dropKs(nextNext, I32Sub(n, 1))
                             let n : int = apply dropKs(sl, #0(readSet))
                             let nextCont : skipList = #2(readSet)
                             let newSL : skipList = (skipList) alloc(tv, enum(0):any, writeSet, sl, nextCont)
                             let newRS : [int, skipList, skipList] = alloc(n, newSL, nextCont)
                             let captureFreq : int = FLS.@get-counter2()
                             let newFreq : int = I32Mul(captureFreq, 2)
                             do FLS.@set-counter(I32Sub(newFreq, 1))
                             do FLS.@set-counter2(newFreq)
                             FLS.@set-key(READ_SET, newRS / exh)
                     throw retK(current)
            end
        ;

        define @get(tv:tvar / exh:exh) : any = 
            cont retK(x:any) = return(x)
            @get-aux(tv, retK / exh);

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
            let readSet : [int, skipList, skipList] = FLS.@get-key(READ_SET / exh)
            let readSet : skipList = #1(readSet)
            let writeSet : List.list = FLS.@get-key(WRITE_SET / exh)
            let rawStamp: long = #0(startStamp)
            fun validate(readSet:skipList, locks:List.list, newStamp : stamp, abortInfo : any, i:int) : () = 
                if Equal(readSet, nil)
                then if Equal(abortInfo, enum(1))
                     then return() (*no violations detected*)
                     else let abortInfo : readItem = (readItem) abortInfo
                          if Equal(#1(abortInfo), enum(0))  (*no abort continuation, restart...*)
                          then do apply release(locks)
                               let e : exn = Fail(@"__ABORT_EXCEPTION__") (*no checkpoint info*)
                               throw exh(e)
                          else do apply release(locks)  
                               let abortInfo : readItem = (readItem) abortInfo
                               let abortK : cont(any) = (cont(any)) #1(abortInfo)
                               let tv : tvar = (tvar) #0(abortInfo)
                               let newRS : [int,skipList,skipList] = alloc(i, #3(abortInfo), #4(abortInfo))
                               do FLS.@set-key(READ_SET, newRS / exh)
                               do FLS.@set-key(WRITE_SET, #2(abortInfo) / exh)
                               do FLS.@set-key(STAMP_KEY, alloc(newStamp) / exh)
                               let captureFreq : int = FLS.@get-counter2()
                               do FLS.@set-counter(captureFreq)
                               BUMP_ABORT
                               let _ = @get-aux(tv, abortK / exh)
                               do ccall M_Print("Impossible\n")
                               return()
                else let readSet : readItem = (readItem) readSet
                     let tl : skipList = #3(readSet)
                     let tv : tvar = #0(readSet)
                        if I64Lt(#2(tv), rawStamp)  (*still valid*)
                        then if Equal(abortInfo, enum(1))           (*don't need chkpoint info*)
                             then if Equal(#1(readSet), enum(0))    (*no checkpoint here*)
                                  then apply validate(tl, locks, newStamp, abortInfo, i)
                                  else apply validate(tl, locks, newStamp, abortInfo, I32Add(i, 1))
                             else if Equal(#1(readSet), enum(0))  
                                  then apply validate(tl, locks, newStamp, abortInfo, i)
                                  else apply validate(tl, locks, newStamp, readSet, 0)
                        else apply validate(tl, locks, newStamp, readSet, 0)  
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
                                  do apply validate(readSet, acquired, newStamp, enum(1),  0)  (*figure out where to abort to*)
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
            do apply validate(readSet, locks, newStamp, enum(1), 0)
            do apply update(locks, newStamp)
            return()
        ;

        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            cont enter() = 
                let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
                if (#0(in_trans))
                then do ccall M_Print ("WARNING: entering nested transaction\n") apply f(UNIT/exh)
                else do FLS.@set-key(READ_SET, alloc(0, nil, nil) / exh)  (*initialize STM log*)
                     do FLS.@set-key(WRITE_SET, nil / exh)
                     let stamp : stamp = VClock.@bump(/exh)
                     let stamp : [stamp] = alloc(stamp)
                     do FLS.@set-key(STAMP_KEY, stamp / exh)
                     do #0(in_trans) := true
                     cont abortK(e:exn) = 
                        case e  (*Check that the exception received was because of an aborted TX*)
                            of Fail(s:ml_string) => 
                                 let arg : [ml_string, ml_string] = alloc(@"__ABORT_EXCEPTION__", s)
                                 let res : bool = String.@same(arg / exh)
                                 if(res)
                                 then BUMP_ABORT 
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
        PRINT_ABORT_COUNT
        return(UNIT);
    )

    	type 'a tvar = _prim(tvar)
    	val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = _prim(@new)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val printStats : unit -> unit = _prim(@print-stats)
end












 
