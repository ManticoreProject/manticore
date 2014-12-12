(* stm.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Software Transactional Memory with partial aborts and a bounded number of continuations
 * held in the log.
 *)

structure BoundedHybridPartialSTMLowMem = 
struct 

#define COUNT

#ifdef COUNT
#define BUMP_PABORT do ccall M_BumpCounter(0)
#define PRINT_PABORT_COUNT let counter1 : int = ccall M_GetCounter(0) \
                          do ccall M_Print_Int("Partially aborted %d transactions\n", counter1)
#define BUMP_FABORT do ccall M_BumpCounter(1)
#define PRINT_FABORT_COUNT let counter2 : int = ccall M_GetCounter(1) \
                           do ccall M_Print_Int("Fully aborted %d transactions\n", counter2)                     
#define PRINT_COMBINED do ccall M_Print_Int("Aborted %d transactions in total\n", I32Add(counter1, counter2))                                                                                                          
#else
#define BUMP_PABORT
#define PRINT_PABORT_COUNT
#define BUMP_FABORT
#define PRINT_FABORT_COUNT
#define PRINT_COMBINED 
#endif

#define READ_SET_BOUND 19

#define START_TIMER let vp : vproc = host_vproc do ccall GenTimerStart(vp)
#define STOP_TIMER let vp : vproc = host_vproc do ccall GenTimerStop(vp)

    datatype 'a item = Write of 'a * 'a * 'a | NilItem | WithK of 'a * 'a * 'a * 'a * 'a
                     | WithoutK of 'a * 'a

    _primcode(

        extern void * M_Print_Int(void *, int);
        extern void * M_Print_Int2(void *, int, int);
        extern void M_Print_Long (void *, long);
        extern void M_BumpCounter(int);
        extern int M_GetCounter(int);
        extern void M_StartTimer();
        extern void M_StopTimer();
        extern long M_GetTimeAccum();
        extern void GenTimerStart(void *);
        extern void GenTimerStop(void *);
        extern void GenTimerPrint();
        
        typedef stamp = VClock.stamp;
        typedef tvar = ![any, long, stamp]; (*contents, lock, version stamp*)

        typedef readItem = ![tvar,                  (*0: tvar operated on*)
                            (*cont(any)*) any,      (*1: abort continuation (enum(0) if no continuation)*)
                            any,              (*2: write list*)
                            any,                    (*3: next read item*)
                            item,item];                   (*4: next read item with a continuation*)

        typedef skipList = any;

        define @new(x:any / exh:exh) : tvar = 
            let tv : tvar = alloc(x, 0:long, 0:long)
            let tv : tvar = promote(tv)
            return(tv)
        ;

        define @unsafe-get(tv : tvar / exh:exh) : any = 
            return(#0(tv));

        define @get(tv:tvar / exh:exh) : any = 
            let myStamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
            let readSet : [int, item, item] = FLS.@get-key(READ_SET / exh)
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
                    (*must have exclusive access when reading for first time*)
                     (*fun lk() : () = 
                         let swapRes : long = CAS(&1(tv), 0:long, #0(myStamp))
                         if I64Eq(swapRes, 0:long)
                         then return()
                         else do Pause() apply lk()
                     do apply lk()  
                     let current : any = #0(tv)  *)
                     (*
                     let swapRes : long = CAS(&1(tv), 0:long, #0(myStamp))
                     let current : any = 
                        if I64Eq(swapRes, 0:long)
                        then return(#0(tv))
                        else let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                             throw abortK() 
                     do #1(tv) := 0:long*)
                     let current : any = #0(tv)
                     let stamp : stamp = #2(tv)
                     do if I64Eq(#1(tv), 0:long)
                        then return()
                        else let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                             throw abortK()
                     do if I64Lt(#0(myStamp), stamp)
                        then let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                             throw abortK()
                        else return()
                     let sl : item = #1(readSet)
                     if I32Lt(#0(readSet), READ_SET_BOUND)    (*still have room for more*)
                     then let captureCount : int = FLS.@get-counter()
                          if I32Eq(captureCount, 0)  (*capture a continuation*)
                          then let nextCont : item = #2(readSet)
                               let newSL : item = WithK(tv, retK, writeSet, sl, nextCont)
                               let captureFreq : int = FLS.@get-counter2()
                               do FLS.@set-counter(captureFreq)
                               let n : int = I32Add(#0(readSet), 1)  (*update number of conts*)
                               let newRS : [int, item, item] = alloc(n, newSL, newSL)
                               do FLS.@set-key(READ_SET, newRS / exh)
                               return(current)
                          else let n : int = #0(readSet)          (*don't capture cont*)
                               do FLS.@set-counter(I32Sub(captureCount, 1))
                               let nextCont : item = #2(readSet)
                               let newSL : item = WithoutK(tv, sl) 
                               let newRS : [int,item,item] = alloc(n, newSL, nextCont)
                               do FLS.@set-key(READ_SET, newRS / exh)
                               return(current)
                     else fun dropKs(l:item, n:int) : int =   (*drop every other continuation*)
                              case l
                                of NilItem => return(n)
                                 | WithK(_:tvar,_:cont(any),_:List.list,_:item,next:item) =>
                                    case next
                                        of NilItem => return(n)
                                         | WithK(_:tvar,_:cont(any),_:List.list,_:item,nextNext:item) =>
                                            let l : readItem = (readItem) l
                                            let next : readItem = (readItem) next
                                            do #2(next) := enum(0):any
                                            do #5(l) := nextNext
                                            apply dropKs(nextNext, I32Sub(n, 1))
                                    end
                             end
                          let nextCont : item = #2(readSet)
                          let n : int = apply dropKs(nextCont, #0(readSet))
                          let newSL : item = WithoutK(tv, sl)
                          let newRS : [int, item, item] = alloc(n, newSL, nextCont)
                          let captureFreq : int = FLS.@get-counter2()
                          let newFreq : int = I32Mul(captureFreq, 2)
                          do FLS.@set-counter(I32Sub(newFreq, 1))
                          do FLS.@set-counter2(newFreq)
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
            let startStamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
            fun release(locks : item) : () = 
                case locks 
                    of Write(tv:tvar, contents:any, tl:item) =>
                        do #1(tv) := 0:long         (*unlock*)
                        apply release(tl)
                     | NilItem => return()
                end
            let readSet : [int, item, item] = FLS.@get-key(READ_SET / exh)
            let readSet : item = #1(readSet)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            let rawStamp: long = #0(startStamp)
            fun validate(readSet:item, locks:item, newStamp : stamp, abortInfo : item, i:int) : () = 
                case readSet
                    of NilItem => 
                        case abortInfo
                            of NilItem => return() (*no violations detected*)
                             | WithK(tv:tvar,abortK:any,ws:item,_:item,_:item) =>
                                if Equal(abortK, enum(0))
                                then do apply release(locks)
                                     let abortK :cont() = FLS.@get-key(ABORT_KEY / exh)
                                     throw abortK()  (*no checkpoint found*)
                                else do apply release(locks)
                                     let abortK : cont(any) = (cont(any)) abortK
                                     fun lk() : () = 
                                         let swapRes : long = CAS(&1(tv), 0:long, rawStamp)
                                         if I64Eq(swapRes, 0:long)
                                         then return()
                                         else do Pause() apply lk()
                                     do apply lk()
                                     let current : any = #0(tv)
                                     do #1(tv) := 0:long
                                     let newRS : [int,item,item] = alloc(i, abortInfo, abortInfo)
                                     do FLS.@set-key(READ_SET, newRS / exh)
                                     do FLS.@set-key(WRITE_SET, ws / exh)
                                     do #0(startStamp) := newStamp
                                     let captureFreq : int = FLS.@get-counter2()
                                     do FLS.@set-counter(captureFreq)
                                     BUMP_PABORT
                                     throw abortK(current)
                             | WithoutK(tv:tvar,_:item) =>
                                do apply release(locks)
                                let abortK :cont() = FLS.@get-key(ABORT_KEY / exh)
                                throw abortK()  (*no checkpoint found*)
                        end                          
                    | WithK(tv:tvar,k:any,ws:List.list,next:item,nextK:item) => 
                        if I64Lt(#2(tv), rawStamp)
                        then case abortInfo
                               of NilItem => 
                                    if Equal(k, enum(0))            (*continuation was dropped*)
                                    then apply validate(next, locks,newStamp,abortInfo,i)
                                    else apply validate(next, locks,newStamp, abortInfo, I32Add(i, 1))
                                | WithK(_:tvar,k':any,_:List.list,_:item,_:item) =>  (*already going to abort*)
                                    if Equal(k', enum(0))   (*don't have checkpoint*)
                                    then if Equal(k,enum(0))        (*don't have one here either*)
                                         then apply validate(next,locks,newStamp,abortInfo,i) 
                                         else apply validate(next,locks,newStamp,readSet,0) (*use this checkpoint*)
                                    else if Equal(k,enum(0))
                                         then apply validate(next,locks,newStamp,abortInfo,i)
                                         else apply validate(next,locks,newStamp,abortInfo,I32Add(i,1))
                               | _ => if Equal(k,enum(0))
                                      then apply validate(next,locks,newStamp,abortInfo,i)
                                      else apply validate(next,locks,newStamp,readSet, 0)
                             end
                        else apply validate(next,locks,newStamp,readSet,0)
                    | WithoutK(tv:tvar,rest:item) => 
                        if I64Lt(#2(tv), rawStamp)
                        then apply validate(rest, locks,newStamp,abortInfo,i)
                        else apply validate(rest,locks,newStamp,readSet,0)
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
                                  do apply validate(readSet, acquired, newStamp, NilItem,  0)  (*figure out where to abort to*) 
                                  do apply release(acquired)
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
            do apply validate(readSet, locks, newStamp, NilItem, 0)
            do apply update(locks, newStamp)
            return()
        ;

        define inline @force-gc() : () = 
            let vp : vproc = host_vproc
            let limitPtr : any = vpload(LIMIT_PTR, vp)
            do vpstore(ALLOC_PTR, vp, limitPtr)
            return()   
        ;
        
        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#0(in_trans))
            then apply f(UNIT/exh)
            else cont enter() = 
                     do FLS.@set-counter(0)
                     do FLS.@set-key(READ_SET, alloc(0, NilItem, NilItem) / exh)  (*initialize STM log*)
                     do FLS.@set-key(WRITE_SET, NilItem / exh)
                     let stamp : stamp = VClock.@bump(/exh)
                     let stampPtr : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
                     do #0(stampPtr) := stamp
                     do #0(in_trans) := true
                     cont abortK() = BUMP_FABORT do #0(in_trans) := false throw enter()
                     do FLS.@set-key(ABORT_KEY, abortK / exh)
                     cont transExh(e:exn) = 
                        do ccall M_Print("Warning: exception raised in transaction\n")
                        do @commit(/exh)  (*exception may have been raised because of inconsistent state*)
                        throw exh(e)
                     let res : any = apply f(UNIT/transExh)
                     do @commit(/transExh)
                     do #0(in_trans) := false
                     do FLS.@set-key(READ_SET, nil / exh)
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
    )

    	type 'a tvar = 'a PartialSTM.tvar
    	val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = _prim(@new)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val printStats : unit -> unit = _prim(@print-stats)
    val abort : unit -> 'a = _prim(@abort)
    val unsafeGet : 'a tvar -> 'a = _prim(@unsafe-get)
end












 
