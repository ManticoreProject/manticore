(* stm.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Software Transactional Memory with partial aborts and a bounded number of continuations
 * held in the log and fast forwarding.
 *)

structure OrderedSTM = 
struct 

#define COUNT 

#ifdef COUNT
#define BUMP_PABORT do ccall M_BumpCounter(0)
#define PRINT_PABORT_COUNT let counter1 : int = ccall M_SumCounter(0) \
                           do ccall M_Print_Int("Partial-Aborts = %d\n", counter1)
#define BUMP_FABORT do ccall M_BumpCounter(1)
#define PRINT_FABORT_COUNT let counter2 : int = ccall M_SumCounter(1) \
                           do ccall M_Print_Int("Full-Aborts = %d\n", counter2)                     
#define PRINT_COMBINED do ccall M_Print_Int("Total-Aborts = %d\n", I32Add(counter1, counter2))     
#define BUMP_KCOUNT do ccall M_BumpCounter(2)
#define PRINT_KCOUNT let counter1 : int = ccall M_SumCounter(2) \
                     do ccall M_Print_Int("Fast Forward Continuation Hits = %d\n", counter1)                                                                                                 
#else
#define BUMP_PABORT
#define PRINT_PABORT_COUNT
#define BUMP_FABORT
#define PRINT_FABORT_COUNT
#define PRINT_COMBINED 
#define BUMP_KCOUNT
#define PRINT_KCOUNT 
#endif

#define START_TIMER let vp : vproc = host_vproc do ccall GenTimerStart(vp)
#define STOP_TIMER let vp : vproc = host_vproc do ccall GenTimerStop(vp)

(*TODO:
    -add a 64-bit bloom filter to FLS that can be used for determining if a given
     tref is on the short path of the fast-forward read set
    -64 bits is sufficient given that we will never have more than 20 item on the short path
*)

#define READ_SET_BOUND 20

    structure RS = ReadSet

    

    _primcode(

        extern void * M_Print_Int(void *, int);
        extern void * M_Print_Int2(void *, int, int);
        extern void M_Print_Long (void *, long);
        extern void M_BumpCounter(int);
        extern int M_SumCounter(int);
        extern long M_GetTimeAccum();
        extern int M_PolyEq(void * , void *);

        typedef stamp = VClock.stamp;
        typedef tvar = ![any, long, stamp]; (*contents, lock, version stamp*)
	
        typedef readItem = ![tvar,                  (*0: tvar operated on*)
                            (*cont(any)*) any,      (*1: abort continuation (enum(0) if no continuation)*)
                            any,                    (*2: write list*)
                            any,                    (*3: next read item*)
                            RS.item,RS.item];             (*4: next read item with a continuation*)
        
        define @new(x:any / exh:exh) : tvar = 
            let tv : tvar = alloc(x, 0:long, 0:long)
            let tv : tvar = promote(tv)
            return(tv)
        ;

        define @unsafe-get(tv : tvar / exh:exh) : any = 
            return(#0(tv));

        define @getABCDEFG(tv:tvar / exh:exh) : any = 
            let in_trans : [bool] = FLS.@get-key(IN_TRANS / exh)
            do if(#0(in_trans))
               then return()
               else do ccall M_Print("Trying to read outside a transaction!\n")
                    let e : exn = Fail(@"Reading outside transaction\n")
                    throw exh(e)
            let myStamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
            let readSet : RS.read_set = FLS.@get-key(READ_SET / exh)
            let writeSet : RS.item = FLS.@get-key(WRITE_SET / exh)
            fun chkLog(writeSet : RS.item) : Option.option = (*use local copy if available*)
                 case writeSet
                     of RS.Write(tv':tvar, contents:any, tl:RS.item) =>
                         if Equal(tv', tv)
                         then return(Option.SOME(contents))
                         else apply chkLog(tl)
                     | RS.NilItem => return (Option.NONE)
                 end
            cont retK(x:any) = return(x)
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
                                 else 
                                    let newStamp : stamp = VClock.@bump(/exh)
                                    do RS.@validate(readSet, myStamp, newStamp/ exh)
                                    do #0(myStamp) := newStamp
                                    apply getCurrentLoop()
                            else do Pause() apply getCurrentLoop()
                        apply getCurrentLoop()
                     let numK : int = RS.@getNumK(readSet)
                     if I32Lt(numK, READ_SET_BOUND)
                     then
                        let captureCount : int = FLS.@get-counter()
                        if I32Eq(captureCount, 0)  (*capture a continuation*)
                        then 
                            let newRS : RS.read_set = RS.@insert-with-k(tv, retK, writeSet, readSet / exh)
                            do FLS.@set-key(READ_SET, newRS / exh)
                            let freq : int = FLS.@get-counter2()
                            do FLS.@set-counter(freq)
                            return(current)
                        else
                            do FLS.@set-counter(I32Sub(captureCount, 1))
                            let newRS : RS.read_set = RS.@insert-without-k(tv, readSet / exh)
                            do FLS.@set-key(READ_SET, newRS / exh)
                            return(current)
                     else 
                        do RS.@filterRS(readSet)
                        let captureFreq : int = FLS.@get-counter2()
                        let newFreq : int = I32Mul(captureFreq, 2)
                        do FLS.@set-counter(I32Sub(newFreq, 1))
                        do FLS.@set-counter2(newFreq)
                        let newRS : RS.read_set = RS.@insert-without-k(tv, readSet / exh)
                        do FLS.@set-key(READ_SET, newRS / exh)
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
            let writeSet : RS.item = FLS.@get-key(WRITE_SET / exh)
            let newWriteSet : RS.item = RS.Write(tv, v, writeSet)
            do FLS.@set-key(WRITE_SET, newWriteSet / exh)
            return(UNIT)
        ;

        define @commit(/exh:exh) : () = 
            let startStamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
            fun release(locks : RS.item) : () = 
                case locks 
                    of RS.Write(tv:tvar, contents:any, tl:RS.item) =>
                        do #1(tv) := 0:long         (*unlock*)
                        apply release(tl)
                     | RS.NilItem => return()
                end
            let readSet : RS.read_set = FLS.@get-key(READ_SET / exh)
            let writeSet : RS.item = FLS.@get-key(WRITE_SET / exh)
            let rawStamp: long = #0(startStamp)
            fun acquire(ws:RS.item, acquired : RS.item) : RS.item = 
                case ws
                    of RS.Write(tv:tvar, contents:any, tl:RS.item) =>
                        let casRes : long = CAS(&1(tv), 0:long, rawStamp) (*lock it*)
                        if I64Eq(casRes, 0:long)  (*locked for first time*)
                        then apply acquire(tl, RS.Write(tv, contents, acquired))
                        else if I64Eq(casRes, rawStamp)    (*already locked it*)
                             then apply acquire(tl, acquired)
                             else (*release, but don't end atomic*)
                                  fun release(locks : RS.item) : () = 
                                    case locks 
                                        of RS.Write(tv:tvar, contents:any, tl:RS.item) =>
                                            do #1(tv) := 0:long         (*unlock*)
                                            apply release(tl)
                                         | RS.NilItem => return()
                                    end
                                  do apply release(acquired) 
                                  apply acquire(writeSet, RS.NilItem)
                     |RS.NilItem => return(acquired)
                end
            fun update(writes:RS.item, newStamp : stamp) : () = 
                case writes
                    of RS.Write(tv:tvar, newContents:any, tl:RS.item) =>
                        let newContents : any = promote(newContents)
                        do #2(tv) := newStamp            (*update version stamp*)
                        do #0(tv) := newContents         (*update contents*)
                        do #1(tv) := 0:long              (*unlock*)
                        apply update(tl, newStamp)       (*update remaining*)
                     | RS.NilItem => return()
                end
            let locks : RS.item = apply acquire(writeSet, RS.NilItem)
            fun unlock(x:unit / exh:exh) : unit = do apply release(locks) return(UNIT)
            let newStamp : stamp = VClock.@bump(/exh)  
            do RS.@validate-commit(readSet, startStamp, newStamp, unlock / exh)
            do apply update(locks, newStamp)
            return()
        ;
        
        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#0(in_trans))
            then apply f(UNIT/exh)
            else let stampPtr : ![stamp, int] = FLS.@get-key(STAMP_KEY / exh)
                 do #1(stampPtr) := 0
                 cont enter() = 
                     let newRS : RS.read_set = RS.@new()
                     do FLS.@set-key(READ_SET, newRS / exh)  (*initialize STM log*)
                     do FLS.@set-key(WRITE_SET, RS.NilItem / exh)
                     let stamp : stamp = VClock.@bump(/exh)
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
                     do FLS.@set-key(WRITE_SET, RS.NilItem / exh)
                     return(res)
                 throw enter()
      ;

      define @timeToString = Time.toString;
      
      define @print-stats(x:unit / exh:exh) : unit = 
        PRINT_PABORT_COUNT
        PRINT_FABORT_COUNT
        PRINT_COMBINED
        PRINT_KCOUNT
        return(UNIT);
        
      define @abort(x : unit / exh : exh) : any = 
         let e : cont() = FLS.@get-key(ABORT_KEY / exh)
         let stamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
         let readSet : [int, RS.item, RS.item] = FLS.@get-key(READ_SET / exh)
         throw e();        

      define @tvar-eq(arg : [tvar, tvar] / exh : exh) : bool = 
         if Equal(#0(arg), #1(arg))
         then return(true)
         else return(false);

    define @print2(x : ml_string / exh:exh) : unit =
    do ccall M_Print(#0(x)) return (UNIT)
 (*       let ffInfo : any = FLS.@get-key(FF_KEY / exh)
        if Equal(ffInfo, enum(0))
        then return(UNIT)
        else do ccall M_Print(#0(x))
             return(UNIT)
   *) ;

         
    )

    type 'a tvar = 'a PartialSTM.tvar
    val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@getABCDEFG)
    val new : 'a -> 'a tvar = _prim(@new)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val printStats : unit -> unit = _prim(@print-stats)
    val abort : unit -> 'a = _prim(@abort)
    val unsafeGet : 'a tvar -> 'a = _prim(@unsafe-get)
    val same : 'a tvar * 'b tvar -> bool = _prim(@tvar-eq)

    val print2 : string -> unit = _prim(@print2)
    
    

end












 
