(* stm.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Software Transactional Memory with partial aborts.
 *)


#define Read 0
#define Write 1

structure FullAbortSTM = (* :
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
#define BUMP_ABORT do ccall M_BumpCounter(0)
#define PRINT_ABORT_COUNT let counter : int = ccall M_GetCounter(0) \
                          do ccall M_Print_Int("Aborted %d transactions\n", counter)
#define BUMP_ALLOC do ccall M_BumpCounter(1)
#define PRINT_ALLOC_COUNT  let counter : int = ccall M_GetCounter(1) \
                           do ccall M_Print_Int("Allocated %d abort continuations\n", counter)
#else
#define BUMP_ABORT
#define PRINT_ABORT_COUNT
#define BUMP_ALLOC
#define PRINT_ALLOC_COUNT
#endif

    _primcode(

        extern void * M_Print_Int(void *, int);
        extern void * M_Print_Int2(void *, int, int);
        extern void M_Print_Long (void *, long);
        extern void M_BumpCounter(int);
        extern int M_GetCounter(int);
        
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

        define @get(tv : tvar / exh:exh) : any = 
            let myStamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
            let readSet : List.list = FLS.@get-key(READ_SET / exh)
            let writeSet : List.list = FLS.@get-key(WRITE_SET / exh)
            fun chkLog(writeSet : List.list) : Option.option = (*use local copy if available*)
                case writeSet
                    of CONS(hd:writeItem, tl:List.list) =>
#ifdef EAGER_CONFLICT                    
                        if Equal(#0(hd), tv)
                        then if I64Lt(#2(#0(hd)), #0(myStamp))
                             then let res : Option.option = Option.SOME(#1(hd))
                                  return(res)
                             else let newStamp : stamp = VClock.@bump(/exh)
                                  let e : exn = Fail(@"__ABORT_EXCEPTION__")
                                  throw exh(e)
                        else apply chkLog(tl)
#else
                        if Equal(#0(hd), tv)
                        then return(Option.SOME(#1(hd)))
                        else apply chkLog(tl)
#endif                        
                    | nil => return (Option.NONE)
                end
            let localRes : Option.option = apply chkLog(writeSet)
            case localRes
                of Option.SOME(v:any) => return(v)
                 | Option.NONE =>
                    fun lk() : () = 
                        let swapRes : long = CAS(&1(tv), 0:long, #0(myStamp))
                        if I64Eq(swapRes, 0:long)
                        then return()
                        else do Pause() apply lk()
                    do apply lk()
                    let current : any = #0(tv)
                    do #1(tv) := 0:long
                    let item : [tvar] = alloc(tv)
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
            cont enter() = 
                let startStamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
                let vp : vproc = SchedulerAction.@atomic-begin()
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
                fun validate(readSet : List.list, locks : List.list, newStamp : stamp) : () = 
                    case readSet 
                        of CONS(hd:readItem, tl:List.list) =>
                            let tv : tvar = #0(hd)
                            let e : exn = Fail(@"__ABORT_EXCEPTION__")
                            if I64Lt(#2(tv), rawStamp)  (*still valid*)
                            then apply validate(tl, locks, newStamp)
                            else do apply release(locks)
                                 do SchedulerAction.@atomic-end(vp)
                                 throw exh(e)
                         |nil => return()
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
                                 else do apply release(acquired)
                                      do SchedulerAction.@atomic-end(vp)
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
                let plusOne : stamp = I64Add(rawStamp, 1:long)
                do apply validate(readSet, locks, newStamp)
                do apply update(locks, newStamp)
                do SchedulerAction.@atomic-end(vp)
                return()
            throw enter()
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













 
