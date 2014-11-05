(* stm.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Software Transactional Memory with partial aborts.
 *)

#define Read 0
#define Write 1

structure STM = (* :
    sig
	
*)
struct

#ifndef NDEBUG
#define PDebug(msg)  do ccall M_Print(msg)  
#define PDebugInt(msg, v)  do ccall M_Print_Int(msg, v)  
#define PDebugInt2(msg, v1, v2)  do ccall M_Print_Int2(msg, v1, v2)  
#define PDebugLong(msg, v) do ccall M_Print_Long(msg, v)
#define PDebugID(msg) let id : int = FLS.@get-id() do ccall M_Print_Int(msg, id)
#else
#define PDebug(msg) 
#define PDebugInt(msg, v)   
#define PDebugInt2(msg, v1, v2) 
#define PDebugLong(msg, v) 
#define PDebugID(msg) 
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


#define TIME
#ifdef TIME
#define START do ccall M_StartTimer()
#define STOP do ccall M_StopTimer()
#else
#define START
#define STOP
#endif

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

        typedef readItem = [tvar,       (*0: tvar operated on*)
                            cont(any),  (*1: abort continuation*)
                            List.list]; (*2: write set*)

        typedef writeItem = [tvar,    (*0: tvar operated on*)
                             any];    (*1: contents of local copy*)

        define @new(x:any / exh:exh) : tvar = 
            let tv : tvar = alloc(x, 0:long, 0:long)
            let tv : tvar = promote(tv)
            return(tv)
        ;

        define @get(tv:tvar / exh:exh) : any = 
            START
            let myStamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
            let readSet : List.list = FLS.@get-key(READ_SET / exh)
            let writeSet : List.list = FLS.@get-key(WRITE_SET / exh)
            let v : any = #0(tv)
            cont retK(x:any) = return(x)
#ifdef EAGER_CONFLICT            
            fun abort(rs : List.list, newStamp : stamp, abortItem:Option.option, newRS : List.list) : () = 
                case rs
                    of CONS(hd:readItem, tl:List.list) =>
                        if I64Lt(#2(#0(hd)), #0(myStamp))
                        then apply abort(tl, newStamp, abortItem, newRS)
                        else apply abort(tl, newStamp, Option.SOME(hd), tl)
                     | nil => case abortItem
                                of Option.SOME(item:readItem) =>
                                    let old : long = CAS(&1(tv), 0:long, newStamp)
                                    if I64Eq(old, 0:long)
                                    then let newRS : List.list = CONS(item, newRS)
                                         do FLS.@set-key(READ_SET, newRS / exh)
                                         do FLS.@set-key(WRITE_SET, #2(item) / exh)
                                         let current : any = #0(tv)     (*read tvar*)
                                         do #1(tv) := 0:long     (*unlock*)
                                         BUMP_ABORT 
                                         let abortK : cont(any) = #1(item)
                                         throw abortK(current)
                                    else apply abort(readSet, newStamp, Option.NONE, nil)
                                | Option.NONE => return()
                             end
                end
#endif                
           fun chkLog(writeSet : List.list) : Option.option = (*use local copy if available*)
                case writeSet
                    of CONS(hd:writeItem, tl:List.list) =>
#ifdef EAGER_CONFLICT                        
                        if Equal(#0(hd), tv)
                        then if I64Lt(#2(#0(hd)), #0(myStamp))
                             then let res : Option.option = Option.SOME(#1(hd))
                                  return(res)
                             else PDebugID("Aborting via eager conflict detection with ID: %d\n")
                                  let newStamp : stamp = VClock.@bump(/exh)                                
                                  do apply abort(readSet, newStamp, Option.NONE, nil)
                                  do ccall M_Print("Error: we should never get here\n")
                                  return(Option.NONE)
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
               of Option.SOME(v:any) => STOP return(v)
                | Option.NONE =>
                   (*must have exclusive access when reading for first time*)
                   let swapRes : long = CAS(&1(tv), 0:long, #0(myStamp))
                   if I64Eq(swapRes, 0:long)
                   then let current : any = #0(tv)
                        do #1(tv) := 0:long
                        let item : readItem = alloc(tv, retK, writeSet)
                        let newReadSet : List.list = CONS(item, readSet)
                        do FLS.@set-key(READ_SET, newReadSet / exh)
                        STOP
                        return(current)
                   else 
#ifdef EAGER_CONFLICT                   
                        let newStamp : stamp = VClock.@bump(/exh)
                        do apply abort(readSet, newStamp, Option.NONE, nil)
#endif                        
                        let e : exn = Fail(@"__ABORT_EXCEPTION__")
                        STOP
                        throw exh(e)
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
                fun validate(readSet:List.list, locks:List.list, newStamp : stamp, abortItem : Option.option, newRS : List.list) : () =
                    case readSet
                        of CONS(hd : readItem, tl:List.list) =>
                            let tv : tvar = #0(hd)
                            if I64Lt(#2(tv), rawStamp)    (*stamp still valid*)
                            then if I64Eq(#1(tv), 0:long)     (*hasn't been locked*)
                                 then apply validate(tl, locks, newStamp, abortItem, newRS)
                                 else if I64Eq(#1(tv), rawStamp)   (*we locked it*)
                                      then apply validate(tl, locks, newStamp, abortItem, newRS)
                                      else apply validate(tl, locks, newStamp, Option.SOME(hd), tl)
                            else apply validate(tl, locks, newStamp, Option.SOME(hd), tl)
                        | nil => case abortItem
                                    of Option.SOME(item:readItem) =>
                                        do apply release(locks)
                                        let tv : tvar = #0(item)
                                        fun lk() : () = 
                                            let old : long = CAS(&1(tv), 0:long, newStamp)
                                            if I64Eq(old, 0:long)
                                            then let current : any = #0(tv)
                                                 do #1(tv) := 0:long
                                                 do SchedulerAction.@atomic-end(vp)
                                                 let newRS : List.list = CONS(item, newRS)
                                                 do FLS.@set-key(READ_SET, newRS / exh)
                                                 do FLS.@set-key(WRITE_SET, #2(item) / exh)
                                                 do #0(startStamp) := newStamp
                                                 BUMP_ABORT 
                                                 let abortK : cont(any) = #1(item)
                                                 throw abortK(current)
                                            else do Pause() apply lk()
                                        apply lk()
                                    | Option.NONE => return()
                                 end
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
                                      do apply validate(readSet, acquired, newStamp, Option.NONE, nil)  (*figure out where to abort to*)
                                      do apply release(acquired)
                                      do SchedulerAction.@atomic-end(vp)
                                      throw enter()  (*if all reads are still valid, try and commit again*)
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
                do apply validate(readSet, locks, newStamp, Option.NONE, nil)
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

       define @getID(x:unit / exh:exh) : ml_int =
        let id : int = FLS.@get-id()
        let id : [int] = alloc(id)
        return(id)
      ;

      define @timeToString = Time.toString;
        
      define @print-stats(x:unit / exh:exh) : unit = 
        PRINT_ABORT_COUNT
        let t : long = ccall M_GetTimeAccum()
        let s : ml_string = @timeToString(alloc(t) / exh)
        do ccall M_Print("Total time spent reading was ")
        do ccall M_Print(#0(s))
        do ccall M_Print(" seconds\n")
        return(UNIT);

      define @whichException(e:exn / exh:exh) : unit = 
        case e
            of Fail(s:ml_string) => 
                do ccall M_Print("Fail exception: ")
                do ccall M_Print(#0(s))
                return(UNIT)
            | _ => do ccall M_Print("Unkown exception\n")
                   throw exh(e)
                (*
            | Bind => 
                do ccall M_Print("Bind exception\n")
                return(UNIT)
            | Div =>
                do ccall M_Print("Div exception\n")
                return(UNIT)
            | Match =>
                do ccall M_Print("Match exception\n")
                return(UNIT) *)
       end;
        
    )
    val whichException : exn -> unit = _prim(@whichException)

    	type 'a tvar = _prim(tvar)
    	val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = _prim(@new)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val getID : unit -> int = _prim(@getID)
    val printStats : unit -> unit = _prim(@print-stats)
end


(*
Notes:
    -record restarts in transactional log
    -if lock is held when acquiring, spin until lock is released and abort
*)










 
