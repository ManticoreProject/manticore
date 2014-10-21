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

    _primcode(
        define @init-counter(_:unit / exh:exh) : ml_int = 
            let x : ml_int = alloc(0)
            let x : ml_int = promote(x)
            return(x);
    )

    val initCounter : unit -> int = _prim(@init-counter)
    val abortCounter = initCounter()
    fun getAbortCounter() = abortCounter
    
    _primcode(

        define @get-aborts = getAbortCounter;

        define @bump-aborts() : () = 
            cont dummy(e:exn) = return()
            let x : ml_int = @get-aborts(UNIT/dummy)
            let x : ![int] = (![int]) x
            let v : int = I32FetchAndAdd(&0(x), 1)
            return();
        
        extern void * M_Print_Int(void *, int);
        extern void * M_Print_Int2(void *, int, int);
        extern void M_Print_Long (void *, long);
        typedef itemType = int; (*correponds to the above #define's*)
        typedef stamp = VClock.stamp;
        typedef tvar = ![any, long, stamp]; (*contents, lock, version stamp*)

        typedef logItem = [tvar,      (*0: tvar operated on*)
                           itemType,  (*1: type of action performed (read or write)*)
                           stamp,     (*2: time stamp taken when action was performed*)
                           any,       (*3: contents of local copy*)
                           cont()];   (*4: abort continuation*)

        define @new(x:any / exh:exh) : tvar = 
            let stamp : stamp = VClock.@bump(/exh)
            let tv : tvar = alloc(x, 0:long, stamp)
            let tv : tvar = promote(tv)
            return(tv)
        ;

        define @get(tv : tvar / exh:exh) : any = 
            fun enter() : any = 
                let v : any = #0(tv)
                fun chkLog(log : List.list) : Option.option = (*use local copy if available*)
                    case log
                        of CONS(hd:logItem, tl:List.list) =>
                            if Equal(#0(hd), tv)
                            then if I64Lt(#2(#0(hd)), #2(hd))
                                 then let res : Option.option = Option.SOME(#3(hd))
                                      return(res)
                                 else let k : cont() = #4(hd)
                                      let id : int = FLS.@get-id()
                                      do @bump-aborts()
                                      PDebugInt("Aborting via eager conflict detection with ID: %d\n", id)
                                      throw k()
                            else apply chkLog(tl)
                        | nil => return (Option.NONE)
                    end
                let log : List.list = FLS.@get-key(LOG_KEY / exh)
                let localRes : Option.option = apply chkLog(log)
                case localRes
                    of Option.SOME(v:any) => return(v)
                     | Option.NONE =>
                        cont k() =
                            do FLS.@set-key(LOG_KEY, log / exh) (*reset log*)
                            PDebugID("Inside abort continuation with ID: %d\n")
                            apply enter()
                        let stamp : stamp = VClock.@bump(/exh)
                        let current : any = #0(tv)
                        let item : logItem = alloc(tv, Read, stamp, current, k)
                        let newLog : List.list = CONS(item, log)
                        do FLS.@set-key(LOG_KEY, newLog / exh)
                        return(current)
                end
            apply enter()
        ;

        define @put(arg:[tvar, any] / exh:exh) : unit =
            fun enter() : unit = 
                let tv : tvar = #0(arg)
                let v : any = #1(arg)
                fun mkItem(log:List.list) : logItem = 
                    case log 
                        of CONS(hd:logItem, tl:List.list) =>
                            if Equal(#0(hd), tv)
                            then let item : logItem = alloc(tv, Write, #2(hd), v, #4(hd))
                                 return(item)
                            else apply mkItem(tl)
                         | nil => let stamp : stamp = VClock.@bump(/exh)
                                 cont k () = 
                                    do FLS.@set-key(LOG_KEY, log / exh)
                                    PDebugID("Inside abort continuation with ID: %d\n")
                                    apply enter()
                                 let item : logItem = alloc(tv, Write, stamp, v, k)
                                 return(item)
                    end
                let log : List.list = FLS.@get-key(LOG_KEY / exh)
                let item : logItem = apply mkItem(log)
                let newLog : List.list = CONS(item, log)
                do FLS.@set-key(LOG_KEY, newLog / exh)
                return(UNIT)
            apply enter()
        ;

        define @commit(/exh:exh) : () = 
            let stamp : stamp = VClock.@bump(/exh)
            let vp : vproc = SchedulerAction.@atomic-begin()
            fun release(locks : List.list) : () = 
                case locks 
                    of CONS(hd:logItem, tl:List.list) =>
                        let tv:tvar = #0(hd)
                        do #1(tv) := 0:long
                        apply release(tl)
                     | nil => return()
                end
            fun acquire(log:List.list, acquired : List.list, readSet : List.list) : (List.list, List.list) = 
                case log 
                    of CONS(hd:logItem, tl:List.list) =>
                        let tv : tvar = #0(hd)
                        if I32Eq(#1(hd), Read)
                        then apply acquire(tl, acquired, CONS(hd, readSet))
                        else let casRes : long = CAS(&1(tv), 0:long, stamp) (*lock it*)
                             if I64Eq(casRes, 0)
                             then apply acquire(tl, CONS(hd, acquired), readSet)  (*acquired for the first time*)
                             else if I64Eq(casRes, stamp)
                                  then apply acquire(tl, acquired, readSet)  (*already acquired this lock*)
                                  else do apply release(acquired)
                                       PDebug("Aborting because lock is already taken\n")
                                       do @bump-aborts()
                                       do SchedulerAction.@atomic-end(vp)
                                       let abortK : cont() = #4(hd)
                                       throw abortK()
                     |nil => return(acquired, readSet)
                end
            fun update(writes:List.list) : () = 
                case writes
                    of CONS(hd:logItem, tl:List.list) =>
                        let tv : tvar = #0(hd)           (*pull out the tvar*)
                        let newContents : any = #3(hd)   (*get the local contents*)
                        let newContents : any = promote(newContents)
                        do #0(tv) := newContents         (*update contents*)
                        do #2(tv) := stamp               (*update version stamp*)
                        do #1(tv) := 0:long              (*unlock*)
                        apply update(tl)                 (*update remaining*)
                     | nil => return()
                end
            let log : List.list = FLS.@get-key(LOG_KEY / exh)
            let (locks : List.list, readSet : List.list) = apply acquire(log, nil, nil)
            fun validate(readSet : List.list) : () = 
                case readSet 
                    of CONS(hd:logItem, tl:List.list) =>
                        let tv : tvar = #0(hd)
                        let abortK : cont() = #4(hd)
                        if I64Eq(#1(tv), 0)  (*check that it is unlocked*)
                        then if I64Lt(#2(tv), #2(hd))
                             then apply validate(tl)
                             else do apply release(locks)
                                  do SchedulerAction.@atomic-end(vp)
                                  do @bump-aborts()
                                  throw abortK()
                        else if I64Eq(#1(tv), stamp)   (*we locked it*)
                             then if Equal(#0(tv), #3(hd))  (*I64Lt(#2(tv), #2(hd)) *)
                                  then apply validate(tl)
                                  else do apply release(locks)
                                       do SchedulerAction.@atomic-end(vp)
                                       do @bump-aborts()
                                       throw abortK()
                             else do apply release(locks)
                                  do SchedulerAction.@atomic-end(vp)
                                  do @bump-aborts()
                                  throw abortK()
                     |nil => return()
              end            
            do apply validate(readSet)
            do apply update(locks)
            do SchedulerAction.@atomic-end(vp)
            PDebugID("Successfully committed with ID: %d\n")
            return()
        ;
        
        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            PDebugID("Entering transaction with ID: %d\n")
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#0(in_trans))
            then PDebug("entering nested transactions\n") apply f(UNIT/exh)
            else do FLS.@set-key(LOG_KEY, nil / exh)  (*initialize STM log*)
                 do #0(in_trans) := true           
                 let res : any = apply f(UNIT/exh)
                 do @commit(/exh)
                 do #0(in_trans) := false
                 PDebugID("Finished transaction with ID: %d\n")
                 return(res)
        ;            

       define @getID(x:unit / exh:exh) : ml_int =
        let id : int = FLS.@get-id()
        let id : [int] = alloc(id)
        return(id)
      ;

      define @print-stats(x:unit / exh:exh) : unit = 
        let aborts : ml_int = @get-aborts(x/exh)
        do ccall M_Print_Int("Total number of aborted transactions is %d\n", #0(aborts))
        return(UNIT);

    )

    	type 'a tvar = _prim(tvar)
    	val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = _prim(@new)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val getID : unit -> int = _prim(@getID)
    val printStats : unit -> unit = _prim(@print-stats)
end













 
