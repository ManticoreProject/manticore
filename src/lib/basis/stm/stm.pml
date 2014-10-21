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
(*
#define STMDebug
*)
#ifdef STMDebug
#define PDebug(msg)  do ccall M_Print(msg)  
#define PDebugInt(msg, v)  do ccall M_Print_Int(msg, v)  
#define PDebugLong(msg, v) do ccall M_Print_Long(msg, v)
#else
#define PDebug(msg) 
#define PDebugInt(msg, v)   
#define PDebugLong(msg, v) do ccall M_Print_Long(msg, v)
#endif
    _primcode(

(*TODO: update this to support seperate read/write sets instead of a single log.
**when committing, first lock the entire write set, and then validate the read set*)
(*Or figure out a better way to aqcuire locks on the write set before verifying the read set*)
        extern void * M_Print_Int(void *, int);
        extern void M_Print_Long (void *, long);
        typedef itemType = int; (*correponds to the above #define's*)
        typedef stamp = VClock.stamp;
        typedef tvar = ![any, long, ![stamp]]; (*contents, lock, version stamp*)

        typedef logItem = [tvar,      (*0: tvar operated on*)
                           itemType,  (*1: type of action performed (read or write)*)
                           stamp,     (*2: time stamp taken when action was performed*)
                           ![stamp],  (*3: pointer to current time stamp*)
                           any,       (*4: contents of local copy*)
                           cont()];   (*5: abort continuation*)

        define @new(x:any / exh:exh) : tvar = 
            let stamp : stamp = VClock.@bump(/exh)
            let tv : tvar = alloc(x, 0:long, (![long])alloc(stamp))
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
                            then if I64Lt(#0(#3(hd)), #2(hd))
                                 then let res : Option.option = Option.SOME(#4(hd))
                                      return(res)
                                 else let k : cont() = #5(hd)
                                      let id : int = FLS.@get-id()
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
                            let id : int = FLS.@get-id()
                            PDebugInt("Inside abort continuation with ID: %d\n", id)
                            apply enter()
                        let stamp : stamp = VClock.@bump(/exh)
                        let current : any = #0(tv)
                        let item : logItem = alloc(tv, Read, stamp, #2(tv), current, k)
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
                            then let item : logItem = alloc(tv, Write, #2(hd), #2(tv), v, #5(hd))
                                 return(item)
                            else apply mkItem(tl)
                         |nil => let stamp : stamp = VClock.@bump(/exh)
                                 cont k () = 
                                    do FLS.@set-key(LOG_KEY, log / exh)
                                    let id : int = FLS.@get-id()
                                    PDebugInt("Inside abort continuation with ID: %d\n", id)
                                    apply enter()
                                 let item : logItem = alloc(tv, Write, stamp, #2(tv), v, k)
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
            fun release(locks : List.list) : () = 
                case locks 
                    of CONS(hd:logItem, tl:List.list) =>
                        let tv:tvar = #0(hd)
                        do #1(tv) := 0:long
                        apply release(tl)
                     | nil => return()
                end
            fun acquire(log:List.list) : List.list = 
                let id : int = FLS.@get-id()
                case log 
                    of CONS(hd:logItem, tl:List.list) =>
                        let res : List.list = apply acquire(tl)
                        let tv : tvar = #0(hd)
                        let abortK : cont() = #5(hd)
                        if I32Eq(#1(hd), Read)
                        then let currentStamp : ![stamp] = #3(hd)
                             if I64Lt(#0(currentStamp), #2(hd))
                             then return(res)
                             else do apply release(res)
                                  throw abortK()
                        else let casRes : long = CAS(&1(tv), 0:long, stamp) (*lock it*)
                             if I64Eq(#1(tv), stamp)
                             then return(CONS(hd, res))
                             else do apply release(res)
                                  PDebugInt("Aborting because lock is already taken with ID: %d\n", id)
                                  PDebugLong("Lock value is %ld\n", casRes)
                                  throw abortK()
                     |nil => return(nil)
                end
            fun update(writes:List.list) : () = 
                case writes
                    of CONS(hd:logItem, tl:List.list) =>
                        if I64Eq(#0(#3(hd)), stamp)           (*already updated this one, do nothing...*)
                        then return()
                        else let tv : tvar = #0(hd)           (*pull out the tvar*)
                             let newContents : any = #4(hd)   (*get the local contents*)
                             let newContents : any = promote(newContents)
                             do #0(tv) := newContents         (*update contents*)
                             do #0(#2(tv)) := stamp           (*update version stamp*)
                             let intInterp : ml_int = (ml_int) newContents
                             PDebugInt("Updating contents to %d\n", #0(intInterp))
                             (*note that if we try and update this tvar again (corresponding
                             **to an earlier write, then we simply do nothing)*)
                             do apply update(tl)              (*update remaining*)
                             do #1(tv) := 0:long              (*unlock*)
                             return()
                     | nil => return()
                end
            let log : List.list = FLS.@get-key(LOG_KEY / exh)
            let writes : List.list = apply acquire(log)
            do apply update(writes)
            let id : int = FLS.@get-id()
            PDebugInt("Successfully committed with ID: %d\n", id)
            return()
        ;
        
        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            let id : int = FLS.@get-id()
            PDebugInt("Entering transaction with ID: %d\n", id)
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#0(in_trans))
            then PDebug("entering nested transactions\n") apply f(UNIT/exh)
            else do FLS.@set-key(LOG_KEY, nil / exh)  (*initialize STM log*)
                 do #0(in_trans) := true           
                 let res : any = apply f(UNIT/exh)
                 do @commit(/exh)
                 do #0(in_trans) := false
                 let id : int = FLS.@get-id()
                 PDebugInt("Finished transaction with ID: %d\n", id)
                 return(res)
        ;            

       define @getID(x:unit / exh:exh) : ml_int =
        let id : int = FLS.@get-id()
        let id : [int] = alloc(id)
        return(id)
      ;

    )

    	type 'a tvar = _prim(tvar)
    	val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = _prim(@new)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val getID : unit -> int = _prim(@getID)
end













 
