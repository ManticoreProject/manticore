(* ordered-tl2.pml
 *
 * COPYRIGHT (c) 2016 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Basline STM: TinySTM (http://se.inf.tu-dresden.de/pubs/papers/felber2008tinystm.pdf)
 *)

structure TinySTM = 
struct 
    datatype 'a item = Read of 'a * 'a item | Write of 'a * 'a * 'a item | NilItem

    _primcode(
    
        extern void M_Print_Long2(void*, long, long);

        typedef stamp = VClock.stamp;
        typedef tvar = FullAbortSTM.tvar;        
        typedef stamp_rec = ![long,long,long,long]; (*current timestamp, lock_val, old time stamp, thread ID*)

        (*this won't typecheck without the `inline` annotation*)
        define inline @abort(/exh:exh) noreturn = 
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            fun release(i : item, max : long) : long = 
                case i 
                   of NilItem => return(max)
                    | Write(tv:tvar, x:any, next : item) => 
                        let new_stamp : long = I64Add(#PREV_LOCK(tv), 1:long)
                        do #TVAR_CONTENTS(tv) := x               (*undo write*)
                        do #CURRENT_LOCK(tv) := new_stamp        (*release lock*)
                        if U64Gt(new_stamp, max)
                        then apply release(next, new_stamp)
                        else apply release(next, max)     
                end
            let max : long = apply release(writeSet, 0:long)
            do 
                let current : long = VClock.@get(/exh)
                if U64Gt(max, current)
                then
                    let _ : stamp = VClock.@bump(/exh)
                    return()
                else return()
            let e : cont() = FLS.@get-key(ABORT_KEY / exh)
            throw e();   

        define @ts-extend(readSet : item, myStamp : stamp_rec / exh:exh) : () = 
            fun lp(i : item) : () = 
                case i 
                   of NilItem => return()
                    | Read(tv:tvar, next:item) => 
                        let time : stamp = #CURRENT_LOCK(tv)
                        if U64Gt(time, #START_STAMP(myStamp))
                        then
                            if I64Eq(time, #LOCK_VAL(myStamp))
                            then apply lp(next)
                            else @abort(/exh)
                        else apply lp(next)
                end
            let ts : stamp = VClock.@get(/exh)
            do apply lp(readSet)
            do #START_STAMP(myStamp) := ts
            return()
        ;

        define @get(tv:tvar / exh:exh) : any = 
            let myStamp : stamp_rec = FLS.@get-key(STAMP_KEY / exh)
            let readSet : item = FLS.@get-key(READ_SET / exh)
            cont retry() = 
                let v1 : long = #CURRENT_LOCK(tv)
                let temp : any = #TVAR_CONTENTS(tv)
                if I64Eq(v1, #LOCK_VAL(myStamp))
                then return(temp) (*we own the lock*)
                else
                    let v2 : long = #CURRENT_LOCK(tv)
                    if I64Eq(v1, v2)
                    then 
                        if U64Lte(v1, #START_STAMP(myStamp))
                        then
                            let readSet : item = Read(tv, readSet)
                            do FLS.@set-key(READ_SET, readSet / exh)
                            return(temp)
                        else (*v1 > myStamp*)
                            if LOCKED(v1)
                            then @abort(/exh)
                            else (*!LOCKED(v1)*)
                                do @ts-extend(readSet, myStamp / exh)
                                throw retry()
                    else (*v1 != v2*)
                        if LOCKED(v1)
                        then @abort(/exh)
                        else (*!LOCKED(v1)*)
                            do @ts-extend(readSet, myStamp / exh)
                            throw retry()
            throw retry()
        ;

        define @put(arg : [tvar, any] / exh:exh) : unit = 
            let tv : tvar = #0(arg)
            let x : any = #1(arg)
            let myStamp : stamp_rec = FLS.@get-key(STAMP_KEY / exh)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            cont retry() = 
                let v1 : long = #CURRENT_LOCK(tv)
                if U64Lte(v1, #START_STAMP(myStamp))
                then
                    if BCAS(&CURRENT_LOCK(tv), v1, #LOCK_VAL(myStamp))
                    then
                        do #OLD_STAMP(tv) := v1
                        let writeSet : item = Write(tv, #TVAR_CONTENTS(tv), writeSet)
                        do FLS.@set-key(WRITE_SET, writeSet / exh)
                        let x : any = promote(x)
                        do #TVAR_CONTENTS(tv) := x
                        return(UNIT)
                    else @abort(/exh)
                else    
                    if I64Eq(v1, #LOCK_VAL(myStamp))
                    then 
                        let x : any = promote(x)
                        do #TVAR_CONTENTS(tv) := x
                        return(UNIT)
                    else    
                        if LOCKED(#CURRENT_LOCK(tv))
                        then @abort(/exh)
                        else 
                            let readSet : item = FLS.@get-key(READ_SET / exh)
                            do @ts-extend(readSet, myStamp / exh)
                            throw retry()
            throw retry()
        ;   

        define @force-abort(x : unit / exh : exh) : any = 
            let e : cont() = FLS.@get-key(ABORT_KEY / exh)
            throw e();        

        define @commit(/exh:exh) : () = 
            let readSet : item = FLS.@get-key(READ_SET / exh)
            let myStamp : stamp_rec = FLS.@get-key(STAMP_KEY / exh)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            if Equal(writeSet, NilItem)
            then return()
            else 
                let end_time : stamp = VClock.@inc(1:long/exh)
                do 
                    if I64Eq(end_time, #START_STAMP(myStamp))
                    then return()
                    else 
                        fun lp(i:item) : () = 
                            case i 
                               of NilItem => return()
                                | Read(tv:tvar, next:item) => 
                                    let time : long = #CURRENT_LOCK(tv)
                                    if U64Gt(time, #START_STAMP(myStamp))
                                    then 
                                        if I64Eq(time, #LOCK_VAL(myStamp))
                                        then apply lp(next)
                                        else @abort(/exh)
                                    else apply lp(next) 
                            end
                        apply lp(readSet)
                let end_time : stamp = I64Add(end_time, 1:long)
                fun unlock(i:item) : () = 
                    case i 
                       of NilItem => return()
                        | Write(tv:tvar, _:any, next:item) => 
                            do #CURRENT_LOCK(tv) := end_time
                            apply unlock(next)
                    end
                apply unlock(writeSet)
        ;
        
        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#0(in_trans))
            then apply f(UNIT/exh)
            else 
                let stampPtr : stamp_rec = FLS.@get-key(STAMP_KEY / exh)
                do #LOCK_VAL(stampPtr) := SET_MSB(#THREAD_ID(stampPtr))
                cont enter() = 
                    do FLS.@set-key(READ_SET, NilItem / exh)  (*initialize STM log*)
                    do FLS.@set-key(WRITE_SET, NilItem / exh)
                    let stamp : stamp = VClock.@get(/ exh)
                    do #START_STAMP(stampPtr) := stamp
                    do #0(in_trans) := true
                    do FLS.@set-key(ABORT_KEY, (any) enter/ exh)
                    let res : any = apply f(UNIT/exh)
                    do @commit(/exh)
                    do #0(in_trans) := false
                    do FLS.@set-key(READ_SET, NilItem / exh)
                    do FLS.@set-key(WRITE_SET, NilItem / exh)
                    return(res)
                throw enter()
        ;
    )

    type 'a tvar = 'a PartialSTM.tvar
    val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = FullAbortSTM.new
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val abort : unit -> 'a = _prim(@force-abort)
   
end












 
