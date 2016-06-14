(* ordered-tl2.pml
 *
 * COPYRIGHT (c) 2016 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Full Abort TinySTM with ordered read set
 *)

#define RS_TAIL        0
#define RS_HEAD   1

#define LOG_TAG 0
#define LOG_TVAR 1
#define LOG_NEXT 2

#define WITHK_CONT 3
#define WITHK_NEXT 4
#define WITHK_PREV 5

#define WRITE_VAL 3
#define WRITE_NEXT 4

structure TinySTMOrdered = 
struct 
    datatype 'a item = Read of 'a * 'a item 
                     | Write of 'a * 'a item * 'a
                     | NilItem

    _primcode(
    
        extern void M_Print_Long2(void*, long, long);

        typedef stamp = VClock.stamp;
        typedef tvar = FullAbortSTM.tvar;        
        typedef stamp_rec = ![long,long,long,long]; (*current timestamp, lock_val, old time stamp, thread ID*)
        typedef read_set = ![item,item];  (*write set, long path, short path, num continuations*)
        typedef write = ![enum(4), tvar, item, any, item];
        typedef read = ![enum(4), tvar, item];

        define @new(abortK : cont() / exh:exh) : read_set = 
            let tref : any = TL2OrderedRS.@get-tref(UNIT / exh)
            let head : item = Read(tref, NilItem)
            let rs : read_set = alloc(head, head)  
            return(rs);

(*#define MSG(msg, ty) , msg ty*)
#define MSG(msg, ty)

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
            BUMP_FABORT
            let e : cont() = FLS.@get-key(ABORT_KEY / exh)
            throw e();   

        define @ts-extend(readSet : read_set, myStamp : stamp_rec / exh:exh) : () = 
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
            do apply lp(#RS_HEAD(readSet))
            do #START_STAMP(myStamp) := ts
            return()
        ;

        define @append(readSet : read_set, newItem : item, stamp : stamp_rec / exh:exh) : read_set = 
            let vp : vproc = host_vproc
            let nurseryBase : long = vpload(NURSERY_BASE, vp)
            let limitPtr : long = vpload(LIMIT_PTR, vp)
            let lastAddr : any = (any) #RS_TAIL(readSet)
            let casted : read = (read)lastAddr
            if U64Gte(lastAddr, nurseryBase)
            then
                if U64Lt(lastAddr, limitPtr)
                then (*last item is still in nursery*)
                    do #LOG_NEXT(casted) := newItem
                    do #RS_TAIL(readSet) := newItem
                    return(readSet)
                else (*not in nursery, add last item to remember set*)
                    let newRS : read_set = alloc(newItem, #1(readSet))
                    let rs : any = vpload(REMEMBER_SET, vp)
                    let newRemSet : [read, int, long, any] = alloc(casted, LOG_NEXT, #THREAD_ID(stamp), rs)
                    do vpstore(REMEMBER_SET, vp, newRemSet)
                    do #LOG_NEXT(casted) := newItem
                    do FLS.@set-key(READ_SET, newRS / exh)
                    return(newRS)
            else (*not in nursery, add last item to remember set*)
                let newRS : read_set = alloc(newItem, #1(readSet))
                let rs : any = vpload(REMEMBER_SET, vp)
                let newRemSet : [read, int, long, any] = alloc(casted, LOG_NEXT, #THREAD_ID(stamp), rs)
                do vpstore(REMEMBER_SET, vp, newRemSet)
                do #LOG_NEXT(casted) := newItem
                do FLS.@set-key(READ_SET, newRS / exh)
                return(newRS)
        ;   

        define inline @poll(pollingTVar:tvar/exh:exh) : () = 
            fun lp(i : int) : () = 
                if I32Eq(i, 0)
                then
                    @abort(/exh)
                else
                    if LOCKED(#CURRENT_LOCK(pollingTVar))
                    then apply lp(I32Sub(i, 1))
                    else return()
            apply lp(100)
        ;

        (*
         * IMPORTANT: we must lookup the read set **INSIDE** the retry continuation
         * this way, if we partially abort here, we will get the most recent read set.
         * It's possible that we could reallocate the read set inside of @log-read
         *)
        define @get(tv:tvar / exh:exh) : any = 
            let myStamp : stamp_rec = FLS.@get-key(STAMP_KEY / exh)
            cont retry() = 
                let readSet : read_set = FLS.@get-key(READ_SET / exh) 
                let v1 : long = #CURRENT_LOCK(tv)
                let temp : any = #TVAR_CONTENTS(tv)
                if U64Eq(v1, #LOCK_VAL(myStamp))
                then return(temp) (*we own the lock*)
                else
                    let v2 : long = #CURRENT_LOCK(tv)
                    if U64Eq(v1, v2)
                    then 
                        if U64Lte(v1, #START_STAMP(myStamp))
                        then
                            let i : item = Read(tv, NilItem)
                            let _ : read_set = @append(readSet, i, myStamp / exh)
                            return(temp)
                        else (*v1 > myStamp*)
                            if LOCKED(v1)
                            then 
                                do @poll(tv/exh)
                                throw retry()
                            else (*!LOCKED(v1)*)
                                do @ts-extend(readSet, myStamp / exh)
                                throw retry()
                    else (*v1 != v2*)
                        if LOCKED(v1)
                        then 
                            do @poll(tv/exh)
                            throw retry()
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
                    else 
                        do @poll(tv/exh)
                        throw retry()
                else    
                    if U64Eq(v1, #LOCK_VAL(myStamp))
                    then 
                        let x : any = promote(x)
                        do #TVAR_CONTENTS(tv) := x
                        return(UNIT)
                    else    
                        if LOCKED(#CURRENT_LOCK(tv))
                        then 
                            do @poll(tv/exh)
                            throw retry() 
                        else 
                            let readSet : read_set = FLS.@get-key(READ_SET / exh)
                            do @ts-extend(readSet, myStamp / exh)
                            throw retry()
            throw retry()
        ;   

        define @force-abort(x : unit / exh : exh) : any = 
            let k : cont() = FLS.@get-key(ABORT_KEY / exh)
            throw k()
        ;

        define @commit(/exh:exh) : () = 
            let readSet : read_set = FLS.@get-key(READ_SET / exh)
            let myStamp : stamp_rec = FLS.@get-key(STAMP_KEY / exh)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            if Equal(writeSet, NilItem)
            then return()  (*read only transaction*)
            else 
                let end_time : stamp = VClock.@inc(1:long/exh)
                do 
                    if U64Eq(end_time, #START_STAMP(myStamp))
                    then return()  (*no one else could have committed*)
                    else
                        fun lp(i:item) : () = 
                            case i 
                               of NilItem => return()
                                | Read(tv:tvar, next:item) => 
                                    let time : stamp = #CURRENT_LOCK(tv)
                                    if U64Gt(time, #START_STAMP(myStamp))
                                    then
                                        if U64Eq(time, #LOCK_VAL(myStamp))
                                        then apply lp(next)
                                        else @abort(/exh)
                                    else apply lp(next)
                            end
                        let head : item = #RS_HEAD(readSet)
                        apply lp(head) 
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
                    let rs : read_set = @new(enter / exh)
                    do FLS.@set-key(READ_SET, rs / exh)  (*initialize STM log*)
                    do FLS.@set-key(WRITE_SET, NilItem / exh)
                    do FLS.@set-key(ABORT_KEY, enter / exh)
                    let stamp : stamp = VClock.@get(/ exh)
                    do #START_STAMP(stampPtr) := stamp
                    do #0(in_trans) := true
                    let res : any = apply f(UNIT/exh)
                    do @commit(/exh)
                    do #0(in_trans) := false
                    do FLS.@set-key(READ_SET, NilItem / exh)
                    do FLS.@set-key(WRITE_SET, NilItem / exh)
                    do FLS.@set-key(ABORT_KEY, NilItem / exh)
                    return(res)
                throw enter()
        ;
    )
    type 'a tvar = 'a FullAbortSTM.tvar
    val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = FullAbortSTM.new
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val abort : unit -> 'a = _prim(@force-abort)
   
end












 
