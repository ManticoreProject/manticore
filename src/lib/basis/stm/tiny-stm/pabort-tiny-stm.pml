(* ordered-tl2.pml
 *
 * COPYRIGHT (c) 2016 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * TinySTM with Partial Aborts
 * This uses bounded continuations and an ordered log
 *)

#define RS_TAIL        0
#define RS_LONG_PATH   1
#define RS_SHORT_PATH  2
#define RS_NUMK        3

#define LOG_TAG 0
#define LOG_TVAR 1
#define LOG_NEXT 2

#define WITHK_CONT 3
#define WITHK_NEXT 4
#define WITHK_PREV 5

#define WRITE_VAL 3
#define WRITE_NEXT 4

#define READ_SET_BOUND 21

structure TinySTMPartial = 
struct 
    (*NOTE: do not change the order of these.  The C runtime depends on it.*)
    datatype 'a item = WithoutK of 'a * 'a item 
                     | WithK of 'a * 'a item * 'a * 'a item * 'a item
                     | Write of 'a * 'a item * 'a
                     | NilItem

    _primcode(
        define @mk-tag(x:unit / exh:exh) : any = 
            let x : item = WithoutK(enum(0):any, NilItem)
            let x : [any] = ([any]) x
            return(#0(x));
    )

    val mkTag : unit -> 'a = _prim(@mk-tag)
    val tag = mkTag()
    fun getTag() = tag

    _primcode(
        define @print-headers(x:unit/exh:exh) : unit = 
            let withK : item = WithK(enum(0):any, NilItem, enum(0):any, NilItem, NilItem)
            let withoutK : item = WithoutK(enum(0):any, NilItem)
            let withK : [any] = ([any])withK
            let withoutK : [any] = ([any]) withoutK
            do ccall M_Print_Long("WithK tag = %lu\n", #0(withK))
            do ccall M_Print_Long("WithoutK tag = %lu\n", #0(withoutK))
            return (UNIT)
        ;
    )

    val printHeaders : unit -> unit = _prim(@print-headers)
    val _ = printHeaders()

    _primcode(
    
        extern void M_Print_Long2(void*, long, long);

        typedef stamp = VClock.stamp;
        typedef tvar = FullAbortSTM.tvar;        
        typedef stamp_rec = ![long,long,long,long]; (*current timestamp, lock_val, old time stamp, thread ID*)
        typedef read_set = ![item,item,item,long];  (*write set, long path, short path, num continuations*)
        typedef with_k = ![enum(4), tvar, item, any, item, item];
        typedef write = ![enum(4), tvar, item, any, item];
        typedef without_k = ![enum(4), tvar, item];

        (*
         * Allocate a new read set.  This will put a dummy checkpointed 
         * entry in the read set, this way we never have to check to see
         * if the tail is NULL.  Since we have to put a dummy node in the 
         * read set every time, we might as well make it a checkpoint and
         * put the full abort continuation in it.  
         *)
        define @new(abortK : cont() / exh:exh) : read_set = 
            let tref : any = TL2OrderedRS.@get-tref(UNIT / exh)
            let withK : item = WithK(tref, NilItem, abortK, NilItem, NilItem)
            let rs : read_set = alloc(withK, withK, NilItem, 0:long)   (*don't put checkpoint on the short path*)
            return(rs);

        (*this won't typecheck without the `inline` annotation*)
        define inline @abort(readSet : read_set, chkpnt : item, n:long / exh:exh) noreturn = 
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
            (*null the write set without rebuilding the FLS*)
            let casted : with_k = (with_k) chkpnt
            let k : cont() = #WITHK_CONT(casted)
            if Equal(chkpnt, #RS_LONG_PATH(readSet))
            then throw k() (*full abort*)
            else
                let newRS : read_set = alloc(#WITHK_PREV(casted), #RS_LONG_PATH(readSet), #WITHK_NEXT(casted), n)
                do FLS.@set-key(READ_SET, newRS / exh)
                do FLS.@set-key(WRITE_SET, NilItem / exh)
                throw k()
        ;

        (*
         * This is a bit clumsy, but we maintain the *previous* entry of the current
         * checkpoint.  When we need to abort, the previous entry becomes the new 
         * tail and we invoke the continuation of the next entry (which is necessarily a checkpoint)
         * To make this even uglier, when we find a violation, we need to check 
         * if the checkpoint is the first entry, in which case this corresponds to a full abort
         * there is no previous entry, so the "checkpoint" becomes the new head and tail.
         *)
        define @ts-extend(readSet : read_set, myStamp : stamp_rec / exh:exh) : () = 
            fun lp(i : item, chkpnt : item, count : long) : () = 
                case i 
                   of NilItem => return()
                    | WithoutK(tv:tvar, next:item) => 
                        let time : stamp = #CURRENT_LOCK(tv)
                        if U64Gt(time, #START_STAMP(myStamp))
                        then
                            if I64Eq(time, #LOCK_VAL(myStamp))
                            then apply lp(next, chkpnt, count)
                            else @abort(readSet, chkpnt, count / exh)
                        else apply lp(next, chkpnt, count)  
                    | WithK(tv:tvar, next:item, k:cont(), nextK:item, prev:item) =>
                        let time : stamp = #CURRENT_LOCK(tv)
                        if U64Gt(time, #START_STAMP(myStamp))
                        then
                            if I64Eq(time, #LOCK_VAL(myStamp))
                            then apply lp(next, i, I64Add(count, 1:long))
                            else @abort(readSet, chkpnt, count / exh)
                        else apply lp(next, i, I64Add(count, 1:long))  
                end
            let ts : stamp = VClock.@get(/exh)
            let long_path : item = #RS_LONG_PATH(readSet)
            let casted : with_k = (with_k) long_path
            (*first entry is a dummy node with the full abort continuation*)
            do apply lp(#LOG_NEXT(casted), long_path, 0:long) 
            do #START_STAMP(myStamp) := ts
            return()
        ;

        define @append(readSet : read_set, newItem : item, stamp : stamp_rec / exh:exh) : read_set = 
            let vp : vproc = host_vproc
            let nurseryBase : long = vpload(NURSERY_BASE, vp)
            let limitPtr : long = vpload(LIMIT_PTR, vp)
            let lastAddr : any = (any) #RS_TAIL(readSet)
            let casted : without_k = (without_k)lastAddr
            if U64Gte(lastAddr, nurseryBase)
            then
                if U64Lt(lastAddr, limitPtr)
                then (*last item is still in nursery*)
                    do #LOG_NEXT(casted) := newItem
                    do #RS_TAIL(readSet) := newItem
                    return(readSet)
                else (*not in nursery, add last item to remember set*)
                    let newRS : read_set = alloc(newItem, #1(readSet), #2(readSet), #3(readSet))
                    let rs : any = vpload(REMEMBER_SET, vp)
                    let newRemSet : [without_k, int, long, any] = alloc(casted, LOG_NEXT, #THREAD_ID(stamp), rs)
                    do vpstore(REMEMBER_SET, vp, newRemSet)
                    do #LOG_NEXT(casted) := newItem
                    do FLS.@set-key(READ_SET, newRS / exh)
                    return(newRS)
            else (*not in nursery, add last item to remember set*)
                let newRS : read_set = alloc(newItem, #1(readSet), #2(readSet), #3(readSet))
                let rs : any = vpload(REMEMBER_SET, vp)
                let newRemSet : [without_k, int, long, any] = alloc(casted, LOG_NEXT, #THREAD_ID(stamp), rs)
                do vpstore(REMEMBER_SET, vp, newRemSet)
                do #LOG_NEXT(casted) := newItem
                do FLS.@set-key(READ_SET, newRS / exh)
                return(newRS)
        ;   

        define @get-tag = getTag;

        define @log-read(rs : read_set, k : cont(), tv : tvar, stamp : stamp_rec / exh:exh) : () = 
            let captureCount : int = FLS.@get-counter()
            if I32Eq(captureCount, 1)
            then
                let newItem : item = WithK(tv, NilItem, k, #RS_SHORT_PATH(rs), #RS_TAIL(rs))
                let newRS : read_set = @append(rs, newItem, stamp / exh)
                do #RS_SHORT_PATH(newRS) := newItem
                if I32Lte(#RS_NUMK(newRS), READ_SET_BOUND)
                then (*we have enough room for this one*)
                    let freq : int = FLS.@get-counter2()
                    FLS.@set-counter(freq)
                else (*filter read set*)
                    fun dropKs(l:item, n:long) : long =   (*drop every other continuation*)
                        case l
                           of NilItem => return(n)
                            | WithK(_:tvar,_:item,_:cont(any),next:item,_:item) =>
                                case next
                                   of NilItem => return(n)
                                    | WithK(_:tvar,_:item,_:cont(any),nextNext:item,_:item) =>
                                    (* NOTE: if compiled with -debug, this will generate warnings
                                     * that we are updating a bogus local pointer, however, given the
                                     * nature of the data structure, we do preserve the heap invariants*)
                                    let withoutKTag : enum(4) = @get-tag(UNIT/exh)
                                    let l : with_k = (with_k) l
                                    let next : with_k = (with_k) next
                                    do #WITHK_CONT(next) := enum(0):any
                                    do #LOG_TAG(next) := withoutKTag
                                    do #WITHK_NEXT(l) := nextNext
                                    apply dropKs(nextNext, I64Sub(n, 1:long))
                                end
                        end
                    let kCount : long = apply dropKs(#RS_SHORT_PATH(newRS), #RS_NUMK(newRS))
                    do #RS_NUMK(newRS) := kCount
                    let freq : int = FLS.@get-counter2()
                    let newFreq : int = I32Mul(freq, 2)
                    do FLS.@set-counter(newFreq)
                    do FLS.@set-counter2(newFreq)
                    return()
            else (*don't record continuation*)
                let newItem : item = WithoutK(tv, NilItem)
                let newRS : read_set = @append(rs, newItem, stamp / exh)
                return()
        ;

        (*just abort for now*)
        define inline @poll(readSet : read_set/exh:exh) noreturn = 
            let head : item = #RS_LONG_PATH(readSet)
            @abort(readSet, head, 0:long / exh)
        ;

        define @get(tv:tvar / exh:exh) : any = 
            let myStamp : stamp_rec = FLS.@get-key(STAMP_KEY / exh)
            let readSet : read_set = FLS.@get-key(READ_SET / exh)
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
                            do @log-read(readSet, retry, tv, myStamp / exh)
                            return(temp)
                        else (*v1 > myStamp*)
                            if LOCKED(v1)
                            then @poll(readSet/exh)
                            else (*!LOCKED(v1)*)
                                do @ts-extend(readSet, myStamp / exh)
                                throw retry()
                    else (*v1 != v2*)
                        if LOCKED(v1)
                        then @poll(readSet/exh)
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
                        do FLS.@set-counter(0) (*no more checkpoints*)
                        return(UNIT)
                    else 
                        let readSet : read_set = FLS.@get-key(READ_SET / exh)
                        @poll(readSet/exh)
                else    
                    if I64Eq(v1, #LOCK_VAL(myStamp))
                    then 
                        let x : any = promote(x)
                        do #TVAR_CONTENTS(tv) := x
                        return(UNIT)
                    else    
                        if LOCKED(#CURRENT_LOCK(tv))
                        then 
                            let readSet : read_set = FLS.@get-key(READ_SET / exh)
                            @poll(readSet/exh)
                        else 
                            let readSet : read_set = FLS.@get-key(READ_SET / exh)
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
                (*                | Read(tv:tvar, next:item) => 
                                    let time : long = #CURRENT_LOCK(tv)
                                    if U64Gt(time, #START_STAMP(myStamp))
                                    then 
                                        if I64Eq(time, #LOCK_VAL(myStamp))
                                        then apply lp(next)
                                        else @abort(/exh)
                                    else apply lp(next)  *)
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
                    let freq : int = FLS.@get-counter2()
                    do FLS.@set-counter(freq)
                    let rs : read_set = @new(enter / exh)
                    do FLS.@set-key(READ_SET, rs / exh)  (*initialize STM log*)
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
    type 'a tvar = 'a FullAbortSTM.tvar
    val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = FullAbortSTM.new
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val abort : unit -> 'a = _prim(@force-abort)
   
end












 
