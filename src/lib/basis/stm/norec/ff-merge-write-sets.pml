structure NoRecMergeRSWriteSets = 
struct

#define READ_SET_BOUND 21

    structure RS = FFReadSetMergeWriteSets

	_primcode(
		(*I'm using ![any, long, long] as the type
		 * for tvars so that the typechecker will treat them
		 * as the same type as the other STM implementations.
		 * However, only the first element is ever used*)
		typedef tvar = FullAbortSTM.tvar;
		typedef stamp = VClock.stamp;

        extern void M_PruneRemSetAll(void*, long);

		define @getMergeWS(tv : tvar / exh:exh) : any = 
			let in_trans : [bool] = FLS.@get-key(IN_TRANS / exh)
            do 	
            	if(#0(in_trans))
               	then return()
               	else 
               		do ccall M_Print("Trying to read outside a transaction!\n")
                  	let e : exn = Fail(@"Reading outside transaction\n")
                    throw exh(e)
            let myStamp : ![stamp, int, int, long] = FLS.@get-key(STAMP_KEY / exh)
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
            do  if I64Gt(#1(tv), 0:long)
                then RS.@fast-forward2(readSet, writeSet, tv, retK, myStamp / exh)
                else return()
            let localRes : Option.option = apply chkLog(writeSet)
            case localRes
               of Option.SOME(v:any) => 
                    do RS.@insert-local-read(tv, v, readSet, myStamp, writeSet / exh)
                    return(v)
                | Option.NONE =>
                	fun getLoop() : any = 
                        let v : any = #0(tv)
                        do FenceRead()
                		let t : long = VClock.@get(/exh)
                		if I64Eq(t, #0(myStamp))
                		then return(v)
                		else
                			do RS.@c-validate(readSet, myStamp, true / exh)
                			apply getLoop()
                	let current : any = apply getLoop()
                    let captureCount : int = FLS.@get-counter()
                    if I32Eq(captureCount, 1)
                    then
                        let kCount : int = RS.@getNumK(readSet)
                        if I32Lt(kCount, READ_SET_BOUND)
                        then
                            do RS.@insert-with-k(tv, current, retK, writeSet, readSet, myStamp / exh)
                            let captureFreq : int = FLS.@get-counter2()
                            do FLS.@set-counter(captureFreq)
                            return(current)
                        else
                            do RS.@insert-with-k(tv, current, retK, writeSet, readSet, myStamp / exh)
                            do RS.@filterRS(readSet, myStamp/ exh)
                            let captureFreq : int = FLS.@get-counter2()
                            let newFreq : int = I32Mul(captureFreq, 2)
                            do FLS.@set-counter(newFreq)
                            do FLS.@set-counter2(newFreq)
                            return(current)
                    else
                        do FLS.@set-counter(I32Sub(captureCount, 1))
                        do RS.@insert-without-k(tv, current, readSet, myStamp / exh)
                        return(current)
            end
		;

        define @commit(stamp : ![stamp,int,int,long]/exh:exh) : () =
        	let readSet : RS.read_set = FLS.@get-key(READ_SET / exh)
        	let writeSet : RS.item = FLS.@get-key(WRITE_SET / exh)
        	let counter : ![long] = VClock.@get-boxed(/exh)
        	fun lockClock() : () = 
        		let current : stamp = #0(stamp)
        		let old : long = CAS(&0(counter), current, I64Add(current, 1:long))
        		if I64Eq(old, current)
        		then return()
        		else
        			do RS.@validate(readSet, stamp, false / exh)
        			apply lockClock()
        	do apply lockClock()
        	fun writeBack(ws:RS.item) : () = 
        		case ws 
        		   of RS.NilItem => return()
        			| RS.Write(tv:tvar, x:any, next:RS.item) => 
        				let x : any = promote(x)
        				do #0(tv) := x
        				apply writeBack(next)
        		end
            fun reverseWS(ws:RS.item, new:RS.item) : RS.item = 
                case ws 
                   of RS.NilItem => return(new)
                    | RS.Write(tv:tvar, x:any, next:RS.item) => apply reverseWS(next, RS.Write(tv, x, new))
                end
            let writeSet : RS.item = apply reverseWS(writeSet, RS.NilItem)
        	do apply writeBack(writeSet)
            do FenceRead()
        	do #0(counter) := I64Add(#0(stamp), 2:long) (*unlock clock*)
            let ffInfo : RS.read_set =  FLS.@get-key(FF_KEY / exh)
            do RS.@decCounts(ffInfo / exh)
        	return()
        ;

		define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#0(in_trans))
            then apply f(UNIT/exh)
            else 
            	let stampPtr : ![stamp, int, int, long] = FLS.@get-key(STAMP_KEY / exh)
                do FLS.@set-key(FF_KEY, enum(0) / exh)
                cont enter() = 
                    let freq : int = FLS.@get-counter2()
                    do FLS.@set-counter(freq)
                    cont abortK(x:any) = BUMP_FABORT do #0(in_trans) := false throw enter()
                    let rs : RS.read_set = RS.@new(abortK)
                    do FLS.@set-key(READ_SET, rs / exh)  (*initialize STM log*)
                    do FLS.@set-key(WRITE_SET, RS.NilItem / exh)
                    let stamp : stamp = NoRecFF.@get-stamp(/exh)
                    do #0(stampPtr) := stamp
                    do #0(in_trans) := true
                    do FLS.@set-key(ABORT_KEY, abortK / exh)
                    cont transExh(e:exn) = 
                        do ccall M_Print("Warning: exception raised in transaction\n")
                        throw exh(e)
                    let res : any = apply f(UNIT/transExh)
                    do @commit(stampPtr / transExh)
                    let vp : vproc = host_vproc
                    do ccall M_PruneRemSetAll(vp, #3(stampPtr))
                    do #0(in_trans) := false
                    do FLS.@set-key(READ_SET, RS.NilItem / exh)
                    do FLS.@set-key(WRITE_SET, RS.NilItem / exh)
                    do FLS.@set-key(FF_KEY, enum(0) / exh)
                    return(res)
                throw enter()
      	;

        define @putMergeWS(arg:[tvar, any] / exh:exh) : unit =
            let in_trans : [bool] = FLS.@get-key(IN_TRANS / exh)
            do if(#0(in_trans))
               then return()
               else do ccall M_Print("Trying to write outside a transaction!\n")
                    let e : exn = Fail(@"Writing outside transaction\n")
                    throw exh(e)
            let tv : tvar = #0(arg)
            let v : any = #1(arg)
            let writeSet : RS.item = FLS.@get-key(WRITE_SET / exh)
            let newWriteSet : RS.item = NoRecOrderedReadSet.Write(tv, v, writeSet)
            do FLS.@set-key(WRITE_SET, newWriteSet / exh)
            return(UNIT)
        ;

        define @get-ref-count(arg : tvar / exh:exh) : ml_long = 
            let count : long = #1(arg)
            let count : [long] = alloc(count)
            return(count)
        ;
(*)
        define @get-with-context(arg : [tvar, ml_string] / exh:exh) : any = 
            let tv : tvar = #0(arg)
            let in_trans : [bool] = FLS.@get-key(IN_TRANS / exh)
            do  
                if(#0(in_trans))
                then return()
                else 
                    do ccall M_Print("Trying to read outside a transaction!\n")
                    let e : exn = Fail(@"Reading outside transaction\n")
                    throw exh(e)
            let myStamp : ![stamp, int, int, long] = FLS.@get-key(STAMP_KEY / exh)
            let readSet : RS.read_set = FLS.@get-key(READ_SET / exh)
            let writeSet : RS.item = FLS.@get-key(WRITE_SET / exh)
            fun chkLog(writeSet : RS.item) : Option.option = 
                case writeSet
                   of RS.Write(tv':tvar, contents:any, tl:RS.item) =>
                        if Equal(tv', tv)
                        then return(Option.SOME(contents))
                        else apply chkLog(tl)
                    | RS.NilItem => return (Option.NONE)
                end
            cont retK(x:any) = return(x)
            do  if I64Gt(#1(tv), 0:long)
                then RS.@fast-forward2(readSet, writeSet, tv, retK, myStamp, #1(arg) / exh)
                else return()
            let localRes : Option.option = apply chkLog(writeSet)
            case localRes
               of Option.SOME(v:any) => 
                    do RS.@insert-local-read(tv, v, readSet, myStamp, writeSet / exh)
                    return(v)
                | Option.NONE =>
                    fun getLoop() : any = 
                        let v : any = #0(tv)
                        let t : long = VClock.@get(/exh)
                        if I64Eq(t, #0(myStamp))
                        then return(v)
                        else
                            do RS.@validate(readSet, myStamp, true / exh)
                            apply getLoop()
                    let current : any = apply getLoop()
                    let captureCount : int = FLS.@get-counter()
                    if I32Eq(captureCount, 1)
                    then
                        let kCount : int = RS.@getNumK(readSet)
                        if I32Lt(kCount, READ_SET_BOUND)
                        then
                            do RS.@insert-with-k(tv, current, retK, writeSet, readSet, myStamp / exh)
                            let captureFreq : int = FLS.@get-counter2()
                            do FLS.@set-counter(captureFreq)
                            return(current)
                        else
                            do RS.@insert-with-k(tv, current, retK, writeSet, readSet, myStamp / exh)
                            do RS.@filterRS(readSet, myStamp/ exh)
                            let captureFreq : int = FLS.@get-counter2()
                            let newFreq : int = I32Mul(captureFreq, 2)
                            do FLS.@set-counter(newFreq)
                            do FLS.@set-counter2(newFreq)
                            return(current)
                    else
                        do FLS.@set-counter(I32Sub(captureCount, 1))
                        do RS.@insert-without-k(tv, current, readSet, myStamp / exh)
                        return(current)
            end
        ;
*)
        typedef syncVar = ![bool];

        define @mk-sync-var(x:bool / exh:exh) : ![bool] = 
            let x : ![bool] = alloc(x)
            return(x);
        
        (* atomic-sync : (unit -> 'a) -> ![bool] -> bool
         * the first argument is the function to be run atomically
         * the second is a bool ref that is used for synchronizing threads
         * if the ref is equal to the third bool argument, proceed, otherwise spin 
         * until it is equal
         *)
        define @atomic-sync(arg:[fun(unit / exh -> any), ![bool], bool] / exh:exh) : any = 
            let f : fun(unit / exh -> any) = #0(arg)
            let syncVal : ![bool] = #1(arg)
            let waitVal : bool = #2(arg)
            let aborted : ![bool] = alloc(false)
            fun lp() : () = 
                if Equal(#0(syncVal), waitVal)
                then return()
                else do Pause() apply lp()
            do apply lp()
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#0(in_trans))
            then apply f(UNIT/exh)
            else 
                let stampPtr : ![stamp, int, int, long] = FLS.@get-key(STAMP_KEY / exh)
                do FLS.@set-key(FF_KEY, enum(0) / exh)
                cont enter() = 
                    let freq : int = FLS.@get-counter2()
                    do FLS.@set-counter(freq)
                    cont abortK(x:any) = BUMP_FABORT do #0(in_trans) := false throw enter()
                    let rs : RS.read_set = RS.@new(abortK)
                    do FLS.@set-key(READ_SET, rs / exh)  
                    do FLS.@set-key(WRITE_SET, RS.NilItem / exh)
                    let stamp : stamp = NoRecFF.@get-stamp(/exh)
                    do #0(stampPtr) := stamp
                    do #0(in_trans) := true
                    do FLS.@set-key(ABORT_KEY, abortK / exh)
                    cont transExh(e:exn) = 
                        do ccall M_Print("Warning: exception raised in transaction\n")
                        throw exh(e)
                    let res : any = apply f(UNIT/transExh)
                    do 
                        if(waitVal)
                        then 
                            if (#0(aborted))
                            then return()
                            else do #0(aborted) := true do #0(syncVal) := false apply lp()
                        else return()
                    do @commit(stampPtr / transExh)
                    do 
                        if(waitVal)
                        then return()
                        else do #0(syncVal) := true return()
                    let vp : vproc = host_vproc
                    do ccall M_PruneRemSetAll(vp, #3(stampPtr))
                    do #0(in_trans) := false
                    do FLS.@set-key(READ_SET, RS.NilItem / exh)
                    do FLS.@set-key(WRITE_SET, RS.NilItem / exh)
                    do FLS.@set-key(FF_KEY, enum(0) / exh)
                    return(res)
                throw enter()
        ;
	)

	type 'a tvar = 'a FullAbortSTM.tvar
    val get : 'a tvar -> 'a = _prim(@getMergeWS)
    val new : 'a -> 'a tvar = NoRecFFCounter.new
    val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val put : 'a tvar * 'a -> unit = _prim(@putMergeWS)
    val abort : unit -> 'a = NoRecFFCounter.abort
    val getRefCount : 'a tvar -> long = _prim(@get-ref-count)


    type sync_var = _prim(syncVar)
    val mkSyncVar : bool -> sync_var = _prim(@mk-sync-var)
    val atomicSync : (unit -> 'a) * sync_var * bool -> 'a = _prim(@atomic-sync)

(*)
    val getCtxt : 'a tvar * string -> 'a = _prim(@get-with-context)
*)
end














