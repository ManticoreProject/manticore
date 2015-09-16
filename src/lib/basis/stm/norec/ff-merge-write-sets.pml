structure NoRecMergeRSWriteSets = 
struct

#define READ_SET_BOUND 20

    structure RS = FFReadSetMergeWriteSets

    _primcode(
        define @init-count(x:unit / exh:exh) : ml_long = 
            let x : [long] = alloc(1:long)
            let x : [long] = promote(x)
            return(x);
    )

    val initCount : unit -> long = _prim(@init-count)
    val count = initCount()
    fun getCount() = count

	_primcode(
		(*I'm using ![any, long, long] as the type
		 * for tvars so that the typechecker will treat them
		 * as the same type as the other STM implementations.
		 * However, only the first element is ever used*)
		typedef tvar = ![any, long, long];
		typedef stamp = VClock.stamp;

        extern void M_PruneRemSetAll(void*, long, void*);

        define @get-count = getCount;

		define @new(x:any / exh:exh) : tvar =
            let c : [long] = @get-count(UNIT / exh)
            let c : ![long] = (![long]) c
            let id : long = I64FetchAndAdd(&0(c), 1:long)
			let tv : [any, long, long] = alloc(x, 0:long, id)
			let tv : [any, long, long] = promote(tv)
			let tv : tvar = (tvar) tv
			return(tv)
		;

		define inline @get-stamp(/exh:exh) : stamp = 
			fun stampLoop() : long = 
				let current : long = VClock.@get(/exh)
				let lastBit : long = I64AndB(current, 1:long)
				if I64Eq(lastBit, 0:long)
				then return(current)
				else do Pause() apply stampLoop()
			let stamp : stamp = apply stampLoop()
			return(stamp)
		;

		define @getFFNoRecCounter(tv : tvar / exh:exh) : any = 
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
            let writeSet : RS.witem = FLS.@get-key(WRITE_SET / exh)
            fun chkLog(writeSet : RS.witem) : Option.option = (*use local copy if available*)
                case writeSet
                   of RS.Write(tv':tvar, contents:any, tl:RS.witem) =>
                        if Equal(tv', tv)
                        then return(Option.SOME(contents))
                        else apply chkLog(tl)
                    | RS.NilItem => return (Option.NONE)
                end
            cont retK(x:any) = return(x)
            do  if I64Gt(#1(tv), 0:long)
                then RS.@fast-forward(readSet, writeSet, tv, retK, myStamp / exh)
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
                			do RS.@validate(readSet, myStamp / exh)
                			apply getLoop()
                	let current : any = apply getLoop()
                    let captureCount : int = FLS.@get-counter()
                    if I32Eq(captureCount, 0)
                    then
                        let kCount : long = RS.@getNumK(readSet)
                        if I64Lt(kCount, READ_SET_BOUND:long)
                        then
                            do RS.@insert-with-k(tv, current, retK, writeSet, readSet, myStamp / exh)
                            let captureFreq : int = FLS.@get-counter2()
                            do FLS.@set-counter(captureFreq)
                            return(current)
                        else
                            do RS.@filterRS(readSet, myStamp / exh)
                            do RS.@insert-with-k(tv, current, retK, writeSet, readSet, myStamp / exh)
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

		define @put(arg:[tvar, any] / exh:exh) : unit =
            let in_trans : [bool] = FLS.@get-key(IN_TRANS / exh)
            do if(#0(in_trans))
               then return()
               else do ccall M_Print("Trying to write outside a transaction!\n")
                    let e : exn = Fail(@"Writing outside transaction\n")
                    throw exh(e)
            let tv : tvar = #0(arg)
            let v : any = #1(arg)
            let writeSet : RS.witem = FLS.@get-key(WRITE_SET / exh)
            let newWS : RS.witem = RS.Write(tv, v, writeSet)
            do FLS.@set-key(WRITE_SET, newWS / exh)
            return(UNIT)
        ;

        define @commit(/exh:exh) : () =
        	let readSet : RS.read_set = FLS.@get-key(READ_SET / exh)
        	let writeSet : RS.witem = FLS.@get-key(WRITE_SET / exh)
        	let stamp : ![stamp, int, int, long] = FLS.@get-key(STAMP_KEY / exh)
        	let counter : ![long] = VClock.@get-boxed(/exh)
        	fun lockClock() : () = 
        		let current : stamp = #0(stamp)
        		let old : long = CAS(&0(counter), current, I64Add(current, 1:long))
        		if I64Eq(old, current)
        		then return()
        		else
        			do RS.@validate(readSet, stamp / exh)
        			apply lockClock()
        	do apply lockClock()
        	fun writeBack(ws:RS.witem) : () = 
        		case ws 
        		   of RS.NilItem => return()
        			| RS.Write(tv:tvar, x:any, next:RS.witem) => 
        				let x : any = promote(x)
        				do #0(tv) := x
        				apply writeBack(next)
        		end
            fun reverseWS(ws:RS.witem, new:RS.witem) : RS.witem = 
                case ws 
                   of RS.NilItem => return(new)
                    | RS.Write(tv:tvar, x:any, next:RS.witem) => apply reverseWS(next, RS.Write(tv, x, new))
                end
            let writeSet : RS.witem = apply reverseWS(writeSet, RS.NilItem)
        	do apply writeBack(writeSet)
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
                    let rs : RS.read_set = RS.@new()
                    do FLS.@set-key(READ_SET, rs / exh)  (*initialize STM log*)
                    do FLS.@set-key(WRITE_SET, RS.NilItem / exh)
                    let stamp : stamp = @get-stamp(/exh)
                    do #0(stampPtr) := stamp
                    do #0(in_trans) := true
                    cont abortK() = BUMP_FABORT do #0(in_trans) := false throw enter()
                    do FLS.@set-key(ABORT_KEY, abortK / exh)
                    cont transExh(e:exn) = 
                        do case e 
                           of Fail(s:ml_string) => do ccall M_Print(#0(s)) return()
                            | _ => return()   
                        end
                    	do ccall M_Print("Warning: exception raised in transaction\n")
                        throw exh(e)
                    let res : any = apply f(UNIT/transExh)
                    do @commit(/transExh)
                    let vp : vproc = host_vproc
                    do ccall M_PruneRemSetAll(vp, #3(stampPtr))
                    do #0(in_trans) := false
                    do FLS.@set-key(READ_SET, RS.NilItem / exh)
                    do FLS.@set-key(WRITE_SET, RS.NilItem / exh)
                    do FLS.@set-key(FF_KEY, enum(0) / exh)
                    return(res)
                throw enter()
      	;

      	define @print-stats(x:unit / exh:exh) : unit = 
            PRINT_PABORT_COUNT
	        PRINT_FABORT_COUNT
            PRINT_COMBINED
            PRINT_KCOUNT
            PRINT_FF
	        return(UNIT);

	    define @abort(x : unit / exh : exh) : any = 
            let readSet : RS.read_set = FLS.@get-key(READ_SET / exh)
            let oldFFInfo : RS.read_set = FLS.@get-key(FF_KEY / exh)
            do RS.@decCounts(oldFFInfo / exh)
            do RS.@incCounts(readSet, enum(0):any / exh)
            do FLS.@set-key(FF_KEY, readSet / exh)
            (*</FF>*)
            let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
            throw abortK()
        ;
         
      	define @tvar-eq(arg : [tvar, tvar] / exh : exh) : bool = 
	        if Equal(#0(arg), #1(arg))
	        then return(true)
	        else return(false);  

	    define @unsafe-get(x:tvar / exh:exh) : any = 
	    	return(#0(x));

        define @unsafe-put(arg : [tvar, any] / exh:exh) : unit = 
            let tv : tvar = #0(arg)
            let x : any = #1(arg)
            let x : any = promote(x)
            do #0(tv) := x
            return(UNIT)   
        ;

	)

	type 'a tvar = 'a PartialSTM.tvar
    val get : 'a tvar -> 'a = _prim(@getFFNoRecCounter)
    val new : 'a -> 'a tvar = _prim(@new)
    val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val printStats : unit -> unit = _prim(@print-stats)
    val abort : unit -> 'a = _prim(@abort)
    val same : 'a tvar * 'a tvar -> bool = _prim(@tvar-eq)
    val unsafeGet : 'a tvar -> 'a = _prim(@unsafe-get)
    val unsafePut : 'a tvar * 'a -> unit = _prim(@unsafe-put)

    val _ = Ref.set(STMs.stms, ("mergeWS", (get,put,atomic,new,printStats,abort,unsafeGet,same,unsafePut))::Ref.get STMs.stms)
end














