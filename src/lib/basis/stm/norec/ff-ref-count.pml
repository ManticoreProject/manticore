structure NoRecFFCounter = 
struct

#define READ_SET_BOUND 20

    structure RS = FFReadSetCounter

	_primcode(
		typedef tvar = ![any, long, long];
		typedef stamp = VClock.stamp;

        extern void M_PruneRemSetAll(void*, void*);

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
            let writeSet : RS.item = FLS.@get-key(WRITE_SET / exh)
            fun chkLog(writeSet : RS.item) : Option.option = (*use local copy if available*)
                case writeSet
                   of FFReadSet.Write(tv':tvar, contents:any, tl:RS.item) =>
                        if Equal(tv', tv)
                        then return(Option.SOME(contents))
                        else apply chkLog(tl)
                    | FFReadSet.NilItem => return (Option.NONE)
                end
            cont retK(x:any) = return(x)
            do  if I64Gt(#1(tv), 0:long)
                then RS.@fast-forward(readSet, writeSet, tv, retK, myStamp / exh)
                else return()
            let localRes : Option.option = apply chkLog(writeSet)
            case localRes
               of Option.SOME(v:any) => return(v)
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
                        let kCount : int = FFReadSet.@getNumK(readSet)
                        if I32Lt(kCount, READ_SET_BOUND)
                        then
                            do FFReadSet.@insert-with-k(tv, current, retK, writeSet, readSet, myStamp / exh)
                            let captureFreq : int = FLS.@get-counter2()
                            do FLS.@set-counter(captureFreq)
                            return(current)
                        else
                            do FFReadSet.@filterRS(readSet, myStamp / exh)
                            do FFReadSet.@insert-with-k(tv, current, retK, writeSet, readSet, myStamp / exh)
                            let captureFreq : int = FLS.@get-counter2()
                            let newFreq : int = I32Mul(captureFreq, 2)
                            do FLS.@set-counter(newFreq)
                            do FLS.@set-counter2(newFreq)
                            return(current)
                    else
                        do FLS.@set-counter(I32Sub(captureCount, 1))
                        do FFReadSet.@insert-without-k(tv, current, readSet, myStamp / exh)
                        return(current)
            end
		;

        define @commit(stamp : ![stamp, int, int, long] /exh:exh) : () =
        	do NoRecFF.@commit(stamp / exh)
            let ffInfo : RS.read_set =  FLS.@get-key(FF_KEY / exh)
            do RS.@decCounts(ffInfo / exh)
        	return()
        ;

	    define @abort(x : unit / exh : exh) : any = 
            let readSet : RS.read_set = FLS.@get-key(READ_SET / exh)
            let oldFFInfo : RS.read_set = FLS.@get-key(FF_KEY / exh)
            do RS.@decCounts(oldFFInfo / exh)
            do RS.@incCounts(readSet, FFReadSet.NilItem / exh)
            do FLS.@set-key(FF_KEY, readSet / exh)
            let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
            throw abortK()
        ;
	)

	type 'a tvar = 'a PartialSTM.tvar
    val get : 'a tvar -> 'a = _prim(@getFFNoRecCounter)
    val new : 'a -> 'a tvar = NoRecFF.new
    val atomic : (unit -> 'a) -> 'a = NoRecFF.atomic
    val put : 'a tvar * 'a -> unit = NoRecFF.put
    val printStats : unit -> unit = NoRecFF.printStats
    val abort : unit -> 'a = _prim(@abort)
    val same : 'a tvar * 'a tvar -> bool = NoRecFF.same
    val unsafeGet : 'a tvar -> 'a = NoRecFF.unsafeGet
    val unsafePut : 'a tvar * 'a -> unit = NoRecFF.unsafePut

    val _ = Ref.set(STMs.stms, ("ffRefCount", (get,put,atomic,new,printStats,abort,unsafeGet,same,unsafePut))::Ref.get STMs.stms)
end














