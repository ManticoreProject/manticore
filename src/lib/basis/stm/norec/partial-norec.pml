structure NoRecPartial = 
struct

#define READ_SET_BOUND 20

	(*flat representation for read and write sets*)
    datatype 'a item = Write of 'a * 'a * 'a | NilItem | WithK of 'a * 'a * 'a * 'a * 'a * 'a
                     | WithoutK of 'a * 'a * 'a | Abort of unit

	_primcode(
		(*I'm using ![any, long, long] as the type
		 * for tvars so that the typechecker will treat them
		 * as the same type as the other STM implementations.
		 * However, only the first element is ever used*)
		typedef tvar = ![any, long, long];
		typedef stamp = VClock.stamp;

        typedef mutWithK = 
            ![any,                  (*0: tag*)
              any,                  (*1: tvar read from*)
              any,                  (*2: contents read*)
              (*cont(any)*) any,    (*3: continuation (could be enum(0))*)
              item,                 (*4: write set*)
              item,                 (*5: next item*)
              item];                (*6: next continuation*)
              
		define @new(x:any / exh:exh) : tvar =
			let tv : [any] = alloc(x)
			let tv : [any] = promote(tv)
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

        define @validate(readSet : [int,item,item], startStamp:![stamp, int] / exh:exh) : () = 
            fun validateLoop(rs : item, abortInfo : item) : () =
                case rs 
                   of NilItem => (*finished validating*)
                        case abortInfo 
                           of NilItem => (*no violations*)
                                let currentTime : stamp = VClock.@get(/exh)
                                if I64Eq(currentTime, #0(startStamp))
                                then return() (*no one committed while validating*)
                                else  (*someone else committed, so revalidate*)
                                    let currentTime : stamp = @get-stamp(/exh)
                                    do #0(startStamp) := currentTime
                                    apply validateLoop(#1(readSet), NilItem)
                            | Abort(x : unit) => (*no checkpoint found*)
                                let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                                throw abortK()
                            | WithK(tv:tvar,x:any,abortK:cont(any),ws:item,_:item,_:item) => (*checkpoint found*)
                                fun getLoop() : any = 
                                    let t : long = VClock.@get(/exh)
                                    if I64Eq(t, #0(startStamp))
                                    then return(#0(tv))
                                    else
                                        let currentTime : long = @get-stamp(/exh)
                                        do #0(startStamp) := currentTime
                                        do apply validateLoop(abortInfo, NilItem)
                                        return(#0(tv))
                                let current : any = apply getLoop()
                                fun countKs(i:item, count:int) : int = 
                                    case i 
                                       of NilItem => return(count)
                                        | WithK(_:tvar,_:any,_:cont(any),_:item,_:item,nextC:item) => apply countKs(nextC, I32Add(count, 1))
                                    end
                                let i : int = apply countKs(abortInfo, 0)
                                let newRS : [int,item,item] = alloc(i, abortInfo, abortInfo)
                                do FLS.@set-key(READ_SET, newRS / exh)
                                do FLS.@set-key(WRITE_SET, ws / exh)
                                let captureFreq : int = FLS.@get-counter2()
                                do FLS.@set-counter(captureFreq)
                                BUMP_PABORT
                                throw abortK(current)
                        end
                    | WithoutK(tv:tvar, x:any, next:item) =>
                        if Equal(#0(tv), x)
                        then apply validateLoop(next, abortInfo)
                        else apply validateLoop(next, Abort(UNIT))
                    | WithK(tv:tvar,x:any,abortK:cont(any),ws:item,next:item,_:item) =>
                        if Equal(#0(tv), x)
                        then (*still valid*)
                            case abortInfo 
                               of NilItem => apply validateLoop(next, abortInfo) (*everything still valid so far*)
                                | Abort(x:unit) =>                               (*we need a checkpoint*)
                                    if Equal(abortK, enum(0))
                                    then apply validateLoop(next, abortInfo)     (*don't have one here*)
                                    else apply validateLoop(next, rs)            (*use this checkpoint*)
                                | _ => apply validateLoop(next, abortInfo)       (*already have a checkpoint*)
                            end
                        else
                            if Equal(abortK, enum(0))
                            then apply validateLoop(next, Abort(UNIT))
                            else apply validateLoop(next, rs)
                end
            let currentTime : stamp = @get-stamp(/exh)
            do #0(startStamp) := currentTime
            apply validateLoop(#1(readSet), NilItem)
        ;

		define @get(tv : tvar / exh:exh) : any = 
			let in_trans : [bool] = FLS.@get-key(IN_TRANS / exh)
            do 	
            	if(#0(in_trans))
               	then return()
               	else 
               		do ccall M_Print("Trying to read outside a transaction!\n")
                  	let e : exn = Fail(@"Reading outside transaction\n")
                    throw exh(e)
            let myStamp : ![stamp, int] = FLS.@get-key(STAMP_KEY / exh)
            let readSet : [int, item, item] = FLS.@get-key(READ_SET / exh)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            fun chkLog(writeSet : item) : Option.option = (*use local copy if available*)
                case writeSet
                   of Write(tv':tvar, contents:any, tl:item) =>
                        if Equal(tv', tv)
                        then return(Option.SOME(contents))
                        else apply chkLog(tl)                      
                    | NilItem => return (Option.NONE)
                end
            cont retK(x:any) = return(x)
            let localRes : Option.option = apply chkLog(writeSet)
            case localRes
               of Option.SOME(v:any) => return(v)
                | Option.NONE =>
                	fun getLoop() : any = 
                		let t : long = VClock.@get(/exh)
                		if I64Eq(t, #0(myStamp))
                		then return(#0(tv))
                		else
                			do @validate(readSet, myStamp / exh)
                			apply getLoop()
                	let current : any = apply getLoop()
                	let sl : item = #1(readSet)
                    if I32Lt(#0(readSet), READ_SET_BOUND)    (*still have room for more*)
                    then 
                        let captureCount : int = FLS.@get-counter()
                        if I32Eq(captureCount, 0)  (*capture a continuation*)
                        then 
                            let nextCont : item = #2(readSet)
                            let newSL : item = WithK(tv, current, retK, writeSet, sl, nextCont)
                            let captureFreq : int = FLS.@get-counter2()
                            do FLS.@set-counter(captureFreq)
                            let n : int = I32Add(#0(readSet), 1)  (*update number of conts*)
                            let newRS : [int, item, item] = alloc(n, newSL, newSL)
                            do FLS.@set-key(READ_SET, newRS / exh)
                            return(current)
                        else 
                            let n : int = #0(readSet)          (*don't capture cont*)
                            do FLS.@set-counter(I32Sub(captureCount, 1))
                            let nextCont : item = #2(readSet)
                            let newSL : item = WithoutK(tv, current, sl)
                            let newRS : [int,item,item] = alloc(n, newSL, nextCont)
                            do FLS.@set-key(READ_SET, newRS / exh)
                            return(current)
                    else 
                        fun dropKs(l:item, n:int) : int =   (*drop every other continuation*)
                            case l
                               of NilItem => return(n)
                                | WithK(_:tvar,_:any,_:cont(any),_:List.list,_:item,next:item) =>
                                    case next
                                       of NilItem => return(n)
                                        | WithK(_:tvar,_:any,_:cont(any),_:List.list,_:item,nextNext:item) =>
                                            (* NOTE: if compiled with -debug, this will generate warnings
                                             * that we are updating a bogus local pointer, however, given the
                                             * nature of the data structure, we do preserve the heap invariants*)
                                            let l : mutWithK = (mutWithK) l
                                            let next : mutWithK = (mutWithK) next
                                            do #3(next) := enum(0):any
                                            do #6(l) := nextNext
                                            apply dropKs(nextNext, I32Sub(n, 1))
                                    end
                             end
                          let nextCont : item = #2(readSet)
                          let n : int = apply dropKs(nextCont, #0(readSet))
                          let newSL : item = WithoutK(tv, current, sl)
                          let newRS : [int, item, item] = alloc(n, newSL, nextCont)
                          let captureFreq : int = FLS.@get-counter2()
                          let newFreq : int = I32Mul(captureFreq, 2)
                          do FLS.@set-counter(I32Sub(newFreq, 1))
                          do FLS.@set-counter2(newFreq)
                          do FLS.@set-key(READ_SET, newRS / exh)
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
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            let newWriteSet : item = Write(tv, v, writeSet)
            do FLS.@set-key(WRITE_SET, newWriteSet / exh)
            return(UNIT)
        ;

        define @commit(/exh:exh) : () =
        	let readSet : [int, item, item] = FLS.@get-key(READ_SET / exh)
        	let writeSet : item = FLS.@get-key(WRITE_SET / exh)
        	let stamp : ![stamp, int] = FLS.@get-key(STAMP_KEY / exh)
        	let counter : ![long] = VClock.@get-boxed(/exh)
        	fun lockClock() : () = 
        		let current : stamp = #0(stamp)
        		let old : long = CAS(&0(counter), current, I64Add(current, 1:long))
        		if I64Eq(old, current)
        		then return()
        		else
        			do @validate(readSet, stamp / exh)
        			apply lockClock()
        	do apply lockClock()
        	fun writeBack(ws:item) : () = 
        		case ws 
        		   of NilItem => return()
        			| Write(tv:tvar, x:any, next:item) => 
        				let x : any = promote(x)
        				do #0(tv) := x
        				apply writeBack(next)
        		end
            fun reverseWS(ws:item, new:item) : item = 
                case ws 
                   of NilItem => return(new)
                    | Write(tv:tvar, x:any, next:item) => apply reverseWS(next, Write(tv, x, new))
                end
            let writeSet : item = apply reverseWS(writeSet, NilItem)
        	do apply writeBack(writeSet)
        	do #0(counter) := I64Add(#0(stamp), 2:long) (*unlock clock*)
        	return()
        ;

		define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#0(in_trans))
            then apply f(UNIT/exh)
            else 
            	let stampPtr : ![stamp, int] = FLS.@get-key(STAMP_KEY / exh)
                cont enter() = 
                    do FLS.@set-key(READ_SET, alloc(0, NilItem, NilItem) / exh)  (*initialize STM log*)
                    do FLS.@set-key(WRITE_SET, NilItem / exh)
                    let stamp : stamp = @get-stamp(/exh)
                    do #0(stampPtr) := stamp
                    do #0(in_trans) := true
                    cont abortK() = BUMP_FABORT do #0(in_trans) := false throw enter()
                    do FLS.@set-key(ABORT_KEY, abortK / exh)
                    cont transExh(e:exn) = 
                    	do ccall M_Print("Warning: exception raised in transaction\n")
                        throw exh(e)
                    let res : any = apply f(UNIT/transExh)
                    do @commit(/transExh)
                    do #0(in_trans) := false
                    do FLS.@set-key(READ_SET, NilItem / exh)
                    do FLS.@set-key(WRITE_SET, NilItem / exh)
                    return(res)
                throw enter()
      	;

      	define @print-stats(x:unit / exh:exh) : unit = 
            PRINT_PABORT_COUNT
	        PRINT_FABORT_COUNT
            PRINT_COMBINED
	        return(UNIT);

	    define @abort(x : unit / exh : exh) : any = 
	        let e : cont() = FLS.@get-key(ABORT_KEY / exh)
	        throw e();
         
      	define @tvar-eq(arg : [tvar, tvar] / exh : exh) : bool = 
	        if Equal(#0(arg), #1(arg))
	        then return(true)
	        else return(false);  

	    define @unsafe-get(x:tvar / exh:exh) : any = 
	    	return(#0(x));

	)

	type 'a tvar = 'a PartialSTM.tvar
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = _prim(@new)
    val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val printStats : unit -> unit = _prim(@print-stats)
    val abort : unit -> 'a = _prim(@abort)
    val same : 'a tvar * 'a tvar -> bool = _prim(@tvar-eq)
    val unsafeGet : 'a tvar -> 'a = _prim(@unsafe-get)

    val _ = Ref.set(STMs.stms, ("pnorec", (get,put,atomic,new,printStats,abort,unsafeGet,same))::Ref.get STMs.stms)
end














