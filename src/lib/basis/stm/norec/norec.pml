structure NoRecFull = 
struct

	(*flat representation for read and write sets*)
    datatype 'a item = Read of 'a * 'a * 'a | Write of 'a * 'a * 'a | NilItem

	_primcode(
		typedef tvar = FullAbortSTM.tvar;
		typedef stamp = VClock.stamp;

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

		define @validate(readSet : item, stamp : ![stamp, int] / exh:exh) : () = 
			fun validateLoop(rs : item) : () = 
				case rs 
				   of NilItem => 
				   		let currentTime : stamp = VClock.@get(/exh)
				   		if I64Eq(currentTime, #0(stamp))
				   		then return()
				   		else 
				   			let currentTime : stamp = @get-stamp(/exh)
				   			do #0(stamp) := currentTime
				   			apply validateLoop(readSet)
					| Read(tv:tvar, x:any, next:item) => 
						if Equal(#0(tv), x)
						then apply validateLoop(next)
						else
							let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
							throw abortK()
				end
			let currentTime : stamp = @get-stamp(/exh)
			do #0(stamp) := currentTime
			apply validateLoop(readSet)
		;

		define @getFullAbortNoRec(tv : tvar / exh:exh) : any = 
			let in_trans : [bool] = FLS.@get-key(IN_TRANS / exh)
            do 	
            	if(#0(in_trans))
               	then return()
               	else 
               		do ccall M_Print("Trying to read outside a transaction!\n")
                  	let e : exn = Fail(@"Reading outside transaction\n")
                    throw exh(e)
            let myStamp : ![stamp, int] = FLS.@get-key(STAMP_KEY / exh)
            let readSet : item = FLS.@get-key(READ_SET / exh)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            fun chkLog(writeSet : item) : Option.option = (*use local copy if available*)
                case writeSet
                   of Write(tv':tvar, contents:any, tl:item) =>
                        if Equal(tv', tv)
                        then return(Option.SOME(contents))
                        else apply chkLog(tl)                      
                    | NilItem => return (Option.NONE)
                end
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
                	let newRS : item = Read(tv, current, readSet)
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
        	let readSet : item = FLS.@get-key(READ_SET / exh)
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
                    do FLS.@set-key(READ_SET, NilItem / exh)  (*initialize STM log*)
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

	    define @abort(x : unit / exh : exh) : any = 
	        let e : cont() = FLS.@get-key(ABORT_KEY / exh)
	        throw e();

	)

	type 'a tvar = 'a FullAbortSTM.tvar
    val get : 'a tvar -> 'a = _prim(@getFullAbortNoRec)
    val new : 'a -> 'a tvar = _prim(@new)
    val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val abort : unit -> 'a = _prim(@abort)

    val _ = Ref.set(STMs.stms, ("norec", (get,put,atomic,new,abort))::Ref.get STMs.stms)
end














