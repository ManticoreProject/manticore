structure NoRecPartial = 
struct

#define READ_SET_BOUND 20

	(*flat representation for read and write sets*)
    datatype 'a item = Write of 'a * 'a * 'a | NilItem | WithK of 'a * 'a * 'a * 'a * 'a * 'a
                     | WithoutK of 'a * 'a * 'a | Abort of unit

    _primcode(
        define @mk-tag(x:unit / exh:exh) : any = 
            let x : item = WithoutK(enum(0):any, enum(0):any, enum(0):any)
            let x : [any] = ([any]) x
            return(#0(x));
    )

    val mkTag : unit -> 'a = _prim(@mk-tag)
    val tag = mkTag()
    fun getTag() = tag

	_primcode(
		typedef tvar = FullAbortSTM.tvar;
		typedef stamp = VClock.stamp;

        define @get-tag = getTag;

        typedef mutWithK = 
            ![enum(5),                  (*0: tag*)
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

        define @validate(readSet : [int,item,item], startStamp:![stamp, int] / exh:exh) : () = 
            fun validateLoopPNoRec(rs : item, abortInfo : item, kCount : int) : () =
                case rs 
                   of NilItem => (*finished validating*)
                        case abortInfo 
                           of NilItem => (*no violations*)
                                do FenceRead()
                                let currentTime : stamp = VClock.@get(/exh)
                                if I64Eq(currentTime, #0(startStamp))
                                then return() (*no one committed while validating*)
                                else  (*someone else committed, so revalidate*)
                                    let currentTime : stamp = NoRecFull.@get-stamp(/exh)
                                    do #0(startStamp) := currentTime
                                    apply validateLoopPNoRec(#1(readSet), NilItem, 0)
                            | Abort(x : unit) => (*no checkpoint found*)
                                let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                                throw abortK()
                            | WithK(tv:tvar,x:any,abortK:cont(any),ws:item,next:item,nextK:item) => (*checkpoint found*)
                                fun getLoop() : any = 
                                    let t : long = VClock.@get(/exh)
                                    if I64Eq(t, #0(startStamp))
                                    then return(#0(tv))
                                    else
                                        let currentTime : long = NoRecFull.@get-stamp(/exh)
                                        do #0(startStamp) := currentTime
                                        do apply validateLoopPNoRec(abortInfo, NilItem, 0)
                                        return(#0(tv))
                                let current : any = apply getLoop()
                                let newChkpnt : item = WithK(tv, current, abortK, ws, next,nextK)
                                let newRS : [int,item,item] = alloc(kCount, newChkpnt, newChkpnt)
                                do FLS.@set-key2(WRITE_SET, ws, READ_SET, newRS / exh)
                                let captureFreq : int = FLS.@get-counter2()
                                do FLS.@set-counter(captureFreq)
                                BUMP_PABORT
                                throw abortK(current)
                        end
                    | WithoutK(tv:tvar, x:any, next:item) =>
                        if Equal(#0(tv), x)
                        then apply validateLoopPNoRec(next, abortInfo, kCount)
                        else apply validateLoopPNoRec(next, Abort(UNIT), kCount)
                    | WithK(tv:tvar,x:any,abortK:cont(any),ws:item,next:item,_:item) =>
                        if Equal(#0(tv), x)
                        then (*still valid*)
                            case abortInfo 
                               of Abort(x:unit) => apply validateLoopPNoRec(next, rs, 1)                (*use this checkpoint*)
                                | _ => apply validateLoopPNoRec(next, abortInfo, I32Add(kCount, 1))     (*already have a checkpoint*)
                            end
                        else apply validateLoopPNoRec(next, rs, 1)
                end
            let currentTime : stamp = NoRecFull.@get-stamp(/exh)
            do #0(startStamp) := currentTime
            apply validateLoopPNoRec(#1(readSet), NilItem, 0)
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
                        let x : any = #0(tv)
                        do FenceRead()
                		let t : long = VClock.@get(/exh)
                		if I64Eq(t, #0(myStamp))
                		then return(x)
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
                                            let withoutKTag : enum(5) = @get-tag(UNIT/exh)
                                            let l : mutWithK = (mutWithK) l
                                            let next : mutWithK = (mutWithK) next
                                            do #0(next) := withoutKTag
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
            do FenceRead()
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
                    let stamp : stamp = NoRecFull.@get-stamp(/exh)
                    do #0(stampPtr) := stamp
                    do #0(in_trans) := true
                    cont abortK() = BUMP_FABORT throw enter()
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
    val get : 'a tvar -> 'a = _prim(@get)
    val new : 'a -> 'a tvar = _prim(@new)
    val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val abort : unit -> 'a = _prim(@abort)
   
end














