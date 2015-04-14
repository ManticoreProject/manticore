(* stm.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Software Transactional Memory with partial aborts and a bounded number of continuations
 * held in the log and fast forwarding.
 *)

structure FFSTM = 
struct 

#define COUNT 

#ifdef COUNT
#define BUMP_PABORT do ccall M_BumpCounter(0)
#define PRINT_PABORT_COUNT let counter1 : int = ccall M_SumCounter(0) \
                           do ccall M_Print_Int("Partial-Aborts = %d\n", counter1)
#define BUMP_FABORT do ccall M_BumpCounter(1)
#define PRINT_FABORT_COUNT let counter2 : int = ccall M_SumCounter(1) \
                           do ccall M_Print_Int("Full-Aborts = %d\n", counter2)                     
#define PRINT_COMBINED do ccall M_Print_Int("Total-Aborts = %d\n", I32Add(counter1, counter2))     
#define BUMP_KCOUNT do ccall M_BumpCounter(2)
#define PRINT_KCOUNT let counter1 : int = ccall M_SumCounter(2) \
                     do ccall M_Print_Int("Fast Forward Continuation Hits = %d\n", counter1)                                                                                                 
#else
#define BUMP_PABORT
#define PRINT_PABORT_COUNT
#define BUMP_FABORT
#define PRINT_FABORT_COUNT
#define PRINT_COMBINED 
#define BUMP_KCOUNT
#define PRINT_KCOUNT 
#endif


(*TODO:
    -add a 64-bit bloom filter to FLS that can be used for determining if a given
     tref is on the short path of the fast-forward read set
    -64 bits is sufficient given that we will never have more than 20 item on the short path
*)

	structure V = Vector

#define READ_SET_BOUND 20

    datatype 'a item = Write of 'a * 'a * 'a | NilItem | WithK of 'a * 'a * 'a * 'a * 'a
                     | WithoutK of 'a * 'a | Abort of unit 

    datatype 'a ffRes = FF of 'a * int | Done of 'a * int | NoFF

#define FastForward

    _primcode(

        extern void * M_Print_Int(void *, int);
        extern void * M_Print_Int2(void *, int, int);
        extern void M_Print_Long (void *, long);
        extern void M_BumpCounter(int);
        extern int M_SumCounter(int);
        extern long M_GetTimeAccum();
        extern int M_PolyEq(void * , void *);

        typedef stamp = VClock.stamp;
        typedef tvar = ![any, long, stamp]; (*contents, lock, version stamp*)
	
        typedef readItem = ![tvar,                  (*0: tvar operated on*)
                            (*cont(any)*) any,      (*1: abort continuation (enum(0) if no continuation)*)
                            any,                    (*2: write list*)
                            any,                    (*3: next read item*)
                            item,item];             (*4: next read item with a continuation*)
        
        define @new(x:any / exh:exh) : tvar = 
            let tv : tvar = alloc(x, 0:long, 0:long)
            let tv : tvar = promote(tv)
            return(tv)
        ;

        define @unsafe-get(tv : tvar / exh:exh) : any = 
            return(#0(tv));

		define @skip-list-to-vector(readSet : item, sentinel : item / exh : exh) : V.vector = 
			fun lp(rs : item, values : List.list, i : int) : V.vector = 
				if Equal(rs, sentinel)
				then let v : Vector.vector = V.@from-list-n-bom(i, values / exh)
					 return(v)	
				else case rs
						of WithK(tv:tvar,_:cont(any),_:item,_:item,next:item) =>
							apply lp(next, CONS(tv, values), I32Add(i, 1))
						 | NilItem => let v : Vector.vector = V.@from-list-n-bom(i, values / exh)
									  return(v)	
						 | _ => throw exh(Fail(@"skip-list-to-vector: Impossible!\n"))
					 end
			apply lp(readSet, nil, 0)
		;

				

        define @force-abort(rs : [int,item,item], startStamp:![stamp, int] / exh:exh) : () = 
            do #1(startStamp) := I32Add(#1(startStamp), 1)
            let rawStamp : stamp = #0(startStamp)
            fun faValidate(readSet:item, newStamp : stamp, abortInfo : item, i:int) : () = 
                case readSet
                    of NilItem => 
                        case abortInfo
                            of NilItem => 
                                (*Extend stamp*)
                                do #0(startStamp) := newStamp
                                return()
                             | Abort(x : unit) => 
                                let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                                do FLS.@set-key(FF_KEY, enum(0):any / exh)
                                throw abortK()
                             | WithK(tv:tvar,abortK:any,ws:item,_:item,_:item) =>
                                let abortK : cont(any) = (cont(any)) abortK
                                let current : any = #0(tv)
                                let stamp : stamp = #2(tv)
                                do if I64Eq(#1(tv), 0:long)
                                   then return()
                                   else let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                                        throw abortK()
                                do if I64Lt(rawStamp, stamp)
                                   then let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                                        throw abortK()
                                   else return()
                                let newRS : [int,item,item] = alloc(i, abortInfo, abortInfo)
                                do FLS.@set-key(READ_SET, newRS / exh)
                                do FLS.@set-key(WRITE_SET, ws / exh)
                                do #0(startStamp) := newStamp
                                let captureFreq : int = FLS.@get-counter2()
                                do FLS.@set-counter(captureFreq)
                                BUMP_PABORT
#ifdef FastForward
								let v : V.vector = @skip-list-to-vector(#2(rs), abortInfo / exh)
                                let ffInfo : [item,item,stamp,V.vector] = alloc(#2(rs), abortInfo, rawStamp,v)
                                do FLS.@set-key(FF_KEY, ffInfo / exh)
#endif
                                throw abortK(current)
                        end
                    | WithK(tv:tvar,k:any,ws:List.list,next:item,nextK:item) => 
                        let lock : stamp = #1(tv)
                        let stamp : stamp = #2(tv)
                        let shouldAbort : bool = if I64Eq(lock, 0:long) 
                                                 then if I64Lt(stamp, rawStamp) then return(false) else return(true)
                                                 else return(true)
                        if(shouldAbort)
                        then apply faValidate(next,newStamp,Abort(UNIT),0)
                        else case abortInfo
                                of Abort(x : unit) => 
                                    if Equal(k, enum(0))  (*we dropped this continuation*)
                                    then apply faValidate(next,newStamp,abortInfo,i)
                                    else apply faValidate(next,newStamp,readSet,0)
                                 | _ => if Equal(k, enum(0)) (*either a checkpointed item or not aborting*)
                                        then apply faValidate(next,newStamp,abortInfo,i)
                                        else apply faValidate(next,newStamp,abortInfo,I32Add(i,1))
                             end
                    | WithoutK(tv:tvar,rest:item) => 
                        let lock : stamp = #1(tv)
                        let stamp : stamp = #2(tv)
                        let shouldAbort : bool = if I64Eq(lock, 0:long) 
                                                 then if I64Lt(stamp, rawStamp) then return(false) else return(true)
                                                 else return(true)
                        if(shouldAbort)
                        then apply faValidate(rest,newStamp,Abort(UNIT),0)
                        else apply faValidate(rest,newStamp,abortInfo,i)
                end
            let stamp : stamp = VClock.@bump(/exh)         
            do apply faValidate(#1(rs),stamp,NilItem,0)
            return()
       ;

		define @checkVec(v:V.vector, tv:tvar) : bool = 
			fun lp(v:![any], i:int) : bool =
				if I32Gte(i, 0)
				then let tv' : tvar = ArrLoad(v, i)
					 if Equal(tv,tv')
				     then return(true)
					 else apply lp(v, I32Sub(i, 1))
				else return(false)
			let x : bool = apply lp(#0(v), #1(v))
			return(x)
		;

        define inline @fast-forward(tv : tvar, myStamp : ![stamp,int], readSet : [int, item, item], retK : cont(any), writeSet : item / exh:exh) : () = 
            let ffInfo : any = FLS.@get-key(FF_KEY / exh)
            let ffInfo : [item,item,stamp,V.vector] = ([item,item,stamp,V.vector]) ffInfo
            fun validate(rs:item, sentinel : item, bail:cont(), currentRS:item) : item = 
                if Equal(rs, sentinel)
                then return(currentRS)
                else case rs
                        of WithK(tv':tvar,k:cont(any),ws:List.list,next:item,_:item) =>
                            let stamp : stamp = #2(tv')
                            let lock : stamp = #1(tv')
                            if I64Eq(lock, 0:long) 
                            then if I64Lt(stamp, #2(ffInfo)) 
                                 then let res : item = apply validate(next, sentinel, bail, currentRS)
                                      if Equal(k, enum(0))
                                      then return(WithoutK(tv', res))
                                      else return(WithK(tv', k, ws, res, currentRS))
                                 else throw bail()
                            else throw bail()
                         | WithoutK(tv':tvar, next:item) =>
                            let stamp : stamp = #2(tv')
                            let lock : stamp = #1(tv')
                            if I64Eq(lock, 0:long)
                            then if I64Lt(stamp, #2(ffInfo))
                                 then let res : item = apply validate(next, sentinel, bail, currentRS)
                                      return(WithoutK(tv', res))
                                 else throw bail()
                            else throw bail()
                         | _ => let e : exn = Fail(@"fast-forward:validate:Impossible!\n")
                                throw exh(e)
                     end
            fun checkFF(rs:item, sentinel : item) : ffRes = 
                if Equal(rs, sentinel)
                then return(NoFF)
                else case rs
                        of WithK(tv':tvar,k:cont(any),ws:List.list,next:item,nextC:item) =>
                            if Equal(tv, tv')
                            then (*do ccall M_Print("TVars are equal\n") *)
                                 let res : int = ccall M_PolyEq(k, retK)
                                 if I32Eq(res, 1)
                                 then (*let res : int = ccall M_PolyEq(ws, writeSet) *)  (*polymorphic equality is probably overkill here*)
                                      BUMP_KCOUNT
                                      if Equal(ws, writeSet) (*I32Eq(res, 1) *)
                                      then let stamp : stamp = #2(tv')
                                           if I64Eq(#1(tv'), 0:long)
                                           then if I64Lt(stamp, #2(ffInfo))
                                                then 
                                                     return(FF(WithK(tv',k,ws,#1(readSet),#2(readSet)), 1))
                                                else return(NoFF)
                                           else return(NoFF)
                                      else let res : ffRes = apply checkFF(nextC, sentinel)  (*write sets don't match*)
                                           do ccall M_Print("Write sets do not match\n")
                                           case res
                                             of FF(currentRS:item,i:int) => 
                                                 cont bail() = return(Done(currentRS, i))
                                                 let newRS : item = apply validate(rs, nextC, bail, currentRS)
                                                 return(FF(newRS, I32Add(i, 1)))
                                              | _ => return(res)
                                           end
                                 else do ccall M_Print("Should be impossible!\n")
									  let res : ffRes = apply checkFF(nextC, sentinel)
                                      case res
                                        of FF(currentRS:item,i:int) => 
                                            cont bail() = return(Done(currentRS, i))
                                            let newRS : item = apply validate(rs, nextC, bail, currentRS)
                                            return(FF(newRS, I32Add(i, 1)))
                                         | _ => return(res)
                                      end
                            else let res : ffRes = apply checkFF(nextC, sentinel)
                                 case res
                                    of FF(currentRS:item, i:int) => 
                                        cont bail() = return(Done(currentRS, i))
                                        let newRS : item = apply validate(rs, nextC, bail, currentRS)
                                        return(FF(newRS, I32Add(i, 1)))
                                     | _ => return(res)
                                 end
                         | NilItem => return(NoFF)
                         | _ => throw exh(Fail(@"fast-forward:checkFF:Impossible!\n"))
                     end
            if Equal(ffInfo, enum(0))
            then return()
            else (*let res : bool = @checkVec(#3(ffInfo), tv)	
				 if(res)
				 then *)let x : ffRes = apply checkFF(#0(ffInfo), #1(ffInfo))
                      case x
                       of Done(newRS:item, i:int) =>
                             case newRS
                                 of WithK(tv':tvar,k:cont(any),ws:item,_:item,_:item) =>
                                      let current : any = #0(tv')
                                         let stamp : stamp = #2(tv')
                                      if I64Eq(#1(tv'), 0:long)
                                      then if I64Lt(stamp, #0(myStamp))
                                           then let newRS : [int,item,item] = alloc(I32Add(#0(readSet), i), newRS, newRS)
                                                do FLS.@set-key(READ_SET, newRS / exh)
                                                do FLS.@set-key(WRITE_SET, ws / exh)
                                                let captureFreq : int = FLS.@get-counter2()
                                                do FLS.@set-counter(captureFreq)
                                                do FLS.@set-key(FF_KEY, enum(0) / exh)
                                           (*     do ccall M_Print_Int("Done: Fast forwarding through %d checkpoints\n", i) *)
                                                throw k(current)
                                           else return()
                                      else return()
                                  | _ => let e : exn = Fail(@"fast forward: Impossible\n") throw exh(e)
                             end
                           | FF(newRS:item, i:int) =>
                              case newRS
                                 of WithK(tv':tvar,k:cont(any),ws:item,_:item,_:item) =>
                                      let current : any = #0(tv')
                                      let stamp : stamp = #2(tv')
                                      if I64Eq(#1(tv'), 0:long)
                                      then if I64Lt(stamp, #0(myStamp))
                                           then let newRS : [int,item,item] = alloc(I32Add(#0(readSet), i), newRS, newRS)
                                                do FLS.@set-key(READ_SET, newRS / exh)
                                                do FLS.@set-key(WRITE_SET, ws / exh)
                                                let captureFreq : int = FLS.@get-counter2()
                                                do FLS.@set-counter(captureFreq)
                                                do FLS.@set-key(FF_KEY, enum(0) / exh)
                                           (*     do ccall M_Print_Int("FF: Fast forwarding through %d checkpoints\n", i) *)
                                                throw k(current)
                                           else return()
                                      else return()
                                  | _ => let e : exn = Fail(@"fast forward: Impossible\n") throw exh(e)
                             end
                           | NoFF => return()
                      end  
				 (*else return() *)
        ;





        define @getABCDEFG(tv:tvar / exh:exh) : any = 
            let in_trans : [bool] = FLS.@get-key(IN_TRANS / exh)
            do if(#0(in_trans))
               then return()
               else do ccall M_Print("Trying to read outside a transaction!\n")
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
#ifdef FastForward
            do @fast-forward(tv, myStamp, readSet, retK, writeSet / exh)
#endif
            case localRes
                of Option.SOME(v:any) => return(v)
                 | Option.NONE => 
                     let current : any = 
                        fun getCurrentLoop() : any = 
                            let c : any = #0(tv)
                            let stamp : stamp = #2(tv)
                            if I64Eq(#1(tv), 0:long)
                            then if I64Lt(stamp, #0(myStamp))
                                 then return(c)
                                 else do @force-abort(readSet, myStamp / exh) (*if this returns, it updates myStamp*)
                                      apply getCurrentLoop()
                            else do Pause() apply getCurrentLoop()
                        apply getCurrentLoop()
                     let sl : item = #1(readSet)
                     if I32Lt(#0(readSet), READ_SET_BOUND)    (*still have room for more*)
                     then let captureCount : int = FLS.@get-counter()
                          if I32Eq(captureCount, 0)  (*capture a continuation*)
                          then let nextCont : item = #2(readSet)
                               let newSL : item = WithK(tv, retK, writeSet, sl, nextCont)
                               let captureFreq : int = FLS.@get-counter2()
                               do FLS.@set-counter(captureFreq)
                               let n : int = I32Add(#0(readSet), 1)  (*update number of conts*)
                               let newRS : [int, item, item] = alloc(n, newSL, newSL)
                               do FLS.@set-key(READ_SET, newRS / exh)
                               return(current)
                          else let n : int = #0(readSet)          (*don't capture cont*)
                               do FLS.@set-counter(I32Sub(captureCount, 1))
                               let nextCont : item = #2(readSet)
                               let newSL : item = WithoutK(tv, sl)
                               let newRS : [int,item,item] = alloc(n, newSL, nextCont)
                               do FLS.@set-key(READ_SET, newRS / exh)
                               return(current)
                     else fun dropKs(l:item, n:int) : int =   (*drop every other continuation*)
                              case l
                                of NilItem => return(n)
                                 | WithK(_:tvar,_:cont(any),_:List.list,_:item,next:item) =>
                                    case next
                                        of NilItem => return(n)
                                         | WithK(_:tvar,_:cont(any),_:List.list,_:item,nextNext:item) =>
                                            (* NOTE: if compiled with -debug, this will generate warnings
                                             * that we are updating a bogus local pointer, however, given the
                                             * nature of the data structure, we do preserve the heap invariants*)
                                            let l : readItem = (readItem) l
                                            let next : readItem = (readItem) next
                                            do #2(next) := enum(0):any
                                            do #5(l) := nextNext
                                            apply dropKs(nextNext, I32Sub(n, 1))
                                    end
                             end
                          let nextCont : item = #2(readSet)
                          let n : int = apply dropKs(nextCont, #0(readSet))
                          let newSL : item = WithoutK(tv, sl)
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
            let vp : vproc = SchedulerAction.@atomic-begin()
            let startStamp : ![stamp, int] = FLS.@get-key(STAMP_KEY / exh)
            do #1(startStamp) := I32Add(#1(startStamp), 1)
            fun release(locks : item) : () = 
                case locks 
                    of Write(tv:tvar, contents:any, tl:item) =>
                        do #1(tv) := 0:long         (*unlock*)
                        apply release(tl)
                     | NilItem => do SchedulerAction.@atomic-end(vp)
                                  return()
                end
            let rs : [int, item, item] = FLS.@get-key(READ_SET / exh)
            let readSet : item = #1(rs)
            let writeSet : item = FLS.@get-key(WRITE_SET / exh)
            let rawStamp: long = #0(startStamp)
            fun validate(readSet:item, locks:item, newStamp : stamp, abortInfo : item, i:int) : () = 
                case readSet
                    of NilItem => 
                        case abortInfo
                            of NilItem => do FLS.@set-key(FF_KEY, enum(0):any / exh)
                                          return() (*no violations detected*)
                             | WithK(tv:tvar,abortK:any,ws:item,_:item,_:item) =>
                                if Equal(abortK, enum(0))
                                then do apply release(locks)
                                     let abortK :cont() = FLS.@get-key(ABORT_KEY / exh)
                                     let captureFreq : int = FLS.@get-counter2()
                                     let newFreq : int = I32Div(captureFreq, 2)
                                     do if I32Eq(newFreq, 0)
                                        then return()
                                        else FLS.@set-counter2(1) 
#ifdef FastForward
									 let v : V.vector = @skip-list-to-vector(#2(rs), NilItem / exh)
                                     let ffInfo : [item, item, stamp,V.vector] = alloc(#2(rs), NilItem, rawStamp,v)
                                     do FLS.@set-key(FF_KEY, ffInfo / exh)
#endif
                                     throw abortK()  (*no checkpoint found*)
                                else do apply release(locks)
                                     let abortK : cont(any) = (cont(any)) abortK
                                     let current : any = #0(tv)
                                     let stamp : stamp = #2(tv)
                                     do if I64Eq(#1(tv), 0:long)
                                        then return()
                                        else let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                                            throw abortK()
                                     do if I64Lt(rawStamp, stamp)
                                        then let abortK : cont() = FLS.@get-key(ABORT_KEY / exh)
                                             throw abortK()
                                        else return()
                                     let newRS : [int,item,item] = alloc(i, abortInfo, abortInfo)
                                     do FLS.@set-key(READ_SET, newRS / exh)
                                     do FLS.@set-key(WRITE_SET, ws / exh)
                                     do #0(startStamp) := newStamp
                                     let captureFreq : int = FLS.@get-counter2() 
                                     do FLS.@set-counter(captureFreq)
                                     BUMP_PABORT
#ifdef FastForward
									 let v : V.vector = @skip-list-to-vector(#2(rs), abortInfo / exh)
                                     let ffInfo : [item,item,stamp,V.vector] = alloc(#2(rs), abortInfo,rawStamp,v)
                                     do FLS.@set-key(FF_KEY, ffInfo / exh)
#endif
                                     throw abortK(current) 
                             | WithoutK(tv:tvar,_:item) =>
                                do apply release(locks)
                                let abortK :cont() = FLS.@get-key(ABORT_KEY / exh)
                                let captureFreq : int = FLS.@get-counter2()
                                let newFreq : int = I32Div(captureFreq, 2)
                                do if I32Eq(newFreq, 0)
                                   then return()
                                   else FLS.@set-counter2(newFreq) 
#ifdef FastForward
								let v : V.vector = @skip-list-to-vector(#2(rs), NilItem / exh)
                                let ffInfo : [item, item, stamp,V.vector] = alloc(#2(rs), NilItem, rawStamp,v)
                                do FLS.@set-key(FF_KEY, ffInfo / exh)
#endif
                                throw abortK()  (*no checkpoint found*)
                        end                          
                    | WithK(tv:tvar,k:any,ws:List.list,next:item,nextK:item) => 
                        let stamp : stamp = #2(tv)
                        if I64Lt(rawStamp, stamp)
                        then if Equal(k, enum(0))
                             then apply validate(next,locks,newStamp,Abort(UNIT),0)
                             else apply validate(next,locks,newStamp,readSet,0)
                        else case abortInfo
                               of NilItem => apply validate(next, locks,newStamp,abortInfo,i) 
                                | WithK(_:tvar,k':any,_:List.list,_:item,_:item) =>  (*k is necessarily non-null*)
                                    if Equal(k,enum(0))
                                    then apply validate(next,locks,newStamp,abortInfo,i)
                                    else apply validate(next,locks,newStamp,abortInfo,I32Add(i,1)) 
                               | Abort(_ : unit) => 
                                    if Equal(k,enum(0))
                                    then apply validate(next,locks,newStamp,abortInfo,i)
                                    else apply validate(next,locks,newStamp,readSet,0) (*use this continuation*)
                               | _ => let e : exn = Fail(@"Impossible: validate\n")
                                      throw exh(e)
                             end
                    | WithoutK(tv:tvar,rest:item) => 
                        if I64Lt(#2(tv), rawStamp)
                        then apply validate(rest, locks,newStamp,abortInfo,i)
                        else apply validate(rest,locks,newStamp,Abort(UNIT),0)
                end
            fun acquire(ws:item, acquired : item) : item = 
                case ws
                    of Write(tv:tvar, contents:any, tl:item) =>
                        let casRes : long = CAS(&1(tv), 0:long, rawStamp) (*lock it*)
                        if I64Eq(casRes, 0:long)  (*locked for first time*)
                        then apply acquire(tl, Write(tv, contents, acquired))
                        else if I64Eq(casRes, rawStamp)    (*already locked it*)
                             then apply acquire(tl, acquired)
                             else (*release, but don't end atomic*)
                                  fun release(locks : item) : () = 
                                    case locks 
                                        of Write(tv:tvar, contents:any, tl:item) =>
                                            do #1(tv) := 0:long         (*unlock*)
                                            apply release(tl)
                                         | NilItem => return()
                                    end
                                  do apply release(acquired) 
                                  apply acquire(writeSet, NilItem)
                     |NilItem => return(acquired)
                end
            fun update(writes:item, newStamp : stamp) : () = 
                case writes
                    of Write(tv:tvar, newContents:any, tl:item) =>
                        let newContents : any = promote(newContents)
                        do #2(tv) := newStamp            (*update version stamp*)
                        do #0(tv) := newContents         (*update contents*)
                        do #1(tv) := 0:long              (*unlock*)
                        apply update(tl, newStamp)       (*update remaining*)
                     | NilItem => return()
                end
            let locks : item = apply acquire(writeSet, NilItem)
            let newStamp : stamp = VClock.@bump(/exh)        
            do apply validate(#1(rs),locks,newStamp,NilItem,0)
            do apply update(locks, newStamp)
            do SchedulerAction.@atomic-end(vp)
            do #1(startStamp) := I32Sub(#1(startStamp), 1)
            return()
        ;
        
        define @atomic(f:fun(unit / exh -> any) / exh:exh) : any = 
            let in_trans : ![bool] = FLS.@get-key(IN_TRANS / exh)
            if (#0(in_trans))
            then apply f(UNIT/exh)
            else let stampPtr : ![stamp, int] = FLS.@get-key(STAMP_KEY / exh)
                 do #1(stampPtr) := 0
                 cont enter() = 
                     do FLS.@set-key(READ_SET, alloc(0, NilItem, NilItem) / exh)  (*initialize STM log*)
                     do FLS.@set-key(WRITE_SET, NilItem / exh)
                     let stamp : stamp = VClock.@bump(/exh)
                     do #0(stampPtr) := stamp
                     do #0(in_trans) := true
                     cont abortK() = BUMP_FABORT do #0(in_trans) := false throw enter()
                     do FLS.@set-key(ABORT_KEY, abortK / exh)
                     cont transExh(e:exn) = 
                        do ccall M_Print("Warning: exception raised in transaction\n")
                        do @commit(/exh)  (*exception may have been raised because of inconsistent state*)
                        throw exh(e)
                     let res : any = apply f(UNIT/transExh)
                     do @commit(/transExh)
                     do #0(in_trans) := false
                     do FLS.@set-key(READ_SET, nil / exh)
                     do FLS.@set-key(WRITE_SET, NilItem / exh)
                     return(res)
                 throw enter()
      ;

      define @timeToString = Time.toString;
      
      define @print-stats(x:unit / exh:exh) : unit = 
        PRINT_PABORT_COUNT
        PRINT_FABORT_COUNT
        PRINT_COMBINED
        PRINT_KCOUNT
        return(UNIT);
        
      define @abort(x : unit / exh : exh) : any = 
         let e : cont() = FLS.@get-key(ABORT_KEY / exh)
         let stamp : ![stamp] = FLS.@get-key(STAMP_KEY / exh)
         let readSet : [int, item, item] = FLS.@get-key(READ_SET / exh)
#ifdef FastForward
		 let v : V.vector = @skip-list-to-vector(#2(readSet), NilItem / exh)
         let ffInfo : [item,item,stamp,V.vector] = alloc(#2(readSet), NilItem, #0(stamp),v)
         do FLS.@set-key(FF_KEY, ffInfo / exh)
#endif
         throw e();        

      define @tvar-eq(arg : [tvar, tvar] / exh : exh) : bool = 
         if Equal(#0(arg), #1(arg))
         then return(true)
         else return(false);

    define @print2(x : ml_string / exh:exh) : unit =
    do ccall M_Print(#0(x)) return (UNIT)
 (*       let ffInfo : any = FLS.@get-key(FF_KEY / exh)
        if Equal(ffInfo, enum(0))
        then return(UNIT)
        else do ccall M_Print(#0(x))
             return(UNIT)
   *) ;

         
    )

    	type 'a tvar = 'a PartialSTM.tvar
    	val atomic : (unit -> 'a) -> 'a = _prim(@atomic)
    val get : 'a tvar -> 'a = _prim(@getABCDEFG)
    val new : 'a -> 'a tvar = _prim(@new)
    val put : 'a tvar * 'a -> unit = _prim(@put)
    val printStats : unit -> unit = _prim(@print-stats)
    val abort : unit -> 'a = _prim(@abort)
    val unsafeGet : 'a tvar -> 'a = _prim(@unsafe-get)
    val same : 'a tvar * 'b tvar -> bool = _prim(@tvar-eq)

    val print2 : string -> unit = _prim(@print2)
    
end












 
