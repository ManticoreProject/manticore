structure STM = 
struct
    type 'a tvar = 'a FullAbortSTM.tvar

    fun getArg f args = 
        case args 
            of arg::arg'::args => 
                if String.same(f, arg) then SOME arg'
                else getArg f (arg'::args)
             |_ => NONE

    val args = CommandLine.arguments ()

    val whichSTM = case getArg "-stm" args of SOME s => s | NONE => "bounded" (*use bounded partial abort by default*)

    fun getSTMFuns l =
        case l 
           of (str, funs)::tl => if String.same(str, whichSTM) then funs else getSTMFuns tl
            | nil => (print "STM implementation not recognized\n"; raise Fail(""))

    val (getFunction,put,atomic,new,printStats,abort) = getSTMFuns(Ref.get STMs.stms)

    (*won't typecheck without these nonsense bindings*)
    val get : 'a tvar -> 'a = getFunction
    val put : 'a tvar * 'a -> unit = put
    val atomic : (unit -> 'a) -> 'a = atomic
    val new : 'a -> 'a tvar = new
    val printStats : unit -> unit = printStats
    val abort : unit -> 'a = abort

    _primcode(
        define @post-start-tx-w-msg(msg : [long] / exh : exh) : unit = 
            do Logging.@log-start-tx-w-msg(#0(msg))
            return(UNIT);
 
        (*high, mid, low bits*)
        define @mk-tx-msg(arg : [ml_int, ml_int, ml_int] / exh : exh) : ml_long = 
            let v1 : long = I64LSh(I32ToI64(#0(#0(arg))), 34:long)
            let v2 : long = I64LSh(I32ToI64(#0(#1(arg))), 4:long)
            let v3 : long = I64OrB(v1, v2)
            let v4 : long = I64OrB(v3, I32ToI64(#0(#2(arg))))
            let v5 : ml_long = alloc(v4)
            return(v5)
        ;
            
        extern long M_ToggleAbort();

        define @toggle-abort(x:unit / exh:exh) : bool = 
            let x : bool = ccall M_ToggleAbort()
            return(x);

        define @same-tvar(arg : [FullAbortSTM.tvar, FullAbortSTM.tvar] / exh:exh) : bool = 
            if Equal(#0(arg), #1(arg))
            then return(true)
            else return(false)
        ;

        define @unsafe-put(arg : [FullAbortSTM.tvar, any] / exh:exh) : unit = 
            let v : any = #1(arg)
            let v : any = promote(v)
            let tv : FullAbortSTM.tvar = #0(arg)
            do #0(tv) := v
            return(UNIT)
        ;

        define @unsafe-get(tvar : FullAbortSTM.tvar / exh:exh) : any = 
            return(#0(tvar))
        ;

    ) 

    val same : 'a tvar * 'a tvar -> bool = _prim(@same-tvar)
    val unsafePut : 'a tvar * 'a -> unit = _prim(@unsafe-put)
    val unsafeGet : 'a tvar -> 'a = _prim(@unsafe-get)

    val mkTXMsg : int * int * int -> long = _prim(@mk-tx-msg)
    val postStartTXWMsg : long -> unit = _prim(@post-start-tx-w-msg)

    val flipBit : unit -> bool = _prim(@toggle-abort)
    fun toggleAbort() = 
        if flipBit()
        then abort()
        else ()

    fun atomic' (f, msg) = 
	let val _ = postStartTXWMsg msg
	    val res = atomic f
	    val _ = Logging.postCommitTX()
	in res end

end
