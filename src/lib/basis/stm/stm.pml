structure STM = 
struct
    type 'a tvar = 'a FullAbortSTM.tvar

    fun getArg f args = 
        case args 
            of arg::arg'::args => 
                if String.same(f, arg) then SOME arg'
                else getArg f (arg'::args)
	     | arg::nil => 
	       if String.same(f, arg) then SOME "" else NONE
             |_ => NONE

    val args = CommandLine.arguments ()

    val whichSTM = case getArg "-stm" args of SOME s => s | NONE => "bounded" (*use bounded partial abort by default*)

    fun usage() = 
	print "Valid STM implementations include:\n\
               bounded - bounded partial abort TL2\n\
               full - full abort TL2\n\
               orderedTL2 - bounded partial abort TL2 with an ordered read set\n\
               norec - full abort NOrec\n\
               orderedNoRec - bounded partial abort NOrec with an ordered read set\n\
               tiny - TinySTM (full abort)\n\
               ptiny - Partial Abort Tiny STM\n"

    val (getFunction,put,atomic,new,abort) =
	case whichSTM
	 of "bounded" => (BoundedHybridPartialSTM.get,     (*bounded continuations, but reverse-chron read set*)
			  BoundedHybridPartialSTM.put,
			  BoundedHybridPartialSTM.atomic,
			  BoundedHybridPartialSTM.new,
			  BoundedHybridPartialSTM.abort)
	  | "full" => (FullAbortSTM.get,                   (*baseline TL2*)
		       FullAbortSTM.put,
		       FullAbortSTM.atomic,
		       FullAbortSTM.new,
		       FullAbortSTM.abort)
	  | "orderedTL2" => (OrderedTL2.get,               (*Ordered read set TL2 (bounded continuations)*)
			     OrderedTL2.put,
			     OrderedTL2.atomic,
			     OrderedTL2.new,
			     OrderedTL2.abort)
	  | "norec" => (NoRecFull.get,
			NoRecFull.put,
			NoRecFull.atomic,
			NoRecFull.new,
			NoRecFull.abort)
	  | "pnorec" => (NoRecPartial.get,
			 NoRecPartial.put,
			 NoRecPartial.atomic,
			 NoRecPartial.new,
			 NoRecPartial.abort)
	  | "fullOrderedNOrec" => (NoRecFullOrdered.get,
				   NoRecFullOrdered.put,
				   NoRecFullOrdered.atomic,
				   NoRecFullOrdered.new,
				   NoRecFullOrdered.abort)
	  | "orderedNoRec" => (NoRecOrdered.get,
			       NoRecOrdered.put,
			       NoRecOrdered.atomic,
			       NoRecOrdered.new,
			       NoRecOrdered.abort) 
	  | "tiny" => (TinySTM.get,
		       TinySTM.put,
		       TinySTM.atomic,
		       TinySTM.new,
		       TinySTM.abort)
	  | "orderedFull" => (OrderedFullAbortTL2.get,
			      OrderedFullAbortTL2.put,
			      OrderedFullAbortTL2.atomic,
			      OrderedFullAbortTL2.new,
			      OrderedFullAbortTL2.abort)
	  | "ptiny" => (TinySTMPartial.get,
			TinySTMPartial.put,
			TinySTMPartial.atomic,
			TinySTMPartial.new,
			TinySTMPartial.abort)
	  |_ => (usage(); raise Fail "STM not recognized\n")

    (*won't typecheck without these nonsense bindings*)
    val get : 'a tvar -> 'a = getFunction
    val put : 'a tvar * 'a -> unit = put
    val atomic : (unit -> 'a) -> 'a = atomic
    val new : 'a -> 'a tvar = new
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

        define @print-stats(x:unit / exh:exh) : unit = 
            PRINT_PABORT_COUNT
            PRINT_FABORT_COUNT
            PRINT_COMBINED
            PRINT_KCOUNT
            PRINT_FF
            return(UNIT)
        ;

	define @get-paborts(x:unit / exh:exh) : [int] = 
	    let aborts : int = GET_PABORT_COUNT
	    let aborts : [int] = alloc(aborts)
            return(aborts);

	define @get-faborts(x:unit / exh:exh) : [int] = 
	    let aborts : int = GET_FABORT_COUNT
	    let aborts : [int] = alloc(aborts)
            return(aborts);

    ) 

    val getPartialAborts : unit -> int = _prim(@get-paborts)
    val getFullAborts : unit -> int = _prim(@get-faborts)

    val printStats : unit -> unit = _prim(@print-stats)

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
