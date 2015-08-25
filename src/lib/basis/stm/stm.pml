structure STM = 
struct
    type 'a tvar = 'a PartialSTM.tvar

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

    val (getFunction,put,atomic,new,printStats,abort,unsafeGet,same,unsafePut) = getSTMFuns(Ref.get STMs.stms)

    (*won't typecheck without these nonsense bindings*)
    val get : 'a tvar -> 'a = getFunction
    val put : 'a tvar * 'a -> unit = put
    val atomic : (unit -> 'a) -> 'a = atomic
    val new : 'a -> 'a tvar = new
    val printStats : unit -> unit = printStats
    val abort : unit -> 'a = abort
    val unsafeGet : 'a tvar -> 'a = unsafeGet
    val same : 'a tvar * 'a tvar -> bool = same
    val unsafePut : 'a tvar * 'a -> unit = unsafePut 
end
