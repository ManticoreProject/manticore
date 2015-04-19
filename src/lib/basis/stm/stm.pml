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

    val whichSTM = case getArg "-stm" args of SOME s => s | NONE => "bounded"

    val (get,put,atomic,new,printStats,abort,unsafeGet, same) = 
        if String.same(whichSTM, "bounded")
        then (BoundedHybridPartialSTM.get,BoundedHybridPartialSTM.put,      
              BoundedHybridPartialSTM.atomic,BoundedHybridPartialSTM.new,
              BoundedHybridPartialSTM.printStats,BoundedHybridPartialSTM.abort,
              BoundedHybridPartialSTM.unsafeGet, BoundedHybridPartialSTM.same)
        else if String.same(whichSTM, "full")
             then (FullAbortSTM.get,FullAbortSTM.put,FullAbortSTM.atomic,FullAbortSTM.new,FullAbortSTM.printStats,FullAbortSTM.abort,FullAbortSTM.unsafeGet, FullAbortSTM.same)
             else if String.same(whichSTM, "ff")
                  then (FFSTM.get, FFSTM.put, FFSTM.atomic, FFSTM.new, FFSTM.printStats, FFSTM.abort, FFSTM.unsafeGet, FFSTM.same)
                  else if String.same(whichSTM, "ordered")
                       then (OrderedSTM.get,OrderedSTM.put,OrderedSTM.atomic,OrderedSTM.new,OrderedSTM.printStats,OrderedSTM.abort,OrderedSTM.unsafeGet,OrderedSTM.same)
                       else (PartialSTM.get,PartialSTM.put,PartialSTM.atomic,PartialSTM.new,PartialSTM.printStats,PartialSTM.abort,PartialSTM.unsafeGet, PartialSTM.same)

    (*won't typecheck without these nonsense bindings*)
    val get : 'a tvar -> 'a = get
    val put : 'a tvar * 'a -> unit = put
    val atomic : (unit -> 'a) -> 'a = atomic
    val new : 'a -> 'a tvar = new
    val printStats : unit -> unit = printStats
    val abort : unit -> 'a = abort
    val unsafeGet : 'a tvar -> 'a = unsafeGet
    val same : 'a tvar * 'a tvar -> bool = same
end
