
fun getArg f args = 
    case args 
        of arg::arg'::args => 
            if String.same(f, arg) then SOME arg'
            else getArg f (arg'::args)
         |_ => NONE

val args = CommandLine.arguments ()

fun getIntFlg f dflt = 
    case getArg f args
        of SOME n => (case Int.fromString n of SOME n => n | NONE => dflt)
         | NONE => dflt

val N = getIntFlg "-n" 100
val iters = getIntFlg "-iters" 1000
val trefs = Vector.tabulate(N, fn i => STM.new 0)
val refs = Vector.tabulate(N, fn i => Ref.new 0)

val switch = Ref.new true
val chan = PrimChan.new()

datatype msg = AbortMe | Die | Continue

fun msg i = ()(*print ("Working on iteration " ^ Int.toString i ^ "\n")*)

fun bump i = ()(*
    let val r = Vector.sub(refs, i)
	val _ = Ref.set(r, Ref.get r + 1)
	val _ = msg i
    in () end*)

fun tx(i, sum, txNum) = 
    if i = Vector.length trefs
    then 
	let val _ = PrimChan.send(chan, AbortMe)
	    val _ = PrimChan.recv chan
	in sum end
    else 
	let val _ = bump i
	in tx(i+1, sum + STM.get (Vector.sub(trefs, i)), txNum) end
	    
		
			    
fun txLoop i = 
	if i = 0
	then PrimChan.send(chan, Die)
	else (STM.atomic(fn () => tx(0, 0, i)); txLoop(i-1))

fun txLoop2() = 
    case PrimChan.recv chan 
     of Die => ()
      | AbortMe => 
	if Ref.get switch
	then 
	    let fun tx() = 
		    let val tvar = Vector.sub(trefs, N div 2)
			val _ = STM.put(tvar, STM.get tvar)
		    in () end
		val _ = STM.atomic tx
		val _ = Ref.set(switch, false)
		val _ = PrimChan.send(chan, Continue) 
	    in txLoop2() end
	else 
	    let val _ = Ref.set(switch, true)
		val _ =  PrimChan.send(chan, Continue) 
	    in txLoop2() end


			  
			  

val _ = Threads.spawnOn(1, txLoop2)




val startTime = Time.now()
val _ = txLoop iters
val endTime = Time.now()
val _ = print ("Execution-Time = " ^ Time.toString (endTime - startTime) ^ "\n")

(*
val _ = Vector.app (fn i => print(Int.toString(Ref.get i) ^ ", ")) refs

val _ = print "\n"
*)


