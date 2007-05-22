module Hi
  extern void print (void *); 
  fun init (arg : [int]; k : cont(enum(1)), exh : cont(any)) =

      fun doit (arg : any; k : cont(enum(1)), exh : cont(any)) =
	let () = ccall print ("hello world\n\000")
	throw k (1)

      apply doit (arg; k, exh)

