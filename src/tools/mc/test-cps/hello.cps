module Hi
  extern void print (void *);
  fun init (arg : any, k : cont(enum(1)), exh : cont(any)) =
	let () = ccall print ("hello world\n")
	throw k (1)

