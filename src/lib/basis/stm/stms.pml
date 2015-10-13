structure STMs = 
struct

	_primcode(
		extern void M_IncCounter(void*, int, int);
		extern int M_PolyEq(void*, void*);
	)

	val stms = Ref.new []
end