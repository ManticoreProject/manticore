structure TextIO =
  struct

    _primcode (

      extern void* M_TextIOOpenIn (void*);
      extern void M_TextIOCloseIn (void*);
      extern void* M_TextIOInputLine (long) __attribute__((alloc));

      typedef instream = ml_long;

      define @open-in (str : ml_string / exh : exh) : instream =
	  let h : long = ccall M_TextIOOpenIn (str)
	  return (alloc (h))
	;

      define @close-in (ins : instream / exh : exh) : unit =
	  do ccall M_TextIOCloseIn (ins)
	  return (UNIT)
	;

      define @input-line (ins : instream / exh : exh) : (* ml_string *) Option.option =
	  let r : Option.option = ccall M_TextIOInputLine (#0(ins))
          return (r)
        ;

    )

    type instream = _prim (instream)

    val openIn : string -> instream = _prim (@open-in)
    val closeIn : instream -> unit = _prim (@close-in)
    val inputLine : instream -> string option = _prim (@input-line)

  end
