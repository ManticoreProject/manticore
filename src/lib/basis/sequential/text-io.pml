structure TextIO =
  struct

    _primcode (

      extern void* M_TextIOOpenIn (void*);
      extern void M_TextIOCloseIn (void*);
      extern void* M_TextIOInputLine (long) __attribute__((alloc));

      extern void* M_TextIOOpenOut (void*);
      extern void M_TextIOCloseOut (void*);
      extern void M_TextIOOutput (void*, void*);
      extern void M_TextIOOutputLine (void*, void*);

      typedef instream = ml_long;

      define @open-in (str : ml_string / exh : exh) : (* instream *) Option.option =
	  let h : Option.option = ccall M_TextIOOpenIn (str)
	  return (h)
	;

      define @close-in (ins : instream / exh : exh) : unit =
	  do ccall M_TextIOCloseIn (#0(ins))
	  return (UNIT)
	;

      define @input-line (ins : instream / exh : exh) : (* ml_string *) Option.option =
	  let r : Option.option = ccall M_TextIOInputLine (#0(ins))
          return (r)
        ;

      typedef outstream = ml_long;

      define @open-out (fname : ml_string / exh : exh) : outstream =
	  let h : long = ccall M_TextIOOpenOut (fname)
	  return (alloc (h))
	;

      define @close-out (outs : outstream / exh : exh) : unit =
	  do ccall M_TextIOCloseOut (#0(outs))
	  return (UNIT)
	;

      define @output (arg: [outstream, ml_string] / exh : exh) : unit =
	  do ccall M_TextIOOutput (#0(#0(arg)), #1(arg))
	  return (UNIT)
	;

      define @output-line ( arg: [ml_string, outstream] / exh : exh) : unit =
	  do ccall M_TextIOOutputLine (#0(arg), #0(#1(arg)))
	  return (UNIT)
	;

    )

    type instream = _prim (instream)

    val rawOpenIn : string -> instream option = _prim (@open-in)
    fun openIn (str) =
        case (rawOpenIn str)
         of SOME h => h
          | NONE => Debug.failwith (String.concat ["Failed to open file: ",
                                                   str])

    val closeIn : instream -> unit = _prim (@close-in)
    val inputLine : instream -> string option = _prim (@input-line)

    type outstream = _prim (outstream)

    val openOut : string -> outstream = _prim (@open-out)
    val closeOut : outstream -> unit = _prim (@close-out)
    val output : outstream * string -> unit = _prim (@output)
    val outputLine : string * outstream -> unit = _prim (@output-line)

  end
