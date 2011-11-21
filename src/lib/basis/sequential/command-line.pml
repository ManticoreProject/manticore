(* command-line.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CommandLine (* : sig

    val name : unit -> string
    val arguments : unit -> string list

  end*) = struct

    fun name () = "<unknown>" (* FIXME *)

    _primcode (
      extern void *M_Arguments () __attribute__((alloc,pure));

      define @arguments (_ : unit / _ : exh) : list =
	  let args : list = ccall M_Arguments()
	    return (args)
	;
    )

    val arguments : unit -> string list = _prim(@arguments)

  end

