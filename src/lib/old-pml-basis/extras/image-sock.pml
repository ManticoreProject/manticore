(* image-sock.pml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * An interface for using the 3-channel color images as a rendering target over
 * a socket.
 *)

structure ImageSock =
  struct

    _primcode (
        extern int M_OpenSocket (void *, int);
        extern void M_CloseSocket (int);
        extern int M_ReadEvent (int);
        extern void M_WriteImage (int, void *);

	define inline @open (arg : [ml_string, ml_int] / exh : exh) : ml_int =
	  let host : any = String.@data(#0(arg) / exh)
          let result : int = ccall M_OpenSocket (host, #0(#1(arg)))
            return (alloc(result))
        ;

        define inline @close (arg : ml_int / exh : exh) : unit =
          let () = ccall M_CloseSocket (#0(arg))
	    return (UNIT)
	;
            
        define inline @read (arg : ml_int / exh : exh) : ml_int =
          let result : int = ccall M_ReadEvent (#0(arg))
	    return (alloc(result))
	;

        define inline @write (arg : [ml_int, any] / exh : exh) : unit =
          let () = ccall M_WriteImage (#0(#0(arg)), #1(arg))
            return (UNIT)
        ;
    )

    type socket = _prim (ml_int);

    val open : (string * int) -> socket = _prim(@open)
    val close : socket -> unit = _prim(@close)
    val read : socket -> int = _prim(@read)
    val write : (socket * Image.image) -> unit = _prim(@write)
  end
