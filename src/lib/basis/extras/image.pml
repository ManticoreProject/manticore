(* image.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * An interface for generating and outputing 3-channel color images (the output
 * format is PPM).
 *)

structure Image =
  struct

    _primcode (
	extern void *M_NewImage (int, int) __attribute__((pure));
	extern void M_FreeImage (void *);
	extern void M_OutputImage (void *, void *);
	extern void M_UpdateImage3f (void *, int, int, float, float, float);
	extern void M_UpdateImage3d (void *, int, int, double, double, double);

	define inline @new (arg : [ml_int, ml_int] / exh : exh) : any =
	  let img : any = ccall M_NewImage (unwrap(#0(arg)), unwrap(#1(arg)))
	    return (img)
	;
	
	define inline @free (img : any / exh : exh) : unit =
	  let () = ccall M_FreeImage (img)
	    return (UNIT)
	;
	
	define inline @output (arg : [ml_string, any] / exh : exh) : unit =
	  let file : any = String.@data(#0(arg) / exh)
	  let () = ccall M_OutputImage (#1(arg), file)
	    return (UNIT)
	;
	
	define inline @update3f (arg : [any, ml_int, ml_int, ml_float, ml_float, ml_float] / exh : exh) : unit =
	  let () = ccall M_UpdateImage3f (#0(arg), unwrap(#1(arg)), unwrap(#2(arg)), unwrap(#3(arg)), unwrap(#4(arg)), unwrap(#5(arg)))
	    return (UNIT)
	;
	
	define inline @update3d (arg : [any, ml_int, ml_int, ml_double, ml_double, ml_double] / exh : exh) : unit =
	  let () = ccall M_UpdateImage3d (#0(arg), unwrap(#1(arg)), unwrap(#2(arg)), unwrap(#3(arg)), unwrap(#4(arg)), unwrap(#5(arg)))
	    return (UNIT)
	;
    )

    type image = _prim (any);

(* new : width * height -> image *)
    val new : (int * int) -> image = _prim(@new)

    val free : image -> unit = _prim(@free)
    val output : (string * image) -> unit = _prim(@output)
    val update3f : (image * int * int * float * float * float) -> unit = _prim(@update3f)
    val update3d : (image * int * int * double * double * double) -> unit = _prim(@update3d)

  end
