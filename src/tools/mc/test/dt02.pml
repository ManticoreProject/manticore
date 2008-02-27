(* dt02.pml -- test program for datatypes extracted from rt.pml *)

type vec = (double * double * double);

datatype Surfspec
  = Ambient of vec	(* all but specpow default to zero *)
  | Diffuse of vec
  | Specular of vec
  | Specpow of double	(* default 8. *)
  | Reflect of double
  | Transmit of double
  | Refract of double	(* default 1, like air == no refraction *)
  | Body of vec		(* body color, default 1.,1.,1. *)
  ;

fun refractsurf surf = (case surf
       of nil => 1.0
	| (Refract r :: ss) => r
	| (_ :: ss) => refractsurf ss
      (* end case *));

refractsurf (Refract 0.5 :: nil)

