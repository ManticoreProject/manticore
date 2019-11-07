(*********** some compatibility and helpers for PML **********)


val concat = String.concat

structure U = struct

  fun first2 (x, _) = x
  fun second2 (_, x) = x

  val posInf : double = 2147483648.0

end

(*********************)

structure Rand =
  struct

    val seed = Word64.fromInt 1234567
    (* parameters of the linear congruential generator, chosen to match
      "Numerical Recipes" (source: Wikipedia) *)
    val multiplier = Word64.fromInt 1664525
    val increment = Word64.fromInt 1013904223
    val one = Word64.fromInt 1
    val modulusMask = Word64.sub(Word64.lsh(one, Word64.fromInt 32), one)

    val state : Word64.word Ref.ref = Ref.new seed

    (* fun init 0w0 = (state := 0w1234567)
      | init w = (state := w) *)

(*
    fun randWord32 () = let
	  val r = (Word64.fromLarge (Word32.toLarge (!state)) * 0w48271) mod 0wx7fffffff
	  val r' = Word32.fromLarge (Word64.toLarge r)
	  in
	    state := r';
	    r'
	  end
    *)

    (* range of values is [0, 2^32-1] *)
    fun genrand32 () = let
      val x = Ref.get state
      val x = Word64.mul(x, multiplier)
      val x = Word64.add(x, increment)
      val x = Word64.andb(x, modulusMask)
    in
      (Ref.set(state, x); x)
    end

    val scale : double = 1.0 / 4294967295.0

    fun rand () = scale * Double.fromLong (Word64.toLong (genrand32 ()))

    (* fun randInt n = if (n <= 1)
	  then 1
	  else Word32.toIntX(Word32.mod(genrand32(), Word32.fromInt n)) *)

  end
(* interval.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Interval (* : sig

    type t = (double * double)

    val within : double * t -> bool

    val toString : t -> string

  end *) = struct

    type t = (double * double)

    fun within (t, (min, max) : t) = (min <= t) andalso (t <= max)

    fun toString ((min, max) : t) = String.concat[
	    "[", Double.toString min, " .. ", Double.toString max, "]"
	  ]

  end
(* rgb.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure RGB (* : sig

    type t = (double * double * double)

    val add : (t * t) -> t
    val adds : (t * double * t) -> t
    val modulate : (t * t) -> t

    val scale : (double * t) -> t

  (* lerp (u, t, v) = (1-t)*u + t*v; we assume that 0 <= t <= 1 *)
    val lerp : (t * double * t) -> t

  (* standard colors *)
    val black : t
    val red : t
    val green : t
    val blue : t
    val white : t
    val gray : double -> t

  end *) = struct

    type t = (double * double * double)

    fun add ((r1, g1, b1) : t, (r2, g2, b2)) = (r1 + r2, g1 + g2, b1 + b2)
    fun adds ((r1, g1, b1) : t, s, (r2, g2, b2)) = (r1 + s*r2, g1 + s*g2, b1 + s*b2)
    fun modulate ((r1, g1, b1) : t, (r2, g2, b2)) = (r1 * r2, g1 * g2, b1 * b2)

    fun scale (s, (r, g, b) : t) = (s*r, s*g, s*b)

    fun lerp (c1, t, c2) = add (scale(1.0 - t, c1), scale(t, c2))

  (* standard colors *)
    val black : t = (0.0, 0.0, 0.0)
    val red : t = (1.0, 0.0, 0.0)
    val green : t = (0.0, 1.0, 0.0)
    val blue : t = (0.0, 0.0, 1.0)
    val white : t = (1.0, 1.0, 1.0)
    fun gray v = (v, v, v)

  end

(* color.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Color (* : sig

    type t = Word8.word * Word8.word * Word8.word

    val fromRGB : RGB.t -> t

  (* convert an RGB value to an image color value with a gamma correction of 1/2 *)
    val fromRGBWithGamma : RGB.t -> t

  end *) = struct

    type t = int * int * int

    fun toByte (f : double) = let
	  val f' = Double.intFloor (f * 255.99)
	  in
	    if (f' <= 0) then 0
	    else if (255 <= f') then 255
	    else f'
	  end

    fun fromRGB ((r, g, b) : RGB.t) = (toByte r, toByte g, toByte b)

    fun fromRGBWithGamma ((r, g, b) : RGB.t) = let
	  fun cvt f = toByte (Double.sqrt f)
	  in
	    (cvt r, cvt g, cvt b)
	  end

  end
(* vec3-sig.sml
 *
 * COPYRIGHT (c) 2012 The SML3d Project (http://sml3d.cs.uchicago.edu)
 * All rights reserved.
 *)

signature VEC3 =
  sig

    type t = (double * double * double)

    val toString : t -> string

  (* zero vector *)
    val zero : t

  (* vector arithmetic *)
    val negate : t -> t
    val add : (t * t) -> t
    val sub : (t * t) -> t
    val mul : (t * t) -> t

    val scale : (double * t) -> t

  (* adds (u, s, v) = u + s*v *)
    val adds : (t * double * t) -> t

  (* lerp (u, t, v) = (1-t)*u + t*v; we assume that 0 <= t <= 1 *)
    val lerp : (t * double * t) -> t

    val dot : (t * t) -> double

    val normalize : t -> t

    val length : t -> double

  (* cross product *)
    val cross : t * t -> t

  (* iterators *)
    val app  : (double -> unit) -> t -> unit
    val map  : (double -> 'a) -> t -> ('a * 'a * 'a)

  (* graphics related functions *)

  (* `reflect (v, n)` reflects `v` around the normal vector `n` *)
    val reflect : (t * t) -> t

    val rotateX : double -> t -> t
    val rotateY : double -> t -> t
    val rotateZ : double -> t -> t

    val randomPointInSphere : unit -> t

  end
(* vec3.sml
 *
 * COPYRIGHT (c) 2012 The SML3d Project (http://sml3d.cs.uchicago.edu)
 * All rights reserved.
 *
 * Operations on vectors in R^3 (scalar version)
 *)

structure Vec3 =
  struct

    val epsilon = 0.0001

    type t = (double * double * double)

    fun toString ((x, y, z) : t) = concat[
	    "<", Double.toString x, ",", Double.toString y, ",", Double.toString z, ">"
	  ]

    val zero : t = (0.0, 0.0, 0.0)

    val e1 : t = (1.0, 0.0, 0.0)
    val e2 : t = (0.0, 1.0, 0.0)
    val e3 : t = (0.0, 0.0, 1.0)

    fun negate ((x, y, z) : t) = (~x, ~y, ~z)

    fun add ((x1, y1, z1) : t, (x2, y2, z2)) = (x1+x2, y1+y2, z1+z2)

    fun sub ((x1, y1, z1) : t, (x2, y2, z2)) = (x1-x2, y1-y2, z1-z2)

    fun mul ((x1, y1, z1) : t, (x2, y2, z2)) = (x1*x2, y1*y2, z1*z2)

    fun scale (s, (x, y, z) : t) = (s*x, s*y, s*z)

    fun adds (v1, s, v2) = add(v1, scale(s, v2))

    fun dot ((x1, y1, z1) : t, (x2, y2, z2)) = (x1*x2 + y1*y2 +z1*z2)

    fun lerp (v1, t, v2) = adds (scale(1.0 - t, v1), t, v2)

    fun cross ((x1, y1, z1) : t, (x2, y2, z2)) = (
	    y1*z2 - z1*y2,
	    z1*x2 - x1*z2,
	    x1*y2 - y1*x2
	  )

    fun lengthSq v = dot(v, v)
    fun length v = Double.sqrt(lengthSq v)

    fun distanceSq (u, v) = lengthSq (sub (u, v))
    fun distance (u, v) = length (sub (u, v))

    fun lengthAndDir v = let
	  val l = length v
	  in
	    if (l < epsilon)
	      then (0.0, zero)
	      else (l, scale(1.0 / l, v))
	  end

    fun normalize v = U.second2(lengthAndDir v)

    fun reflect (v, n) = adds (v, ~2.0 * dot(v, n), n)

    fun rotateX angle = let
	  val theta = (Double.pi * angle) / 180.0
	  val s = Double.sin theta
	  val c = Double.cos theta
	  in
	    fn ((x, y, z) : t) => (x, c * y - s * z, s * y + c * z)
	  end

    fun rotateY angle = let
	  val theta = (Double.pi * angle) / 180.0
	  val s = Double.sin theta
	  val c = Double.cos theta
	  in
	    fn ((x, y, z) : t) => (c * x + s * z, y, c * z - s * x)
	  end

    fun rotateZ angle = let
	  val theta = (Double.pi * angle) / 180.0
	  val s = Double.sin theta
	  val c = Double.cos theta
	  in
	    fn ((x, y, z) : t) => (c * x - s * y, s * x + c * y, z)
	  end

    fun randomPointInSphere () = let
	  val pt = (Rand.rand(), Rand.rand(), Rand.rand())
	  in
	    if (dot(pt, pt) < 1.0) then pt else randomPointInSphere()
	  end

  (* iterators *)
    fun map f (x, y, z) = (f x, f y, f z)
    fun app (f : 'a -> unit) (x, y, z) = (f x; f y; f z)

  end
(* ray.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Ray (* : sig

    type t = Vec3.t * Vec3.t

    val make : Vec3.t * Vec3.t -> t

    val eval : t * double -> Vec3.t

  end *) = struct

    type t = (Vec3.t * Vec3.t)

    fun make (origin, dir) = (origin, Vec3.normalize dir)

    fun eval (r : t, t) = Vec3.adds (U.first2 r, t, U.second2 r)

  end
(* material.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Material (* : sig

    type t

  (* Hit(t, pt, norm, material) *)
    datatype hit = Hit of double * Vec3.t * Vec3.t * t

    val getEmission : hit -> RGB.t
    val getHitInfo : hit * Ray.t -> (RGB.t * Ray.t) option

    val flat : RGB.t -> t
    val normal : t
    val lambertian : RGB.t -> t
    val metal : RGB.t * double -> t
    val diffuseLight : RGB.t -> t

  end *) = struct

    datatype hit = Hit of double * Vec3.t * Vec3.t * t

    and t = Material of
        (hit -> RGB.t) *				(* emit *)
	(Ray.t * hit -> (RGB.t * Ray.t) option)		(* scatter *)

    fun getEmission hit = (case hit
      of Hit(_, _, _, Material(emit, _)) => emit hit
      (* end case *))

    fun getHitInfo (hit, ray) = (case hit
      of Hit(_, _, _, Material(_, scatter)) => scatter (ray, hit)
      (* end case *))

    fun flat rgb = Material(
	  fn _ => RGB.black,
	  fn _ => SOME(rgb, (Vec3.zero, Vec3.zero)))

    val normal = Material(
	  fn _ => RGB.black,
	  fn (_, Hit(_, _, (x, y, z), _)) => SOME(
		(0.5 * (x + 1.0), 0.5 * (y + 1.0), 0.5 * (z + 1.0)),
		(Vec3.zero, Vec3.zero)))

    fun lambertian albedo = Material(
	  fn _ => RGB.black,
	  fn (ray, Hit(_, pt, norm, _)) => SOME(
		albedo,
		Ray.make(pt, Vec3.add(norm, Vec3.randomPointInSphere()))))

    fun metal (albedo, fuzz) = Material(
	  fn _ => RGB.black,
	  fn ((_, rdir), Hit(_, pt, norm, _)) => let
		val dir = Vec3.adds(
		      Vec3.reflect(rdir, norm),
		      fuzz,
		      Vec3.randomPointInSphere())
		in
		  if Vec3.dot(dir, norm) <= 0.0
		    then NONE
		    else SOME(albedo, Ray.make(pt, dir))
		end)

    fun diffuseLight rgb = Material(
	  fn _ => rgb,
	  fn _ => NONE)

  end

(* object.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Object (* : sig

    datatype maybe_hit = Miss | Hit of Material.hit

    datatype t = Obj of Ray.t * Interval.t -> maybe_hit

  (* test a ray against an object *)
    val hitTest : t * Ray.t * Interval.t -> maybe_hit

  (* an empty object that cannot be hit by rays *)
    val empty : t

  (* make an object from a list of objects *)
    val fromList : t list -> t

  (* translate the object by the given offset *)
    val translate : Vec3.t * t -> t

  (* rotate the object counter-clockwise by the specified angle (in degrees) *)
    val rotateX : double * t -> t
    val rotateY : double * t -> t
    val rotateZ : double * t -> t

  end *) = struct

    datatype maybe_hit = Miss | Hit of Material.hit

    datatype t = Obj of Ray.t * Interval.t -> maybe_hit

    fun hitTest (Obj hit, ray, minMaxT) = hit(ray, minMaxT)

    val empty = Obj(fn _ => Miss)

  (* fast min/max functions for reals *)
    fun fmin (x : double, y) = if (x < y) then x else y
    fun fmax (x : double, y) = if (x > y) then x else y

    fun closer (Miss, maybeHit) = maybeHit
      | closer (maybeHit, Miss) = maybeHit
      | closer (hit1, hit2) = (case (hit1, hit2)
        of (Hit(Material.Hit(t1, _, _, _)), Hit(Material.Hit(t2, _, _, _))) =>
            if (t1 <= t2) then hit1 else hit2
        (* end case *))

    fun fromList nil = empty
      | fromList (obj :: nil) = obj
      | fromList objs = let
          val obj1 = List.hd objs
          val objr = List.tl objs
          fun hitTest' (ray, minMaxT) = List.foldl
                (fn (obj, mhit) => closer(mhit, hitTest(obj, ray, minMaxT)))
                  Miss objs
          in
            Obj hitTest'
          end

    fun translate (delta, Obj hit) = let
	  fun hitTest' ((origin, dir), minMaxT) = (
		case hit ((Vec3.sub(origin, delta), dir), minMaxT)
	         of Hit(Material.Hit(t, pt, norm, material)) =>
		      Hit(Material.Hit(t, Vec3.add(pt, delta), norm, material))
		  | Miss => Miss
		(* end case *))
	  in
	    Obj hitTest'
	  end

    fun rotateX (angle, Obj hit) = let
	  val toObj = Vec3.rotateX (~angle)
	  val toWorld = Vec3.rotateX angle
	  fun hitTest' ((origin, dir), minMaxT) = (
		case hit ((toObj origin, toObj dir), minMaxT)
		 of Hit(Material.Hit(t, pt, norm, material)) =>
		      Hit(Material.Hit(t, toWorld pt, toWorld norm, material))
		  | Miss => Miss
		(* end case *))
	  in
	    Obj hitTest'
	  end

    fun rotateY (angle, Obj hit) = let
	  val toObj = Vec3.rotateY (~angle)
	  val toWorld = Vec3.rotateY angle
	  fun hitTest' ((origin, dir), minMaxT) = (
		case hit ((toObj origin, toObj dir), minMaxT)
		 of Hit(Material.Hit(t, pt, norm, material)) =>
		      Hit(Material.Hit(t, toWorld pt, toWorld norm, material))
		  | Miss => Miss
		(* end case *))
	  in
	    Obj hitTest'
	  end

    fun rotateZ (angle, Obj hit) = let
	  val toObj = Vec3.rotateZ (~angle)
	  val toWorld = Vec3.rotateZ angle
	  fun hitTest' ((origin, dir), minMaxT) = (
		case hit ((toObj origin, toObj dir), minMaxT)
		 of Hit(Material.Hit(t, pt, norm, material)) =>
		      Hit(Material.Hit(t, toWorld pt, toWorld norm, material))
		  | Miss => Miss
		(* end case *))
	  in
	    Obj hitTest'
	  end

  end
(* sphere.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Sphere (* : sig

    val make : Vec3.t * double * Material.t -> Object.t

  end *) = struct

    fun make (center, radius, material) = let
	  val rSq = radius * radius
	  val invR = 1.0 / radius
    fun hitTest (ray, minMaxT) = let
		val (ro, rd) = ray
		val q = Vec3.sub(ro, center)
		val b = 2.0 * Vec3.dot(rd, q)
		val c = Vec3.dot(q, q) - rSq
		val disc = b*b - 4.0*c
		in
		  if (disc < 0.0)
		    then Object.Miss
		    else let
		      val t = 0.5 * (~b - Double.sqrt disc)
		      in
			if Interval.within(t, minMaxT)
			  then let
			    val pt = Ray.eval(ray, t)
			    in
			      Object.Hit(Material.Hit(
				t, pt,
				Vec3.scale(invR, Vec3.sub(pt, center)),
				material))
			    end
			  else Object.Miss
		      end
		end
	  in
	    Object.Obj hitTest
	  end

  end
(* image.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Image (* : sig

  (* Img(wid, ht, pixels) *)
    datatype t = Img of int * int * Color.t list

    val writePPM : string * t -> unit

  end *) = struct

    datatype t = Img of int * int * Color.t list

    fun writePPM (file, Img(wid, ht, pixels)) = let
      (* val outS = TextIO.openOut file *)
      val pr = Print.print (* TextIO.output(outS, s) *)
      fun pixToS (r, g, b) = concat [Int.toString r, " ", Int.toString g, " ", Int.toString b]
      in
      (* write header for Plain PPM file: http://netpbm.sourceforge.net/doc/ppm.html *)
        pr "P3\n";
        pr (concat[Int.toString wid, " ", Int.toString ht, "\n"]);
        pr "255\n";
      (* write pixels *)
        List.app (fn pix => (pr (pixToS pix); pr "\n")) pixels
      (* close file *)
        (* TextIO.closeOut outS *)
      end

  end
(* camera.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Camera (* : sig

    type t

 (* make (wid, ht, ns, pos, lookAt, up, fov) *)
    val make : int * int * int * Vec3.t * Vec3.t * Vec3.t * double -> t

  (* simple camera located at the origin looking down the negative Z axis *)
    val simpleCamera : int * int * int * double -> t

    type pixel_renderer = (int * int -> Color.t)

    val makePixelRenderer : (int * int -> RGB.t) * (RGB.t -> Color.t) -> pixel_renderer

    val foreachPixel : t * pixel_renderer -> Image.t

    val pixelToRGB : t * (Ray.t -> RGB.t) -> int * int -> RGB.t

    val rayToRGB : Ray.t -> RGB.t

    val aaPixelToRGB : t * (Ray.t -> RGB.t) -> int * int -> RGB.t

  end *) = struct

    datatype t = Cam of (
	int *		(* width of image *)
	int *		(* height of image *)
	int *		(* number of samples per pixel *)
	Vec3.t *	(* position of camera *)
	Vec3.t *	(* upper-left-corner of image plane *)
	Vec3.t *	(* horizontal pixel-wide vector parallel to image pointing right *)
	Vec3.t)		(* vertical pixel-wide vector parallel to image pointing down *)

    fun make (wid, ht, ns, pos, lookAt, up, fov) = let
	  val dir = Vec3.normalize (Vec3.sub (lookAt, pos))
	  val right = Vec3.normalize (Vec3.cross (dir, up))
	  val up = Vec3.normalize (Vec3.cross (right, dir))
	  val pw = 2.0 / Double.fromInt wid
	  val aspect = Double.fromInt ht / Double.fromInt wid
	  val theta = (Double.pi * fov) / 180.0
	  val flen = 1.0 / Double.tan (0.5 * theta)
	  val imgCenter = Vec3.add(pos, Vec3.scale(flen, dir))
	  val ulc = Vec3.sub(Vec3.add(imgCenter, Vec3.scale(aspect, up)), right)
	  in
	    Cam(wid, ht, ns, pos, ulc, Vec3.scale(pw, right), Vec3.scale(~pw, up))
	  end

    fun simpleCamera (wid, ht, ns, flen) = let
	  val pw = 2.0 / Double.fromInt wid
	  val aspect = Double.fromInt ht / Double.fromInt wid
	  in
	    Cam(
	      wid, ht, ns, Vec3.zero,
	      (0.5 * pw - 1.0, aspect, ~ flen), (pw, 0.0, 0.0), (0.0, ~pw, 0.0))
	  end

    type pixel_renderer = (int * int -> Color.t)

    fun makePixelRenderer (toRGB, cvt) coords = cvt(toRGB coords)

    fun foreachPixel (Cam(wid, ht, _, _, _, _, _), pr) = let
	  fun rowLp (r, colors) = if (r < 0) then colors else colLp(r-1, wid-1, colors)
	  and colLp (r, c, colors) = if (c < 0)
		then rowLp (r, colors)
		else colLp (r, c-1, pr(r, c) :: colors)
	  in
	    Image.Img(wid, ht, rowLp (ht - 1, []))
	  end

    fun rayForPixel (Cam(_, _, _, pos, ulc, hvec, vvec)) = let
	  val ulcCenter = Vec3.adds(ulc, 0.5, Vec3.add(hvec, vvec))
	  in
	    fn (r, c) => Ray.make (
		pos,
		Vec3.add (ulcCenter,
		  Vec3.add (
                    Vec3.scale (Double.fromInt r, vvec),
		    Vec3.scale (Double.fromInt c, hvec))))
	  end

    fun pixelToRGB (cam, trace) = let
	  val rfp = rayForPixel cam
	  in
	    fn coords => trace (rfp coords)
	  end

    fun rayToRGB ((_, (_, y, _)) : Ray.t) =
	  RGB.lerp (RGB.white, 0.5 * (y + 1.0), (0.5, 0.7, 1.0))

    fun raysForPixel (Cam(_, _, ns, pos, ulc, hvec, vvec)) (r, c) = let
	  val r = Double.fromInt r
	  val c = Double.fromInt c
	  val ulcDir = Vec3.sub(ulc, pos)
	  fun mkRay _ = let
		val dir = Vec3.adds(ulcDir, r + Rand.rand(), vvec)
		val dir = Vec3.adds(dir, c + Rand.rand(), hvec)
		in
		  Ray.make (pos, dir)
		end
	  in
	    List.tabulate (ns, mkRay)
	  end

    fun aaPixelToRGB (cam, trace) = (case cam
      of Cam(_, _, ns, _, _, _, _) => let
	  val rfp = raysForPixel cam
	  val scale = if ns = 0 then 1.0 else 1.0 / Double.fromInt ns
	  in
	    fn coords => RGB.scale(
		scale,
		List.foldl
		  (fn (ray, c) => RGB.add(c, trace ray))
		    RGB.black (rfp coords))
	  end
    (* end case *))

  end
(* trace.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Trace (* : sig

  (* ray caster for testing purposes *)
    val castRay : Object.t -> Ray.t -> RGB.t

  (* Given a world object and a maximum tracing depth, this function
   * returns a function that will recursively trace a ray through the
   * world to compute a color
   *)
    val traceRay : Object.t * int -> Ray.t -> RGB.t

    val rayTracer : Camera.t * Object.t -> Image.t

    val timeIt : (unit -> 'a) -> 'a

  end *) = struct

    fun castRay world ray = (
	  case Object.hitTest (world, ray, (0.0, U.posInf))
	   of Object.Miss => Camera.rayToRGB ray
	    | Object.Hit hit => (case Material.getHitInfo(hit, ray)
		 of NONE => Material.getEmission hit
		  | SOME(aten, _) => RGB.add(Material.getEmission hit, aten)
		(* end case *))
	  (* end case *))

    fun traceRay (world, maxDepth) = let
	  val minMaxT = (0.001, U.posInf)
	  fun trace (ray, depth) = if (depth <= 0)
		then RGB.black
		else (case Object.hitTest (world, ray, minMaxT)
		   of Object.Miss => Camera.rayToRGB ray
		    | Object.Hit hit => (case Material.getHitInfo(hit, ray)
			 of NONE => Material.getEmission hit
			  | SOME(aten, reflect) => RGB.add(
			      Material.getEmission hit,
			      RGB.modulate(aten, trace (reflect, depth-1)))
			(* end case *))
		  (* end case *))
	  in
	    fn ray => trace(ray, maxDepth)
	  end

    fun rayTracer (cam, world) =
	  Camera.foreachPixel (
	    cam,
	    Camera.makePixelRenderer (
	      Camera.aaPixelToRGB(cam, traceRay (world, 20)),
	      Color.fromRGBWithGamma))

    (* fun timeIt f = let
	  val rtStart = Timer.startRealTimer()
	  val result = f()
	  val t = Timer.checkRealTimer rtStart
	  in
	    print (concat["total time = ", Time.toString t, "\n"]);
	    result
	  end *)

  end
(* test-random-scene.sml
 *
 * COPYRIGHT (c) 2019 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure TestRandomScene =
  struct

    fun randomSphere (x, z) = let
	  val chooseMat = Rand.rand()
	  val c = let
		val x = Double.fromInt x + (0.9 * Rand.rand())
		val z = Double.fromInt z + (0.9 * Rand.rand())
		in
		  (x, 0.2, z)
		end
	  val mat = if chooseMat < 0.8
		then Material.lambertian (
		    Rand.rand() * Rand.rand(),
		    Rand.rand() * Rand.rand(),
		    Rand.rand() * Rand.rand())
		else Material.metal (
		    ( 0.5 * (1.0 + Rand.rand()),
		      0.5 * (1.0 + Rand.rand()),
		      0.5 * (1.0 + Rand.rand()) ),
		    0.5 * Rand.rand())
	  in
	    Sphere.make (c, 0.2, mat)
	  end

    fun makeScene () = let
	  fun lp (x, z, objs) =
		if (z < 11) then lp (x, z+1, randomSphere(x, z) :: objs)
		else if (x < 11) then lp (x+1, ~11, objs)
		else objs
	  in
	    Object.fromList (
	      lp (~11, ~11, [
		  Sphere.make((0.0, ~1000.0, 0.0), 1000.0,
		    Material.lambertian(RGB.gray 0.5)),
		  Sphere.make((4.0, 1.0, 0.0), 1.0,
		    Material.metal((0.7, 0.6, 0.5), 0.0)),
		  Sphere.make((~4.0, 1.0, 0.0), 1.0,
		    Material.lambertian(0.4, 0.2, 0.1))
		]))
	  end


    fun buildScene () = let
          val cam = Camera.make (
                300, 200, 20,
                (13.0, 2.0, 3.0), Vec3.zero, (0.0, 1.0, 0.0),
                30.0)
          val world = makeScene()
      in
        (cam, world)
      end


    fun test () = let
        val scene = buildScene()
      in
        (* RunSeq.run (fn () => *) Trace.rayTracer scene (* ) *)
      end


    fun test' file = Image.writePPM (file, test())


  end


  (* The main code *)
  structure Main = struct

    (* only time it *)
    fun go () = TestRandomScene.test()

    (* render and dump the image to a Plain PPM file, with timing.
       use ImageMagick to generate a PNG image like so:
              convert image.ppm image.png
    *)
    fun test () = TestRandomScene.test'("image.ppm")

  end

val _ = Main.test()
(* val _ = Main.go() *)
