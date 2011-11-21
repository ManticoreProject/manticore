val (itos, ftos, ln) = (Int.toString, Float.toString, Print.printLn)

val sizeDefault = 40

(* mkImageSilent : int parray parray -> image *)
(* assumes input array is a rectangle *)
(* assumes each int is <empty byte><r byte><g byte><b byte> *)
val mkImageSilent : int parray parray -> Image.image = let
  fun f css = let
    val w = PArray.length (css ! 0)
    val h = PArray.length css
    val img = Image.new (w, h)
    fun lp row =
      if (row >= h) then ()
      else let
        fun lp' col =
          if (col >= w) then ()
          else let
            val colorInt = css!row!col
            fun cvt d = Float.fromInt ((colorInt div d) mod 256) / 255.0
            val r = cvt 65536
            val g = cvt 256
            val b = cvt 1
            in
              (Image.update3f (img, row, col, r, g, b);
               lp' (col+1))
            end
          in
            (lp' 0; lp (row+1))
          end
      in
        (lp 0;
         img)
      end
  in
    f
  end

(* mkImageVerbose : int parray parray -> image *)
(* assumes input array is a rectangle *)
(* assumes each int is <empty byte><r byte><g byte><b byte> *)
val mkImageVerbose : int parray parray -> Image.image = let
  fun f css = let
    val w = PArray.length (css ! 0)
    val h = PArray.length css
    val _ = ln ("w " ^ itos w ^ " h " ^ itos h)
    val img = Image.new (w, h)
    fun lp row =
      if (row >= h) then ()
      else let
        fun lp' col =
          if (col >= w) then ()
          else let
            val colorInt = css!row!col
            fun cvt d = Float.fromInt ((colorInt div d) mod 256) / 255.0
            val r = cvt 65536
            val g = cvt 256
            val b = cvt 1
	    val _ = ln ("row " ^ itos row ^ " col " ^ itos col ^
	    		" colorInt " ^ itos colorInt ^
	    		" r " ^ ftos r ^ " g " ^ ftos g ^ " b " ^ ftos b)
            in
              (Image.update3f (img, row, col, r, g, b);
               lp' (col+1))
            end
          in
            (lp' 0; lp (row+1))
          end
      in
        (lp 0;
         img)
      end
  in
    f
  end

(* main etc. *)

fun getArgs args = let
  fun lp (args, sz) = (case args 
    of nil => (case sz
         of NONE => sizeDefault
	  | SOME s => s
         (* end case *))
     | s::ss => 
	 if String.same (s, "-size") then (case ss
           of s'::ss' => lp (ss', Int.fromString s')
	    | _ => lp (ss, sz)
           (* end case *))
	 else lp (ss, sz)
    (* end case *))
  in
    lp (args, SOME sizeDefault)
  end

fun main args = let
  val n = getArgs args
  val side = [| 0 to (n-1) |]
  val css = [| [| 255 | _ in side |] | _ in side |]
  val imgV = mkImageVerbose css
  val imgS = mkImageSilent css
  val _ = Image.output ("blue-verbose.ppm", imgV)
  val _ = Image.output ("blue-silent.ppm", imgS)
  val _ = (Image.free imgV; Image.free imgS)
  in
    ln ("Done. Please see results in blue-*.ppm.")
  end

val _ = main (CommandLine.arguments ())
