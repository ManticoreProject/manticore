(* wrapper for rt.pml *)

type double = real;
val sqrtd = Math.sqrt;
val powd = Math.pow;
fun fail msg = raise Fail msg;
val itod = real;
val tand = Math.tan;
val dtos = Real.toString;

fun readint () = 1024;

fun gettimeofday () = Time.toReal(Time.now());

datatype image = IMAGE of {
    w : int,
    h : int,
    img : {r : Word8.word, g : Word8.word, b : Word8.word} array
  };

fun newImage (w, h) = let
      val black = {r=0w0, g=0w0, b=0w0}
      in
	IMAGE{w=w, h=h, img=Array.array(h*w, black)}
      end;
fun updateImage3d (IMAGE{w, h, img}, i, j, r, g, b) = let
      fun toByte x = Word8.fromInt(Real.round(x * 255.0))
      val pixel = {r = toByte r, g = toByte g, b = toByte b}
      in
	Array.update (img, (i*w)+j, pixel)
      end;

fun freeImage _ = ();

fun outputImage (IMAGE{w, h, img}, file) = let
      val outS = BinIO.openOut file
      fun out x = BinIO.output1(outS, x)
      fun outRGB {r, g, b} = (out r; out g; out b)
      fun pr s = BinIO.output(outS, Byte.stringToBytes s)
      in
        pr "P6\n";
	pr(concat[Int.toString w, " ", Int.toString h, "\n"]);
	pr "255\n";
	Array.app outRGB img;
	BinIO.closeOut outS
      end;

use "rt.pml";
