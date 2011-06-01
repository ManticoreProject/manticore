(* parray-util.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure PArrayUtil = struct

  val fail = Fail.fail "PArrayUtil"

  fun indexMap1D (f, t, s) = (fn k => f + k * s)

(* the following were generated, BTW *)

  fun indexMap2D ((f1,t1,s1), (f2,t2,s2)) = let
    val d1 = Range.nElts (f1,t1,s1)
    val d2 = Range.nElts (f2,t2,s2)
    in
      fn k => (f1 + ((k div d2) mod d1) * s1,
	       f2 + (k mod d2) * s2)
    end

  fun indexMap3D ((f1,t1,s1), (f2,t2,s2), (f3,t3,s3)) = let
    val d1 = Range.nElts (f1,t1,s1)
    val d2 = Range.nElts (f2,t2,s2)
    val d3 = Range.nElts (f3,t3,s3)
    in
      fn k => (f1 + ((k div (d2 * d3)) mod d1) * s1,
	       f2 + ((k div d3) mod d2) * s2,
	       f3 + (k mod d3) * s3)
    end

  fun indexMap4D ((f1,t1,s1), (f2,t2,s2), (f3,t3,s3), (f4,t4,s4)) = let
    val d1 = Range.nElts (f1,t1,s1)
    val d2 = Range.nElts (f2,t2,s2)
    val d3 = Range.nElts (f3,t3,s3)
    val d4 = Range.nElts (f4,t4,s4)
    in
      fn k => (f1 + ((k div (d2 * d3 * d4)) mod d1) * s1,
	       f2 + ((k div (d3 * d4)) mod d2) * s2,
	       f3 + ((k div d4) mod d3) * s3,
	       f4 + (k mod d4) * s4)
    end

  fun indexMap5D ((f1,t1,s1), (f2,t2,s2), (f3,t3,s3), (f4,t4,s4), (f5,t5,s5)) = let
    val d1 = Range.nElts (f1,t1,s1)
    val d2 = Range.nElts (f2,t2,s2)
    val d3 = Range.nElts (f3,t3,s3)
    val d4 = Range.nElts (f4,t4,s4)
    val d5 = Range.nElts (f5,t5,s5)
    in
      fn k => (f1 + ((k div (d2 * d3 * d4 * d5)) mod d1) * s1,
	       f2 + ((k div (d3 * d4 * d5)) mod d2) * s2,
	       f3 + ((k div (d4 * d5)) mod d3) * s3,
	       f4 + ((k div d5) mod d4) * s4,
	       f5 + (k mod d5) * s5)
    end

  fun indexMap6D ((f1,t1,s1), (f2,t2,s2), (f3,t3,s3), (f4,t4,s4), (f5,t5,s5), (f6,t6,s6)) = let
    val d1 = Range.nElts (f1,t1,s1)
    val d2 = Range.nElts (f2,t2,s2)
    val d3 = Range.nElts (f3,t3,s3)
    val d4 = Range.nElts (f4,t4,s4)
    val d5 = Range.nElts (f5,t5,s5)
    val d6 = Range.nElts (f6,t6,s6)
    in
      fn k => (f1 + ((k div (d2 * d3 * d4 * d5 * d6)) mod d1) * s1,
	       f2 + ((k div (d3 * d4 * d5 * d6)) mod d2) * s2,
	       f3 + ((k div (d4 * d5 * d6)) mod d3) * s3,
	       f4 + ((k div (d5 * d6)) mod d4) * s4,
	       f5 + ((k div d6) mod d5) * s5,
	       f6 + (k mod d6) * s6)
    end

  fun indexMap7D ((f1,t1,s1), (f2,t2,s2), (f3,t3,s3), (f4,t4,s4), (f5,t5,s5), (f6,t6,s6), (f7,t7,s7)) = let
    val d1 = Range.nElts (f1,t1,s1)
    val d2 = Range.nElts (f2,t2,s2)
    val d3 = Range.nElts (f3,t3,s3)
    val d4 = Range.nElts (f4,t4,s4)
    val d5 = Range.nElts (f5,t5,s5)
    val d6 = Range.nElts (f6,t6,s6)
    val d7 = Range.nElts (f7,t7,s7)
    in
      fn k => (f1 + ((k div (d2 * d3 * d4 * d5 * d6 * d7)) mod d1) * s1,
	       f2 + ((k div (d3 * d4 * d5 * d6 * d7)) mod d2) * s2,
	       f3 + ((k div (d4 * d5 * d6 * d7)) mod d3) * s3,
	       f4 + ((k div (d5 * d6 * d7)) mod d4) * s4,
	       f5 + ((k div (d6 * d7)) mod d5) * s5,
	       f6 + ((k div d7) mod d6) * s6,
	       f7 + (k mod d7) * s7)
    end

  fun indexMap8D ((f1,t1,s1), (f2,t2,s2), (f3,t3,s3), (f4,t4,s4), (f5,t5,s5), (f6,t6,s6), (f7,t7,s7), (f8,t8,s8)) = let
    val d1 = Range.nElts (f1,t1,s1)
    val d2 = Range.nElts (f2,t2,s2)
    val d3 = Range.nElts (f3,t3,s3)
    val d4 = Range.nElts (f4,t4,s4)
    val d5 = Range.nElts (f5,t5,s5)
    val d6 = Range.nElts (f6,t6,s6)
    val d7 = Range.nElts (f7,t7,s7)
    val d8 = Range.nElts (f8,t8,s8)
    in
      fn k => (f1 + ((k div (d2 * d3 * d4 * d5 * d6 * d7 * d8)) mod d1) * s1,
	       f2 + ((k div (d3 * d4 * d5 * d6 * d7 * d8)) mod d2) * s2,
	       f3 + ((k div (d4 * d5 * d6 * d7 * d8)) mod d3) * s3,
	       f4 + ((k div (d5 * d6 * d7 * d8)) mod d4) * s4,
	       f5 + ((k div (d6 * d7 * d8)) mod d5) * s5,
	       f6 + ((k div (d7 * d8)) mod d6) * s6,
	       f7 + ((k div d8) mod d7) * s7,
	       f8 + (k mod d8) * s8)
    end

  fun indexMap9D ((f1,t1,s1), (f2,t2,s2), (f3,t3,s3), (f4,t4,s4), (f5,t5,s5), (f6,t6,s6), (f7,t7,s7), (f8,t8,s8), (f9,t9,s9)) = let
    val d1 = Range.nElts (f1,t1,s1)
    val d2 = Range.nElts (f2,t2,s2)
    val d3 = Range.nElts (f3,t3,s3)
    val d4 = Range.nElts (f4,t4,s4)
    val d5 = Range.nElts (f5,t5,s5)
    val d6 = Range.nElts (f6,t6,s6)
    val d7 = Range.nElts (f7,t7,s7)
    val d8 = Range.nElts (f8,t8,s8)
    val d9 = Range.nElts (f9,t9,s9)
    in
      fn k => (f1 + ((k div (d2 * d3 * d4 * d5 * d6 * d7 * d8 * d9)) mod d1) * s1,
	       f2 + ((k div (d3 * d4 * d5 * d6 * d7 * d8 * d9)) mod d2) * s2,
	       f3 + ((k div (d4 * d5 * d6 * d7 * d8 * d9)) mod d3) * s3,
	       f4 + ((k div (d5 * d6 * d7 * d8 * d9)) mod d4) * s4,
	       f5 + ((k div (d6 * d7 * d8 * d9)) mod d5) * s5,
	       f6 + ((k div (d7 * d8 * d9)) mod d6) * s6,
	       f7 + ((k div (d8 * d9)) mod d7) * s7,
	       f8 + ((k div d9) mod d8) * s8,
	       f9 + (k mod d9) * s9)
    end

(* mkImage : int parray parray -> image *)
(* assumes input array is a rectangle *)
(* assumes each int is <empty byte><r byte><g byte><b byte> *)
    val mkImage : int parray parray -> Image.image = let
      fun f css = let
        val w = PArray.length (css ! 0)
        val h = PArray.length css
	val (itos, ftos, ln) = (Int.toString, Float.toString, Print.printLn)
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
		(* val _ = ln ("row " ^ itos row) *)
(*
		val _ = ln ("row " ^ itos row ^ " col " ^ itos col ^ " colorInt " ^ itos colorInt ^ 
			    " r " ^ ftos r ^ " g " ^ ftos g ^ " b " ^ ftos b)
*)
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

end


