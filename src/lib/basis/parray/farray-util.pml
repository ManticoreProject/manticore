(* farray-util.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module exists to contain functions that transform between various 
 * polymorphic and monomorphic farray forms. We avoid recursive dependencies
 * by having them here in a separate module that sits "below" (i.e. downstream of)
 * the others.
 *)

structure FArrayUtil = struct

  val fail = Fail.fail "FArrayUtil"

  structure S  = Shape
  structure F  = FArray
  structure IF = IntFArray
  structure DF = DoubleFArray

  val pr = Print.printLn

  local
    (* fromList: concat a list of int_farrays into one int_farray *)
    fun fromList fs = let
      fun lp (nil, ropeAcc, shapeAcc, _) = let
	    val r = IntRope.balance ropeAcc
	    val s = S.Nd (List.rev shapeAcc)
            in
	      IF.FArray (r, s)
            end
	| lp (IF.FArray(r,s)::t, ropeAcc, shapeAcc, i) = let
	    val r' = IntRope.concat (ropeAcc, r)
	    val s' = S.incrBy i s
            in
	      lp (t, r', s'::shapeAcc, S.maxIdx s')
	    end
      in
        case fs 
	 of IF.FArray(r,s)::t => lp (t, r, [s], S.maxIdx s)    
	  | nil => IF.empty
      end	      
  in
    (* flatten_IF_F : int_farray f_array -> int_farray *)
    fun flatten_IF_F iff = (case F.clean iff
      of F.FArray (data, shape) => (case shape
           of S.Lf (lo, hi) => let
	        fun lp (i, acc) = 
                  if (i<0) then fromList acc
	  	  else lp (i-1, Rope.sub(data,i)::acc)
		val _ = Print.printLn "running flatten_IF_F"
                in
                  lp (hi-1, [])
	        end
	    | S.Nd _ => fail "flatten_IF_F" "not a flat farray"
           (* end case *))
      (* end case *))
  end (* local *)

  local
    (* fromList: concat a list of double_farrays into one double_farray *)
    fun fromList fs = let
      fun lp (nil, ropeAcc, shapeAcc, _) = let
	    val r = DoubleRope.balance ropeAcc
	    val s = S.Nd (List.rev shapeAcc)
            in
	      DF.FArray (r, s)
            end
	| lp (DF.FArray(r,s)::t, ropeAcc, shapeAcc, i) = let
	    val r' = DoubleRope.concat (ropeAcc, r)
	    val s' = S.incrBy i s
            in
	      lp (t, r', s'::shapeAcc, S.maxIdx s')
	    end
      in
        case fs 
	 of DF.FArray(r,s)::t => lp (t, r, [s], S.maxIdx s)    
	  | nil => DF.empty
      end	      
  in
    (* flatten_DF_F : double_farray f_array -> double_farray *)
    fun flatten_DF_F dff = (case F.clean dff
      of F.FArray (data, shape) => (case shape
           of S.Lf (lo, hi) => let
	        fun lp (i, acc) = 
                  if (i<0) then fromList acc
	  	  else lp (i-1, Rope.sub(data,i)::acc)
		val _ = Print.printLn "running flatten_DF_F"
                in
                  lp (hi-1, [])
	        end
	    | S.Nd _ => fail "flatten_IF_F" "not a flat farray"
           (* end case *))
      (* end case *))
  end (* local *)

(* map_IF_poly : (int -> 'b) -> int_farray -> 'b farray *)
  fun map_IF_poly f ns = (case IF.clean ns
    of IF.FArray (data, shape) => (case shape
         of S.Lf (lo, hi) => let
              fun lp (i, acc) = 
                if (i<0) then FArray.fromList (List.rev acc)
		else lp (i-1, f (IntRope.sub(data,i))::acc)
                in
                  lp (hi-1, [])
                end
	  | S.Nd _ => fail "map_IF_poly" "not a flat int_farray"
         (* end case *))
    (* end case *))

(* map_IFF_IF : (int_farray -> int) -> int_farray -> int_farray *)
  fun map_IFF_IF f nss = let
    val len = IF.length nss
    fun lp (i, acc) =  
      if (i<0) then
        IF.fromList acc
      else let
        val n = f (IF.nestedSub (nss, i))
        in
	  lp (i-1, n::acc)
	end
    in
      lp (len-1, [])
    end

(* map_IFF_DFF_DF : (int_farray * dbl_farray -> dbl) -> int_farray * dbl_farray -> dbl_farray *)
  fun map_IFF_DFF_DF f (nss, xss) = let
    val prln = Print.printLn
    fun loc i = prln ("location " ^ Int.toString i)
    val _ = prln "entering map_IFF_DFF_DF"
    val len = IF.length nss
    val len' = DF.length xss
    val _ = prln ("length of nss is " ^ Int.toString len)
    val _ = prln ("length of xss is " ^ Int.toString len')
    val _ = loc 0
    val _ = if (len = len') then () else fail "map_IFF_DFF_DF" "length"
    val _ = loc 1
    fun lp (i, acc) = 
      if (i<0) then
        DF.fromList acc
      else let
	val _ = prln ("in lp, i is " ^ Int.toString i)
	val ns = IF.clean (IF.nestedSub (nss, i))
	val _ = prln ("length of ns is " ^ Int.toString (IF.length ns))
	val _ = loc 11
	val xs = DF.clean (DF.nestedSub (xss, i))
	val _ = prln ("length of xs is " ^ Int.toString (DF.length xs))
	val _ = loc 12
        val x = f (ns, xs)
        in
          lp (i-1, x::acc)
        end
    val res = lp (len-1, [])
    val _ = loc 2
    val _ = prln "exiting map_IFF_DFF_DF"
    in 
      res      
    end

(* mapDDD : (dbl * dbl -> dbl) * dbl_farray * dbl_farray -> dbl_farray *)
  fun mapDDD (f : double * double -> double, a1, a2) = let
    val (DF.FArray (data1, shape1)) = DF.clean a1
    val (DF.FArray (data2, shape2)) = DF.clean a2
    val _ = if Shape.same (shape1, shape2) then () 
	    else let
              val msg = "todo: differing shapes " ^ 
			Shape.toString shape1 ^ "/" ^
			Shape.toString shape2
	      in
                fail "mapDDD" msg
              end
    val data = RopeUtil.mapDDD (f, data1, data2)
    in
      DF.FArray (data, shape1)
    end

(* unzip_IF_IF *)
  fun unzip_IF_IF (x : (IF.int_farray * IF.int_farray) F.farray)
            : IF.int_farray F.farray * IF.int_farray F.farray = (case x
    of F.FArray (data, shape) => let
         val (data1, data2) = RopePair.unzip data
         in
	   (F.FArray (data1, shape), F.FArray (data2, shape))
         end
    (* end case *))

(* unzip_IF_DF *)
  fun unzip_IF_DF (x : (IF.int_farray * DF.double_farray) F.farray)
            : IF.int_farray F.farray * DF.double_farray F.farray = (case x
    of F.FArray (data, shape) => let
         val (data1, data2) = RopePair.unzip data
         in
	   (F.FArray (data1, shape), F.FArray (data2, shape))
         end
    (* end case *))

  

end
