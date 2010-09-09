(* An encoding of the Barnes-Hut n-body force calculation algorithm,
 * closely following the version given in Keller's thesis.
 *)
 
  type vec      = float * float; (* x and y *)

  type cen      = float * vec;   (* mass and position *)

  type particle = vec * cen;     (* position and velocity *)
       
  type area     = vec * vec;     (* lower left and upper right *)

  datatype tree = Tree of cen * (tree parray) 
		| Bogus; 

(* cut : area -> area * area * area * area
 * Divide given space into (a1, a2, a3, a4) where
 * +----+----+
   | a2 | a3 |
   +----+----+
   | a1 | a4 |
   +----+----+
*)
  fun cut ((llx, lly), (urx, ury)) =
      let val dx = (urx - llx) / 2.0
	  val dy = (ury - lly) / 2.0
	  val midx = llx + dx
	  val midy = lly + dy
      in
	  (((llx, lly), (midx, midy)),
	   ((llx, midy), (midx, ury)),
	   ((midx, midy), (urx, ury)),
	   ((midx, lly), (urx, midy)))
      end;

(* splitCentroids : vec * cen list -> (cen list * cen list * cen list * cen list) *)
(* These cen lists should be cen parrays. *)
  fun splitCentroids ((fx, fy), cs) = (0,0,0,0);
(*      let (* FIXME Redo these with parallel comprehensions. *)
	  fun filt1 (x, y) = (x <= fx) andalso (y <= fy)
	  fun filt2 (x, y) = (x > fx) andalso (y <= fy)
	  fun filt3 (x, y) = (x <= fx) andalso (y > fy)
	  fun filt4 (x, y) = (x > fx) andalso (y > fy)
      in
	  (| filter(filt1, cs),
	     filter(filt2, cs),
	     filter(filt3, cs),
	     filter(filt4, cs) |)
      end;
*)

(* centroid : cen parray -> cen *)
  fun centroid cs = 
      let fun addF (x:float, y) = x+y
	  fun addV ((x1, y1), (x2, y2)) = (x1+x2, y1+y2)
	  fun scale (s, (x, y)) = (s*x, s*y)
	  val zeroV = (0.0, 0.0)
	  val weightedSumV = reduceP (addV, zeroV, [| scale (m, pos) | (m, pos) in cs |])
	  val totalM = reduceP (addF, 0.0, [| m | (m, _) in cs |])
      in
	  scale (1.0 / totalM, weightedSumV)
      end;

(* singletonTree : cen -> tree *)
  fun singletonTree c =
      let val pnil = [| |]
      in
	  Tree (c, pnil)
      end;

(* bhTree : area * cen list -> tree *)
  fun bhTree (a, cs) =
      if (plen(cs) = 1) then
	  singletonTree (cs ! 0)
      else
	  let val (a1, a2, a3, a4) = cut a
	      val (a1LL, a1UR) = a1
	      val (cs1, cs2, cs3, cs4) = splitCentroids (a1UR, cs)
	      val areaCentroids = 0
	      val subtrees = 0
	      val cd = 0
	  in
	      (* Tree (cd, subtrees) *)
	      Bogus
	  end;
val _ = ()
