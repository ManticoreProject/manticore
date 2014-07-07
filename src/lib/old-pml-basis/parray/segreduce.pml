(* Segmented Reduce and its associated utilities. *)

structure SegReduce = struct

  structure S = Shape
  structure R = Rope
  structure F = FArray

  fun fail a b = raise Fail (String.concat["seg-sum", a, b])

  (* writePairs : 'a seq * (int * 'a) list -> 'a list *)
  fun writePairs res pss = let
    fun sub i = Seq.sub (res, i)
    fun upd (i, x) = Seq.update (res, i, x)
    fun lp1 ps = (case ps
      of nil => ()
       | (i,n)::t => (upd (i, n); lp1 t)
      (* end case *))
    fun lp0 ps = (case ps
      of nil => ()
       | (i,n)::t => (upd (i, n+sub(i)); lp1 t)
      (* end case *))
    in 
      List.app lp0 pss
    end

  (* split : int * 'a list -> ('a list * 'a list) *)
  fun split (n, ps) = 
    if n<0 then fail "split" "n<0"
    else let
      fun lp (n, ps, acc) = 
        if n=0 then (List.rev acc, ps)
        else (case ps
          of nil => fail "split" "nil"
           | (i,m)::t =>
               if (n<m) then 
                 lp (0, (i,m-n)::t, (i,n)::acc)
               else (* n>=m *)
                 lp (n-m, t, (i,m)::acc)               
          (* end case *))
      in
        lp (n, ps, nil)
      end

  (* segdesFromShape : shape -> (int * int) list *)
  fun segdesFromShape s = let
    fun lp (i, ls) = (case ls
      of nil => []
       | l::ls => (case l
           of S.Lf(lo,hi) => (i,hi-lo)::lp (i+1, ls)
            | S.Nd _ => fail "segdesFromShape" "Nd"
           (* end case *))
      (* end case *))
    in
      case s
        of S.Nd ls => let
             (* val itos = Int.toString
             fun ptos (n,m) = "(" ^ itos n ^ "," ^ itos m ^ ")"
             fun pstos ps = String.concat (List.map ptos ps) *)
             val sd = lp (0, ls) 
             (* val _ = Print.printLn "original shape:"
             val _ = Print.printLn (S.toString s)
             val _ = Print.printLn "segdes:"
             val _ = Print.printLn (pstos sd) *)
             in
               sd
             end
         | S.Lf _ => fail "segdesFromShape" "Lf"
    end

  (* partReduce : ('a -> 'a) * 'a * 'a seq * int * int -> 'a *)
  fun partReduce (f, init, data, lo, len) = let
    val hi = lo+len
    fun sub i = Seq.sub(data,i)
    fun lp (i, acc) =
      if i >= hi then acc
      else lp (i+1, f(acc, sub i))
    in
      lp (lo, init)
    end

  (* segReducev : ('a -> 'a) * 'a * 'a seq * (int * int) list -> (int * 'a) list *)
  fun segReducev (f, init, v, ps) = let
    fun lp (i, ps) = (case ps
      of nil => nil
       | (j,n)::t => let
           val s = partReduce (f, init, v, i, n)
           in
             (j,s)::lp(i+n,t)
           end
      (* end case *))
    in
      lp (0, ps)
    end

  (* segreduce : ('a -> 'a) * 'a * 'a farray farray -> 'a farray *)
  fun segreduce (f, init, nss) = let
    val (F.FArray (data, shape)) = nss
    val segdes = segdesFromShape shape
    fun lp (r, ps) = (case r
      of R.Leaf v => segReducev(f,init,v,ps)::nil
       | R.Cat (_, _, rL, rR) => let
           val nL = R.length rL
           val (psL, psR) = split (nL, ps)
           val (sumsL, sumsR) = (| lp (rL, psL), lp (rR, psR) |)
           in
             sumsL @ sumsR
           end
      (* end case *))
     fun lp2 (r, ps) = (case r
      of R.Leaf v => [segReducev(f,init,v,ps)::nil]
       | R.Cat (_, _, rL, rR) => let
           val nL = R.length rL
           val (psL, psR) = split (nL, ps)
           val (sumsL, sumsR) = (| lp2 (rL, psL), lp2 (rR, psR) |)
           in
             sumsL @ sumsR
           end
      (* end case *))
    fun reassemble pssL = (case pssL
     of pss::t => pss @ (reassemble t)
      | nil => nil
      (* end case *))
    val pssList = lp2 (data, segdes)
    val pss = reassemble pssList
(*    val pss = lp (data, segdes) *)
    (* val _ = Print.printLn "in segsum: computed pss:" *)
    (* val _ = Print.printLn (psstos pss) *)
    val reductions = Seq.tabulate (List.length segdes, fn _ => init)
    val _ = writePairs reductions pss
    val data' = R.fromSeq reductions
    val shape' = S.Lf (0, R.length data')
    in
      F.FArray (data', shape')
    end

end
