structure T = struct

  val lfsz = 1

  val r = Random.rand (~2343333, 2343)
  fun roll n = Random.randNat r mod n

  val x = 2
  val nxt = ref (roll x + 1)
  fun get () =
      if !nxt = 0 then (nxt := roll x + 1; true)
      else let val y = !nxt
	   in
	       nxt := !nxt - 1;
	       false
	   end

  structure DummyLBS =
    struct
      datatype splitting_strategy = datatype SplittingStrategy.splitting_strategy
      val splittingStrategy = LBS{PPT=1}
(*EBS_AP{K=1,V=4}*)
      fun forkjoin (f, g) = (f (), g ())
      val parMap = List.map
      fun numAvailProcs () = 2
      fun workerId () = if roll 2 = 0 then 0 else 1
(*
val x : bool ref = ref false
fun get () =
    let
	val y = !x
    in
	x := not y;
	y
    end
*)
      fun otherHungryProcs () = get ()
    end

  structure R = RopeFn (
		   structure S = VectorSeq
		   val maxLeafSize = lfsz
		   structure RT = DummyLBS
		)

  fun randomInt () = roll 1000

  fun testmap n = 
      let
	  fun mapeq equal f r =
	      let val m1 = R.toList (R.map f r)
		  val m2 = List.map f (R.toList r)
	      in
		  ListPair.allEq equal (m1, m2)
	      end
      in
	  if n <= 0 then ()
	  else if mapeq (op =) (fn x => x * 2) (R.randomRope (randomInt, n, lfsz)) then ()
	  else raise Fail "testmap"
      end

  fun testreduce n = if n <= 0 then ()
		     else let val r = R.randomRope (randomInt, n, lfsz)
			      val ls = R.toList r
			  in
			      if List.foldl (op +) 0 ls = R.reduce (op +) 0 r then ()
			      else raise Fail "testreduce"
			  end

  fun testfilter n = if n <= 0 then ()
		     else let val r = R.randomRope (randomInt, n, lfsz)
			      val ls = R.toList r
			      val ls' = List.filter (fn x => x < 500) ls
			      val rp' = R.filter (fn x => x < 500) r
			  in
			      if ListPair.allEq (op =) (ls', R.toList rp') then ()
			      else raise Fail "testfilter"
			  end

  fun testtabulate n = if n <= 0 then ()
		       else let val r = R.tabulate (n, fn i => i)
				val l = List.tabulate (n, fn i => i)
			    in
				if ListPair.allEq (op =) (l, R.toList r) then ()
				else (
				    print (String.concatWith " " (List.map Int.toString l)^"\n");
				    print (String.concatWith " " (R.toList (R.map Int.toString r))^"\n");
				    raise Fail "testtabulate")
			    end


  local
      structure V = Vector
      fun inits s = V.tabulate (V.length s, fn i => V.tabulate (i, fn j => V.sub (s, j)))
      fun scanl aop z s = V.map (V.foldl aop z) (inits s)
      fun toList v = V.foldr (fn (x, ls) => x :: ls) [] v
  in
  fun testscan n = if n <= 0 then ()
		   else let val r = R.randomRope (randomInt, n, lfsz)
			    val v = V.fromList (R.toList r)
			    val ls' = toList (scanl (op +) 0 v)
			    val rp' = R.Scan.scanl (op +) 0 r
			in
			    if ListPair.allEq (op =) (ls', R.toList rp') then ()
			    else (
				print (String.concatWith " " (List.map Int.toString ls')^"\n");
				print (String.concatWith " " (R.toList (R.map Int.toString rp'))^"\n");
				print (String.concatWith " " (R.toList (R.map Int.toString r))^"\n"); 
				raise Fail "testscan")
			end
  end

  fun testmappair n =
      let val r1 = R.randomRope (randomInt, n, 333)
	  val r2 = R.randomRope (randomInt, n, 13)
	  val (lr, sr) = if R.length r1 > R.length r2 then (r1, r2) else (r2, r1)
	  val lr' = R.take (lr, R.length sr)
	  val _ = if R.length lr' <> R.length sr then raise Fail "bogus" else ()
	  val rres = R.Pair.mapEq (op +) (lr', sr)
	  val lres = ListPair.map (op +) (R.toList lr', R.toList sr)
      in
	  if ListPair.allEq (op =) (R.toList rres, lres) then () else raise Fail "testmappair"
      end

  val nTests = 10
  val maxInputSzD = 12
  val tests = [testmap, testreduce, testfilter, testtabulate, testscan, testmappair]

  fun testall () = ignore (
      List.tabulate (nTests, fn _ => 
	List.tabulate (maxInputSzD, fn d => 
          List.app (fn t => t d) tests)))

  fun toList v = Vector.foldr (fn (x, ls) => x :: ls) [] v

  fun testupdate n =
      let val r1 = R.randomRope (randomInt, n, 333)
	  val v1 = Vector.fromList (R.toList r1)
	  val nt = randomInt () mod 1000
	  fun doit (i, v2, r2)  =
	      if i < nt then (v2, r2) else
	      let
		  val x = randomInt ()
		  val i = randomInt () mod (R.length r1)
		  val v2 = Vector.update (v1, i, x)
		  val r2 = R.update (r1, i, x)
	      in
		  doit (i + 1, v2, r2)
	      end
	  val (v2, r2) = doit (0, v1, r1)
	  val is = List.tabulate (R.length r1, fn i => i)
	  fun v2s (i, x) = "("^Int.toString i^","^Int.toString x^")"
	  val rls = R.toList (R.map v2s (R.Pair.zipEq (R.fromList is, r2)))
	  val vls = List.map v2s (ListPair.zip (is, toList v2))
      in
	  if ListPair.allEq (op =) (R.toList r2, toList v2) then () else (
	  print ("v2="^String.concatWith " " vls^"\n");
	  print ("r2="^String.concatWith " " rls^"\n");
	  raise Fail "testupdate")
      end

  fun v2s (i, x) = "("^Int.toString i^","^Int.toString x^")"

  fun testdelete n =
      let
	  fun delete (s, i) = 
	      let
		  fun f j = if j < i then Vector.sub (s, j) else Vector.sub (s, j + 1)
	      in
		  Vector.tabulate (Vector.length s - 1, f)
	      end
	      
	  val r1 = R.randomRope (randomInt, n, 333)
	  val v1 = Vector.fromList (R.toList r1)
	  val nt = randomInt () mod 1000
	  fun doit (i, v2, r2)  =
	      if i < nt then (v2, r2) else
	      let
		  val i = randomInt () mod (R.length r1)
		  val v2 = delete (v1, i)
		  val r2 = R.delete (r1, i)
	      in
		  doit (i + 1, v2, r2)
	      end
	  val (v2, r2) = doit (0, v1, r1)
	  val is = List.tabulate (R.length r1, fn i => i)
	  val rls = R.toList (R.map v2s (R.Pair.zipEq (R.fromList is, r2)))
	  val vls = List.map v2s (ListPair.zip (is, toList v2))
      in
	  if ListPair.allEq (op =) (R.toList r2, toList v2) then () else (
	  print ("v2="^String.concatWith " " vls^"\n");
	  print ("r2="^String.concatWith " " rls^"\n");
	  raise Fail "testdelete")
      end

  fun atoList a = Array.foldr (op ::) [] a

  fun testpermute n =
      let

	  fun randomIx len = if len = 1 then 0 else randomInt () mod len
	  fun scatter (values, ivpairs) =
	      let
		  val a' = Array.array (Array.length values, 0)
		  val _ = Array.copy {di=0, src=values, dst=a'}
		  val _ = Array.app (fn (ix, v) => Array.update (a', ix, v)
handle Subscript => raise Fail (Int.toString (Array.length a')^" "^Int.toString ix)
) ivpairs
	      in
		  a'
	      end

	  fun toList a = Array.foldr (fn (x, ls) => x :: ls) [] a

	  val values = R.randomRope (randomInt, n, 1)
	  val ixps = R.tabulate (randomInt () mod n, fn _ => (randomIx (R.length values), randomInt ()))
	  val a = scatter (Array.tabulate (R.length values, fn i => R.sub (values, i)),
			   Array.tabulate (R.length ixps, fn i => R.sub (ixps, i)))
	  val r = R.Permute.scatter (values, ixps)
	  val valuesstr = R.toList (R.map Int.toString values)
	  val ixpsstr = R.toList (R.map v2s ixps)
	  val rstr = R.toList (R.map Int.toString r)
	  val astr = List.map Int.toString (atoList a)
      in
	  if ListPair.allEq (op =) (toList a, R.toList r) then ()
	  else (
	      print ("values="^String.concatWith " " valuesstr^"\n");
	      print ("ixps="^String.concatWith " " ixpsstr^"\n");
	      print ("a="^String.concatWith " " astr^"\n");
	      print ("r="^String.concatWith " " rstr^"\n");
	      raise Fail "testpermute")
      end

end
