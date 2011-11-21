structure TRP = struct

  val r = Random.rand (~2343333, 2343)
  fun roll n = Random.randNat r mod n

  val x = 100
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
      val splittingStrategy = LBS{PPT=3}
      fun forkjoin (f, g) = (f (), g ())
      val parMap = List.map
      fun numAvailProcs () = 1
      fun otherHungryProcs () = get ()
      fun workerId () = 0
    end

  structure R = RopeFn (
		   structure S = VectorSeq
		   val maxLeafSize = 2
		   structure RT = DummyLBS
		)

  val r = Random.rand (~2343333, 2343)
  fun roll n = Random.randNat r mod n
  fun randomInt () = roll 1000

  fun testmap (n : int) =
      let val r1 = R.randomRope (randomInt, n, 333)
	  val r2 = R.randomRope (randomInt, n, 13)
	  val (lr, sr) = if R.length r1 > R.length r2 then (r1, r2) else (r2, r1)
	  val lr' = R.take (lr, R.length sr)
	  val rres = R.Pair.mapEq (op +) (lr', sr)
	  val lres = ListPair.map (op +) (R.toList lr', R.toList sr)
      in
	  if ListPair.allEq (op =) (R.toList rres, lres) then () else raise Fail "testmap"
      end

end
