structure Chk = struct

local
    val r = ref 0w324324
    val ran = Random.rand (0, 10000000)
in
fun flip n = 
(*
  (r := Rand.random (!r);
   Rand.range (0, n) (!r) = 0)
*)
Random.randNat ran mod 20 = 0
fun rand () = 
(* note: uncommenting the code below causes mlton compilation to crash *)
(*
  (r := Rand.random (!r);
   Rand.range (0, Option.getOpt (Int.maxInt, 1000000)) (!r))
*)
Random.randNat ran 
fun randRange (x, y) = Rand.range (x, y) (!r)
end

local
fun randHP () = flip 4
fun noneHP () = false
fun everyHP () = true
val determHP = let
      val p = 3
      val n = ref p
      in
	fn () => let
	  val x = !n - 1
	  in
	    if x < 0 then false
	    else if x > 0 then (n := x; false)
	    else (n := p; true)
	  end
      end
in
structure R = RopeImplFn (
  structure Seq = VectorSeq
  structure RT = struct
    fun numProcs () = 4
    val par2 = fn (f, g) => (f (), g ())
    val parN = fn l => List.map (fn f => f ()) l
    val hungryProcs = randHP
  end
  val C = 2.0)
end (* local *)

val balanceFactor = ref 0.2

fun randRope leaf len = let
  val balanceFactor = !balanceFactor
  fun r targetSize =
    if targetSize <= real (LeafSize.getMax ()) then 
      R.Leaf (R.Seq.tabulate (LeafSize.getMax (), fn _ => leaf ()))
    else let
      val factor = balanceFactor * real (randRange (0, 100)) / 100.0
      val targetSize1 = if (factor * targetSize) < 1.0 then 1.0 else factor * targetSize
      val targetSize2 = targetSize - targetSize1
      in
	R.nccat2 (r targetSize1, r targetSize2)
      end
  in
    r (real len)
  end

(*
fun randCur rp = (case rp
  of R.Leaf x => (rp, R.GCTop)
   | R.Cat (l, d, rp1, rp2) => 
*)

fun sameStructure (R.Leaf _, R.Leaf _) = true
  | sameStructure (R.Cat (_, _, l1, r1), R.Cat (_, _, l2, r2)) =
      sameStructure (l1, l2) andalso sameStructure (r1, r2)
  | sameStructure _ = false

local
  fun withChunkingPolicy s name f = let
      val orig = ChunkingPolicy.get ()
      val _ = ChunkingPolicy.set s;
      val x = f ()
      in
	ChunkingPolicy.set orig;
	x
      end
in
fun withSequential f = withChunkingPolicy ChunkingPolicy.Sequential "sequential" f
fun withLTS f = withChunkingPolicy (ChunkingPolicy.LTS 1) "lts" f
fun withETS SST f = withChunkingPolicy (ChunkingPolicy.ETS SST) "ets" f
end

fun withLeafSize s f = let
  val orig = LeafSize.getMax ()
  val _ = LeafSize.setMax s;
  val x = f ()
  in
    LeafSize.setMax orig;
    x
  end

val (orig, seq, test) = (ref (R.singleton 1), ref (R.singleton 2), ref (R.singleton 3))

fun vorig () = R.toList (!orig)
fun vseq () = R.toList (!seq)
fun vtest () = R.toList (!test)

fun error s = print ("!!!!error: "^s)

fun notequal () = raise Fail "not equal"

fun testTab n = let
  val r = R.tabulate (n, fn i => i)
  val _ =     orig := r;
  val r' = withSequential (fn () => R.tabulate (n, fn i => i))
  val _ =     seq := r';
  val alleq = ListPair.allEq (fn (x, y) => x = y) (R.toList r, R.toList r')
  in
    test := r;
    if not alleq then
	notequal ()
    else
	();
    ()
  end

fun testMap d = let
  val r = randRope (fn _ => rand () mod 100) d
  val _ =     orig := r;
  val r'' = withSequential (fn () => R.map (fn x => x + 1) r)
  val _ =     seq := r'';;
  val r' = R.map (fn x => x + 1) r
  val alleq = ListPair.allEq (fn (x, y) => x = y) (R.toList r'', R.toList r')
  val ss = sameStructure (r, r')
  in
    seq := r'';
    test := r';
    if not alleq then
	notequal ()
    else ();
    if not ss andalso LeafSize.getMax () = 1 then
	error "different structures\n"
    else
	();
    ()
  end

fun testReduce d = let
  val r = randRope (fn _ => rand () mod 100) d
  val s = withSequential (fn () => R.reduce (op +) 0 r)
  val p = R.reduce (op +) 0 r
  in
    if s <> p then
      notequal ()
    else ()
  end

fun testFilter d = let
  val r = randRope (fn _ => rand () mod 100) d
  fun isEven x = x mod 2 = 0
  val _ =     orig := r;
  val r'' = withSequential (fn () => R.filter isEven  r)
  val _ =     seq := r'';
  val r' = R.filter isEven r
  val alleq = ListPair.allEq (fn (x, y) => x = y) (R.toList r'', R.toList r')
  val ss = sameStructure (r'', r')
  in
    seq := r'';
    test := r';
    if not alleq then
	notequal ()
    else ();
(*    if not ss then
	error "different structures\n"
    else
	();
*)
    ()
  end

fun testScan d = let
  val r = randRope (fn () => 1) d
  val _ =     orig := r;
  val r'' = withSequential (fn () => R.scan (fn (x, y) => x + y) 0 r)
  val _ =       seq := r'';
  val r' = R.scan (fn (x, y) => x + y) 0 r
  val alleq = ListPair.allEq (fn (x, y) => x = y) (R.toList r'', R.toList r')
  val ss = sameStructure (r, r')
  in
    test := r';
    if not alleq then
	error "not equal\n"
    else ();
    if not ss andalso LeafSize.getMax () = 1 then
	error "different structures\n"
    else
	();
    ()
  end

fun testAll iters ns = let
  fun test n = (
        print ("  size = "^Int.toString n^"\n");
        print "map\n";
        testMap n;
        print "reduce\n";
	testReduce n;
        print "filter\n";
	testFilter n;
        print "tab\n";
	testTab n;
        print "scan\n";
	testScan n;

 () )
  fun iter i =
        if i < iters then (
	  List.app test ns;
	  iter (i + 1))
	else
	  ()
  fun doit () = (
    print "---sequential\n";
    withSequential (fn _ => iter 0);
    print "---ets\n";
    withETS 3 (fn _ => iter 0);
    print "---lts\n";
    withLTS (fn _ => iter 0))
  in
    withLeafSize 1 (fn _ => (
       print ("  leaf size "^Int.toString (LeafSize.getMax ()));
       doit ()));
    withLeafSize 3 (fn _ => (
       print ("  leaf size "^Int.toString (LeafSize.getMax ()));
       doit ()));
    withLeafSize 128 (fn _ => (
       print ("  leaf size "^Int.toString (LeafSize.getMax ()));
       doit ()));
    ()
  end


val (origUp, seqUp, testUp) = (ref (R.singleton 1), ref (R.mcsingleton 2), ref (R.mcsingleton 3))

fun sameStructureMC (R.MCLeaf _, R.MCLeaf _) = true
  | sameStructureMC (R.MCCat (_, _, _, l1, r1), R.MCCat (_, _, _, l2, r2)) =
      sameStructureMC (l1, l2) andalso sameStructureMC (r1, r2)
  | sameStructureMC _ = false

fun testUpsweep d = let
  val r = randRope (fn () => 1) d
  val _ =     origUp := r;
  val r'' = withSequential (fn () => R.upsweepSequential (fn (x, y) => x + y) 0 r)
  val _ =       seqUp := r'';
  val r' = R.upsweepLTS 1 (fn (x, y) => x + y) 0 r
  val alleq = ListPair.allEq (fn (x, y) => x = y) (R.toListMC r'', R.toListMC r')
  val ss = sameStructureMC (r'', r')
  in
    if not alleq then
	print "not equal\n"
    else ();
    if LeafSize.getMax () = 1 andalso not ss then
	print "different structures\n"
    else
	();
    testUp := r'
  end

fun testDownsweep d = let
  val rorig = randRope (fn () => 1) d
  val r = R.upsweepSequential (op +) 0 rorig
  val _ =     orig := rorig;
  val r'' = withSequential (fn () => R.downsweepSequential (op +) 0 r)
  val _ =       seq := r'';
  val r' = R.downsweepLTS 1 (op +) 0 r
  val alleq = ListPair.allEq (fn (x, y) => x = y) (R.toList r'', R.toList r')
  val ss = sameStructure (r'', r')
  in
    if not alleq then
	print "not equal\n"
    else ();
    if LeafSize.getMax () = 1 andalso not ss then
	print "different structures\n"
    else
	();
    test := r'
  end

fun mcrope rp = (case rp
  of R.Leaf s => R.MCLeaf (0, s)
   | R.Cat (l, d, rp1, rp2) => R.MCCat (0, l, d, mcrope rp1, mcrope rp2))

fun testCursorAtIx len = let
  val rp = randRope rand len
  fun check i = let
      val f = R.finish R.nccat2 (R.zipCursor (R.cursorAtIx (rp, i)))
      val alleq = ListPair.allEq (fn (x, y) => x = y) (R.toList rp, R.toList f)
      val ss = sameStructure (rp, f)
      in
      if not alleq then
	  print "not equal\n"
      else ();
      if not ss then
	  print "different structures\n"
      else
	  ();
      1
      end
  in
    List.tabulate (R.length rp, check);
    ()
  end

fun testSplitJoin len = let
  val rp = randRope rand len
  fun index i = let
    val cur = R.zipCursor (R.cursorAtIx (rp, i))
    fun splt n = let
       val n = n + 1
      val splt = R.splitAt R.length R.encodeRope R.cursorAtIx R.unzipCursor R.unzipCursor cur n
      val jn = R.finish R.nccat2 (R.join R.decodeRope (R.finish R.nccat2) R.zipCursor splt)
      val alleq = ListPair.allEq (fn (x, y) => x = y) (R.toList rp, R.toList jn)
      val ss = sameStructure (rp, jn)
      in
	if not alleq then
	    print "not equal\n"
	else ();
	if not ss then
	    print "different structures\n"
	else
	    ();
	()	
      end
    val _ = List.tabulate (R.numUnprocessed R.length R.length cur - 1, splt)
    in
      ()
    end
  val _ = List.tabulate (len - 2, index)
  in
    ()
  end

fun testSplitJoinMC len = let
  val rp = R.upsweepSequential (op +) 0 (randRope (fn _ => 1) len)
  fun index i = let
    val cur = R.zipCursor (R.cursorAtIxMC (op +) 0 (rp, i))
    fun splt n = let
       val n = n + 1
      val splt = R.splitAt R.mclength (R.encodeMCRope (op +)) (R.cursorAtIxMC (op +) 0) R.unzipCursor R.unzipCursor cur n
      val jn = R.finish (R.mcnccat2' (op +)) (R.join R.decodeMCRope (R.finish (R.mcnccat2' (op +))) R.zipCursor splt)
      val alleq = ListPair.allEq (fn (x, y) => x = y) (R.toListMC rp, R.toListMC jn)
      val ss = sameStructureMC (rp, jn)
      in
	if not alleq then
	    print "not equal\n"
	else ();
	if not ss then
	    print "different structures\n"
	else
	    ();
	()	
      end
    val _ = List.tabulate (R.numUnprocessed R.mclength R.mclength cur - 1, splt)
    in
      ()
    end
  val _ = List.tabulate (len - 2, index)
  in
    ()
  end

fun doit () = testAll 4 [15, 100, 1000, 10000]

val _ = doit ()

end
