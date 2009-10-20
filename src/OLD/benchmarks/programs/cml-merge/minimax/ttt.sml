structure TicTacToe = struct

  datatype player = X | O

  type board = player option list (* of length 9 *)

  (* 0 1 2
     3 4 5
     6 7 8 *)

  val empty : board = List.tabulate (9, fn _ => NONE)

  exception IllegalMove

  (* isOccupied : board * int -> bool *)
  fun isOccupied (b,i) = isSome(List.nth(b,i))

  (* isEmpty : board * int -> bool *)
  fun isEmpty (b, i) = not (isOccupied (b, i))

  (* find : board * int -> player option *)
  fun find (b, i) = List.nth (b, i)

  (* playerEq : player * player -> bool *)
  fun playerEq (X, X) = true
    | playerEq (O, O) = true
    | playerEq _ = false

  (* playerOccupies : player -> board -> int -> bool *)
  fun playerOccupies p b i = 
        (case find (b, i)
	   of SOME(p') => playerEq (p, p')
	    | NONE => false)

  (* putAt : 'a * 'a list * int -> 'a list *)
  fun putAt (x, xs, i) =
        if (i=0) then
	    x::tl(xs)
	else if (i>0) then
	    hd(xs)::putAt(x,tl(xs),i-1)
	else (* i<0 *)
	    raise Subscript

  (* moveTo : (board * player) -> int -> board *)
  fun moveTo (b : board, p : player) (i:int) = 
        if isOccupied(b,i) then
	  raise IllegalMove
	else
	  putAt (SOME(p), b, i)

  val rows  = [[0,1,2],[3,4,5],[6,7,8]]
  val cols  = [[0,3,6],[1,4,7],[2,5,8]]
  val diags = [[0,4,8],[2,4,6]]

  (* hasTrip : board * player -> int list -> bool *)
  fun hasTrip (b, p) t = List.all (playerOccupies p b) t

  (* hasRow : board * player -> bool *)
  fun hasRow (b, p) = List.exists (hasTrip (b, p)) rows

  (* hasCol : board * player -> bool *)
  fun hasCol (b, p) = List.exists (hasTrip (b, p)) cols

  (* hasDiag : board * player -> bool *)
  fun hasDiag (b, p) = List.exists (hasTrip (b, p)) diags

  (* isFull : board -> bool *)
  fun isFull b = List.all isSome b

  (* isWinFor : board -> player -> bool *)
  fun isWinFor b p = hasRow(b,p) orelse hasCol(b,p) orelse hasDiag(b,p)

  (* isWin : board -> bool *)
  fun isWin b = isWinFor b X orelse isWinFor b O

  (* isCat : board -> bool *)
  fun isCat b = isFull b andalso not (isWinFor b X) 
                         andalso not (isWinFor b O) (* X moves last *)

  (* score : board -> int *)
  (* -1 if O wins, 1 if X wins, 0 otherwise. *)
  (* This coarse heuristic function suffices b/c we can build the *whole* tree. *)
  fun score b = if isWinFor b X then 1 else if isWinFor b O then ~1 else 0

  datatype 'a rose_tree (* general tree *)
    = Rose of 'a * ('a rose_tree list)

  (* mkLeaf : 'a -> 'a rose_tree *)
  fun mkLeaf x = Rose (x, [])

  type game_tree = (board * int) rose_tree

  (* allMoves : board -> int list *)
  fun allMoves b = 
      let fun f (_, [], acc) = rev acc
	    | f (n, SOME(_)::more, acc) = f (n+1, more, acc)
	    | f (n, NONE::more, acc) = f (n+1, more, n::acc)
      in
	  f (0, b, [])
      end

  (* other : player -> player *)
  fun other X = O
    | other O = X

  (* top : 'a rose_tree -> 'a *)
  fun top (Rose (x, _)) = x

  (* size : 'a rose_tree -> int *)
  fun size (Rose (x, ts)) = 1 + (foldl op+ 0 (map size ts))

  (* listExtreme : (('a * 'a) -> 'a) -> 'a list -> 'a *)
  fun listExtreme e (n::ns) = foldl e n ns
    | listExtreme _ _ = raise Fail "undefined on empty lists"

  (* listmax : int list -> int *)
  val listmax = listExtreme (fn (a:int, b) => if (a>b) then a else b)

  (* listmax : int list -> int *)
  val listmin = listExtreme (fn (a:int, b) => if (a<b) then a else b)

  (* gameOver : board -> bool *)
  fun gameOver b = isWin(b) orelse isCat(b)

  (* successors : board * player -> board list *)
  (* A list of all possible successor states given a board and a player to move. *)
  fun successors (b : board, p : player) : board list = map (moveTo (b, p)) (allMoves b)
    
  (* minimax : player -> board -> game_tree *)
  (* Build the tree and score it at the same time. *)
  (* p is the player to move *)
  (* X is max, O is min *)
  fun minimax (p : player) (b : board) : game_tree =
        if gameOver(b) then
	  mkLeaf (b, score b)
	else let 
          val trees = map (minimax (other p)) (successors (b, p))
	  val scores = map (#2 o top) trees
	  val selectFrom = (case p of X => listmax | O => listmin)
	  in
		Rose ((b, selectFrom scores), trees)
	  end

  (* DEBUGGING *)	

  (* addMaps : int list * int list -> int list *)
  fun addMaps ([], ms) = ms
    | addMaps (ns, []) = ns
    | addMaps (n::ns, m::ms) = (n+m)::addMaps(ns,ms)

  (* pathLengths : 'a rose_tree -> int list *)
  fun pathLengths (t : 'a rose_tree) : int list =
    (case t
      of Rose (x, []) => [1]
       | Rose (_, ts) => 0 :: (foldl addMaps [] (map pathLengths ts)))

  (* go : unit -> (board * int) * int *)
  fun go () = let
    val gettimeofday = Time.toReal o Time.now
    val t0 = gettimeofday ()
    val tree = minimax X empty
    val t1 = gettimeofday ()
    in
      print (Real.toString (t1-t0));
      print " seconds to build the tree\n";
      (top(tree), size(tree))
    end

  (* max : int * int -> int *)
  fun max (m:int, n) = (if m>n then m else n)

  (* min : int * int -> int *)
  fun min (m:int, n) = (if m<n then m else n)

  (* build a tree with alpha-beta pruning *)
  fun alpha_beta () : game_tree = let
    fun max_tree (b, alpha, beta) =
      if gameOver(b) then
        mkLeaf (b, score b)
      else let
        fun loop ([], al, ts) = Rose ((b, al), ts)
	  | loop (s::ss, al, ts) = let
              val t = min_tree (s, al, beta)
              val al' = max (al, #2 (top t))
              in
	        if (al' >= beta) then 
                  Rose ((b, al'), t::ts)
                else 
                  loop (ss, al', t::ts)
	      end
        in
          loop (successors (b, X), alpha, [])
        end     
    and min_tree (b, alpha, beta) =
      if gameOver(b) then
        mkLeaf (b, score b)
      else let
        fun loop ([], bt, ts) = Rose ((b, bt), ts)
	  | loop (s::ss, bt, ts) = let
              val t = max_tree (s, alpha, bt)
              val bt' = min (bt, #2 (top t))
              in
	        if (bt' <= alpha) then 
	          Rose ((b, bt'), t::ts) 
                else 
		  loop (ss, bt', t::ts)
	      end
        in
          loop (successors (b, O), beta, [])
        end
      val neg_inf = ~2
      val pos_inf = 2
    in
      max_tree (empty, neg_inf, pos_inf)
    end
 
  val mkLeafP = mkLeaf
  val plen = List.length
  val pnil = nil

  (* Manticore-ish version *)
  fun alpha_beta_mant () : game_tree = let
    fun maxT (b, alpha, beta) =
      if gameOver(b) then
        mkLeafP (b, score b)
      else let
        val (s,ss) = (case successors (b, X)
          of s::ss => (s,ss)
           | nil => raise Fail "huh?")
	val t0 = minT (s, alpha, beta)
	val alpha' = max (alpha, #2 (top t0))
	fun minT' s = let
          val t = minT (s, alpha', beta)
          val a = max (alpha, #2 (top t))
          in 
	    if (a >= beta) then NONE
	    else SOME(t)
          end
        val n = plen ss
	fun loop (ss, i) =
          if (i >= n) then SOME pnil
	  else case (minT'(List.nth(ss,i)), loop(ss,i+1))
	    of (NONE, _) => NONE
	     | (_, NONE) => NONE
	     | (SOME ti, SOME ts) => SOME (ti :: ts)
	val children = (case loop (ss, 1)
          of NONE => [t0]
	   | SOME(ts) => t0::ts)
	val maxScore = listmax (map (#2 o top) children)
        in
          Rose ((b, maxScore), children)
        end     
    and minT (b, alpha, beta) =
      if gameOver(b) then
        mkLeafP (b, score b)
      else let
        val (s,ss) = (case successors (b, X)
          of s::ss => (s,ss)
           | nil => raise Fail "huh?")
	val t0 = maxT (s, alpha, beta)
	val beta' = min (beta, #2 (top t0))
	fun maxT' s = let
          val t = maxT (s, alpha, beta')
          val b = min (beta, #2 (top t))
          in 
	    if (b <= alpha) then NONE
	    else SOME(t)
          end
        val n = plen ss
	fun loop (ss, i) =
          if (i >= n) then SOME pnil
	  else case (maxT'(List.nth(ss,i)), loop(ss,i+1))
	    of (NONE, _) => NONE
	     | (_, NONE) => NONE
	     | (SOME ti, SOME ts) => SOME (ti :: ts)
	val children = (case loop (ss, 1)
          of NONE => [t0]
	   | SOME(ts) => t0::ts)
	val minScore = listmin (map (#2 o top) children)
        in
          Rose ((b, minScore), children)
        end     
    val neg_inf = ~2
    val pos_inf = 2
    in
      maxT (empty, neg_inf, pos_inf)
    end

  (* btos : board -> string *)
  fun btos (b : board) : string = let
    fun stos NONE = " "
      | stos (SOME X) = "X"
      | stos _ = "O"
    fun build ([], _, acc) = concat (rev acc)
      | build (s::ss, n, acc) = let
          val sep = if (n=2) orelse (n=5) then 
		      "\n------\n" 
		    else if (n=8) then
                      "\n"
                    else
                      "|"
          in
            build (ss, n+1, sep :: stos s :: acc)
          end
    in
      build (b, 0, [])
    end

  (* left_spine : 'a rose_tree -> 'a list *)
  fun left_spine (Rose (x, [])) = [x]
    | left_spine (Rose (x, t::_)) = x::left_spine(t)

  (* fringe : 'a rose_tree -> 'a list *)
  fun fringe (Rose (x, [])) = [x]
    | fringe (Rose (_, ts)) = List.concat (map fringe ts)

  (* a_path : 'a rose_tree -> 'a list *)
  fun a_path (Rose (x, [])) = [x]
    | a_path (Rose (x, _::_::t::_)) = x :: a_path(t)
    | a_path (Rose (x, _::t::_)) = x :: a_path(t)
    | a_path (Rose (x, t::_)) = x :: a_path(t)

  (* goAB : unit -> (board * int) * int *)
  fun goAB () = let
    val t = alpha_beta ()
    val leftmost = map (fn (b, i) => (btos b, i)) (left_spine t)
    val p = map (fn (b, i) => (btos b, i)) (a_path t)
    fun println s = (print s; print "\n")
    fun pr (s, n) = (print s;
		     println ("\t" ^ "\t" ^ (Int.toString n));
		     print "\n")
    in
      app pr p;
      (top(t), size(t))
    end  
  
  (* fringeAB : unit -> unit *)
  fun fringeAB () = let
    val t = alpha_beta ()
    fun println s = (print s; print "\n")
    fun pr (s, n) = (print s;
		     println ("\t" ^ "\t" ^ (Int.toString n));
		     print "\n")
    in
      app pr (map (fn (b,i) => (btos b, i)) (fringe t))
    end    

end

structure Main =
  struct

    structure T = TicTacToe

    fun main (_, args) =
	let
	    fun doit () = T.minimax T.X T.empty
		
	in
	    RunSeq.run doit;
	    0
	end

  end
