structure TicTacToe = struct

  datatype player = X | O

  type board = player option list (* of length 9 *)

  (* 0 1 2
     3 4 5
     6 7 8 *)

  val empty : board = [NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE]

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

  (* FIXME rose tree *)

  datatype 'a gtree (* general tree *)
    = Leaf of 'a
    | Node of 'a * ('a gtree list)

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

  (* top : 'a gtree -> 'a *)
  fun top (Leaf x) = x
    | top (Node (x, _)) = x

  (* size : 'a gtree -> int *)
  fun size (Leaf _) = 1
    | size (Node (x, ts)) = 1 + (foldl op+ 0 (map size ts))

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
    
  (* minimax : player -> board -> (board * int) gtree *)
  (* Build the tree and score it at the same time. *)
  (* p is the player to move *)
  (* X is max, O is min *)
  fun minimax (p : player) (b : board) =
        if gameOver(b) then
	  Leaf (b, score b)
	else let 
          val trees = map (minimax (other p)) (successors (b, p))
	  val scores = map (#2 o top) trees
	  in
	    case p
             of X => Node ((b, listmax scores), trees)
	      | O => Node ((b, listmin scores), trees)
	  end

  (* DEBUGGING *)	

  fun addMap ([], ns) = ns
    | addMap (ns, []) = ns
    | addMap (n::ns, m::ms) = (n+m)::addMap(ns,ms)

  fun pathLengths (t : 'a gtree) : int list =
    (case t
      of Leaf _ => [1]
       | Node (_, ts) => 0 :: (foldl addMap [] (map pathLengths ts)))

  fun go () = let
    val t = minimax X empty
    in
      (top(t), size(t))
    end

  fun max (m:int, n) = (if m>n then m else n)

  fun min (m:int, n) = (if m<n then m else n)

  (* FIXME -- BROKEN! *)
  (* build a tree with alpha-beta pruning *)
  fun alpha_beta () = let
    fun max_tree (b, alpha, beta) =
      if gameOver(b) then
        Leaf (b, score b)
      else let
        fun loop ([], al, ts) = Node ((b, al), ts)
	  | loop (s::ss, al, ts) = let
              val t = min_tree (s, al, beta)
              val al' = max (al, #2 (top t))
              in
	        if (al' >= beta) then 
                  Node ((b, al'), t::ts)
                else 
                  loop (ss, al', t::ts)
	      end
        in
          loop (successors (b, X), alpha, [])
        end     
    and min_tree (b, alpha, beta) =
      if gameOver(b) then
        Leaf (b, score b)
      else let
        fun loop ([], bt, ts) = Node ((b, bt), ts)
	  | loop (s::ss, bt, ts) = let
              val t = max_tree (s, alpha, bt)
              val bt' = min (bt, #2 (top t))
              in
	        if (bt' <= alpha) then 
	          Node ((b, bt'), t::ts) 
                else 
		  loop (ss, bt', t::ts)
	      end
        in
          loop (successors (b, O), beta, [])
        end
      val neg_inf = ~1
      val pos_inf = 1
    in
      max_tree (empty, neg_inf, pos_inf)
    end

  fun btos b = let
    fun stos NONE = " "
      | stos (SOME X) = "X"
      | stos _ = "O"
    fun build ([], _, acc) = acc
      | build (s::ss, n, acc) = let
          val sep = if (n=2) orelse (n=5) then 
		      "\n------\n" 
		    else if (n=8) then
                      "\n"
                    else
                      "|"
          in
            build (ss, n+1, acc ^ (stos s) ^ sep)
          end
    in
      build (b, 0, "")
    end

  fun left_spine (Leaf x) = [x]
    | left_spine (Node (x, [])) = [x]
    | left_spine (Node (x, t::_)) = x::left_spine(t)

  fun goAB () = let
    val t = alpha_beta ()
    val foo = map (fn (b, i) => (btos b, i)) (left_spine t)
    fun println s = (print s; print "\n")
    fun pr (s, n) = (print s;
		     println ("\t" ^ "\t" ^ (Int.toString n));
		     print "\n")
    in
      app pr foo;
      (top(t), size(t))
    end  
  
  fun fringeAB () = let
    fun fringe (Leaf x) = [x]
      | fringe (Node (x, [])) = [x]
      | fringe (Node (_, ts)) = List.concat (map fringe ts)
    val t = alpha_beta ()
    fun println s = (print s; print "\n")
    fun pr (s, n) = (print s;
		     println ("\t" ^ "\t" ^ (Int.toString n));
		     print "\n")
    in
      app pr (map (fn (b,i) => (btos b, i)) (fringe t))
    end    

end
