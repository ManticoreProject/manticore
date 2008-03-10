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

  (* moveTo : board -> player * int -> board *)
  fun moveTo b (p,i:int) = 
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

  (* deepen : board gtree * player -> board gtree *)  
  (* The given player is the one who gets to make the next move. *)
  fun deepen (t, p) = 
        (case t
	   of Leaf b => if isWinFor b (other p) then t 
			else if isCat b then t
			else let
                          val nextMoves = map (fn i => moveTo b (p, i)) (allMoves b)
			  val trees = map Leaf nextMoves
			  val deepTrees = map (fn t => deepen (t, other p)) trees
			in
		          Node (b, deepTrees)
			end
	    | Node _ => raise Fail "shouldn't call deepen on a node")

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

  (*
  (* minimaxVal : board gtree * player -> int *)
  fun minimaxVal (t : board gtree, p : player) : int =
        (case t
	   of Leaf b => score b
	    | Node (b, ts) => 
	        let val ts' = map (fn t => minimaxVal (t, other p)) ts
		in
		    case p
		      of X => listmax ts'
		       | O => listmin ts'
		end)

  (* minimax1 *)
  fun minimax1 () =
      let val b0 = empty
	  val ts = map (fn i => moveTo b0 (X, i)) (allMoves b0)
	  val ts' = map ((fn t => deepen (t, O)) o Leaf) ts
      in
	  print (Int.toString (size (Node (b0, ts'))));
	  print " nodes in the full minimax tree.\n";
	  map (fn t => (top t, minimaxVal(t,X))) ts'
      end
  *)

  (* I think you actually want to build the tree and score it at the same time. *)
  (* p is the player to move *)
  (* X is max, O is min *)
  fun minimax' (b : board, p : player) : (board * int) gtree =
        if isWin b orelse isCat b then Leaf (b, score b)
	else 
	    let fun moveTo' i = moveTo b (p, i)
                fun minimax'' b = minimax' (b, other p)
                fun select2 (_, s) = s
	        val moves = map moveTo' (allMoves b)
		val trees = map minimax'' moves
		val scores = map (select2 o top) trees
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
    val t = minimax' (empty, X)
    in
      (top(t), size(t))
    end

  (* alpha-beta pruning *)
  (* TBD *)
  
end
