structure TicTacToe = struct

  fun compose (f, g) x = f(g x)
  val hd = List.hd
  val tl = List.tl
  fun cut (s, n) = (List.take (s, n), List.drop (s, n))
  fun parMap (f, xs) =
      (case xs
	of nil => nil
	 | x :: nil => f x :: nil
	 | xs => 
	   let
	       val (xs1, xs2) = cut (xs, List.length xs div 2)
	       val (xs1', xs2') = (| parMap (f, xs1), parMap (f, xs2) |)
	   in
	       xs1' @ xs2'
	   end)

  datatype player = X | O

  type board = player option list (* of length 9 *)

  (* 0 1 2
     3 4 5
     6 7 8 *)

  val empty : board = List.tabulate (9, fn _ => NONE)

  (* isOccupied : board * int -> bool *)
  fun isOccupied (b,i) = isSome(List.nth(b,i))

  (* isEmpty : board * int -> bool *)
  fun isEmpty (b, i) = not (isOccupied (b, i))

  (* find : board * int -> player option *)
  fun find (b, i) = List.nth (b, i)

  (* playerEq : player * player -> bool *)
  fun playerEq (p1, p2) =
      (case (p1, p2)
	of (X, X) => true
	 | (O, O) => true
	 | _ => false)

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
	    raise Fail "subscript"

  (* moveTo : (board * player) -> int -> board *)
  fun moveTo (b : board, p : player) (i:int) = 
        if isOccupied(b,i) then
	  raise Fail "illegal move"
	else
	  putAt (SOME(p), b, i)

  val rows  = (0::1::2::nil) :: (3::4::5::nil) :: (6::7::8::nil) :: nil
  val cols  = (0::3::6::nil) :: (1::4::7::nil) :: (2::5::8::nil) :: nil
  val diags = (0::4::8::nil) :: (2::4::6::nil) :: nil

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
  fun mkLeaf x = Rose (x, nil)

  type game_tree = (board * int) rose_tree

  (* allMoves : board -> int list *)
  fun allMoves b = 
      let fun f (n, m, acc) =
	      (case m
		of nil => rev acc
		 | SOME(_)::more => f (n+1, more, acc)
		 | NONE::more => f (n+1, more, n::acc))
      in
	  f (0, b, nil)
      end

  (* other : player -> player *)
  fun other p = (case p 
		  of X => O
		   | O => X)

  (* top : 'a rose_tree -> 'a *)
  fun top (Rose (x, _)) = x

  (* size : 'a rose_tree -> int *)
  fun size (Rose (x, ts)) = 1 + (foldl (fn (x,y) => x+y) 0 (map size ts))

  (* listExtreme : (('a * 'a) -> 'a) -> 'a list -> 'a *)
  fun listExtreme e ns = 
      (case ns
	of nil => (raise Fail "undefined on empty lists")
	 | (n::ns) => foldl e n ns)
      
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
          val trees = parMap (minimax (other p), (successors (b, p)))
	  val scores = map (compose (snd, top)) trees
	  in 
             case p
	      of X => Rose ((b, listmax scores), trees)
	       | Y => Rose ((b, listmin scores), trees)
	  end

end

structure Main =
  struct

  structure T = TicTacToe

    fun main (_, args) =
	let
	    fun doit () = T.minimax T.X T.empty
	in
	    RunPar.run doit
	end

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
