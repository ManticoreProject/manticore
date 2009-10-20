structure TicTacToe = struct

(* NOTE: this benchmark is very sensitive to the choice of max leaf size. In fact,
 * any leaf size less than the max number of choices of moves (which is 9 for tic tac toe)
 * will result in sequential execution.
 *)

  val mapP = (fn f => fn xs => mapP (f, xs))
  val foldP = (fn f => fn id => fn xs => reduceP (f, id, xs))
  fun maxP xs = reduceP (fn (x, currMax) => if x > currMax then x else currMax, subP(xs,0), xs)
  fun minP xs = reduceP (fn (x, currMax) => if x < currMax then x else currMax, subP(xs,0), xs)
(* admittedlythis is stupid, we instead want some short-circuiting version of allP *)
  fun allP f xs = lengthP (filterP (f, xs)) = lengthP xs

  fun compose (f, g) x = f(g x)

  datatype player = X | O

  (* other : player -> player *)
  fun other p = (case p 
		  of X => O
		   | O => X)

  type board = player option parray (* of length 9 *)

  (* 0 1 2
     3 4 5
     6 7 8 *)

  val rows  = (0::1::2::nil) :: (3::4::5::nil) :: (6::7::8::nil) :: nil
  val cols  = (0::3::6::nil) :: (1::4::7::nil) :: (2::5::8::nil) :: nil
  val diags = (0::4::8::nil) :: (2::4::6::nil) :: nil

  datatype 'a rose_tree (* general tree *)
    = Rose of 'a * ('a rose_tree parray)

  (* top : 'a rose_tree -> 'a *)
  fun top (Rose (x, _)) = x

  (* isOccupied : board * int -> bool *)
  fun isOccupied (b,i) = isSome(subP(b,i))

  (* playerEq : player * player -> bool *)
  fun playerEq (p1, p2) =
      (case (p1, p2)
	of (X, X) => true
	 | (O, O) => true
	 | _ => false)

  (* playerOccupies : player -> board -> int -> bool *)
  fun playerOccupies p b i = 
        (case subP (b, i)
	   of SOME(p') => playerEq (p, p')
	    | NONE => false)

  (* hasTrip : board * player -> int list -> bool *)
  fun hasTrip (b, p) t = List.all (playerOccupies p b) t

  (* hasRow : board * player -> bool *)
  fun hasRow (b, p) = List.exists (hasTrip (b, p)) rows

  (* hasCol : board * player -> bool *)
  fun hasCol (b, p) = List.exists (hasTrip (b, p)) cols

  (* hasDiag : board * player -> bool *)
  fun hasDiag (b, p) = List.exists (hasTrip (b, p)) diags

  (* isFull : board -> bool *)
  fun isFull b = allP isSome b

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

  (* gameOver : board -> bool *)
  fun gameOver b = isWin(b) orelse isCat(b)

  (* putAt : 'a * 'a parray * int -> 'a parray *)
  fun putAt (x, xs, i) =
        tabP (lengthP xs,
	     fn j => if (j=i) then
			  x
		      else
			  subP(xs,j))

  (* moveTo : (board * player) -> int -> board *)
  fun moveTo (b : board, p : player) (i:int) = 
        if isOccupied(b,i) then
	  raise Fail "illegal move"
	else
	  putAt (SOME(p), b, i)

  type game_tree = (board * int) rose_tree

  (* allMoves : board -> int parray *)
  fun allMoves b = let
        fun f n =
	    if n = lengthP b then
		[| |]
	    else
		(case subP(b,n)
		  of SOME _ => f(n+1)
		   | NONE => concatP([| n |], f(n+1)))
        in
          f 0
        end

  (* successors : board * player -> board parray *)
  (* A list of all possible successor states given a board and a player to move. *)
  fun successors (b : board, p : player) : board parray = mapP (moveTo (b, p)) (allMoves b)

  (* minimax : player -> board -> game_tree *)
  (* Build the tree and score it at the same time. *)
  (* p is the player to move *)
  (* X is max, O is min *)
  fun minimax (p : player) (b : board) : game_tree =
        if gameOver(b) then
	  Rose ((b, score b), [| |])
	else let 
          val trees = mapP (minimax (other p)) (successors (b, p))
	  val scores = mapP (compose (snd, top)) trees
	  in
		case p
		 of X => Rose ((b, maxP scores), trees)
		  | Y => Rose ((b, minP scores), trees)
	  end

end

structure Main =
  struct

  structure T = TicTacToe

    fun main (_, args) =
	let
	    fun doit () =
		let
		    val empty : T.board = tabP (9, fn _ => NONE)
		in
		    T.minimax T.X empty
		end
	in
	    RunPar.run doit
	end

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
