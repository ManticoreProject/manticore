structure TicTacToe : GAME = struct

  datatype player = X | O
  type board = player option list

  type state = board
  type operator = board -> board

  fun apply (oper, s) = oper s

  (* 0 1 2
     3 4 5 
     6 7 8 *)

  val initial_state = List.tabulate (9, fn _ => NONE : player option)    

  fun player_eq (X, X) = true
    | player_eq (O, O) = true
    | player_eq _ = false

  fun count (p, b) = let
    fun ++ (optPlayer, n) = 
     (case optPlayer
       of SOME p' => if player_eq(p, p') then n+1 else n
	| NONE => n)
    in
      foldl ++ 0 b
    end

  fun whose_turn b =
    if (count (X, b) = count (O, b)) then X
    else if (count (X, b) - 1 = count (O, b)) then O
    else raise Fail "board is in an illegal state"

  fun max_move b = (case whose_turn b of X => true | _ => false)
  fun min_move b = not (max_move b)

  fun empties b = let
    fun loop ([], _, acc) = acc
      | loop (SOME(_)::t, n, acc) = loop (t, n+1, acc)
      | loop (NONE::t, n, acc) = loop (t, n+1, n::acc)
    in
      loop (b, 0, [])
    end

  fun replace (y, 0, x::xs) = y::xs
    | replace (y, n, x::xs) = x::(replace(y, n-1, xs))
    | replace _ = raise Fail "bug"

  fun move (p, i, b) = replace (SOME p, i, b)

  fun legal_moves b = let
    val next_mover = whose_turn b
    in
      map (fn i => (fn b => move (next_mover, i, b))) (empties b)
    end

  fun playerAt (i, b) = List.nth (b, i)

  fun has_all ((i1,i2,i3), p, b) =
   (case (playerAt(i1,b), playerAt(i2,b), playerAt(i3,b))
     of (SOME(p1), SOME(p2), SOME(p3)) => 
          player_eq(p,p1) andalso player_eq(p,p2) andalso player_eq(p,p3)
      | _ => false
    (* end case *))

  (* 0 1 2
     3 4 5 
     6 7 8 *)

  val rows  = [(0,1,2),(3,4,5),(6,7,8)]
  val cols  = [(0,3,6),(1,4,7),(2,5,8)]
  val diags = [(0,4,8),(2,4,6)]

  fun has_row  (p, b) = List.exists (fn trip => has_all (trip, p, b)) rows
  fun has_col  (p, b) = List.exists (fn trip => has_all (trip, p, b)) cols
  fun has_diag (p, b) = List.exists (fn trip => has_all (trip, p, b)) diags

  fun winner b = 
    if has_row (X, b) orelse has_col (X, b) orelse has_diag (X, b) then SOME X
    else if has_row (O, b) orelse has_col (O, b) orelse has_diag (O, b) then SOME O
    else NONE

  fun utility b = 
   (case winner b 
     of NONE => 0
      | SOME X => 1
      | SOME O => ~1
    (* end case *))

  fun terminal_test b = (case winner b of SOME _ => true | _ => false) 
	 		 orelse ((length (empties b)) = 0)

  fun successors s = map (fn m => m s) (legal_moves s)

  fun state_eq (b1, b2) = let
    fun eq ([], []) = true
      | eq (NONE::t1, NONE::t2) = eq (t1, t2)
      | eq (SOME(p)::t1, SOME(q)::t2) = player_eq(p,q) andalso eq(t1,t2)
      | eq _ = false
    in
      eq (b1, b2)
    end

  val neg_inf = ~2
  val pos_inf =  2

end
