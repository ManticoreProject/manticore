structure TicTacToeArray : GAME = struct

  datatype player = X | O
  type board = player option array

  type state = board (* TODO: Include the last mover explicitly in the state. *)
  type operator = board -> board

  fun apply (oper, s) = oper s

  (* mapAL : Generate a list by systematically applying a function to every
   *         element of an array.
   * mapAL is short for "map array to list"
   * mapAL : ('a -> 'b) -> 'a array -> 'b list *)
  fun mapAL f a = let
    fun loop (~1, acc) = acc
      | loop (n, acc)  = loop (n-1, f (Array.sub (a, n)) :: acc)
    in
      loop (Array.length a - 1, [])
    end    

  (* mapALi : (int * 'a -> 'b) -> 'a array -> 'b list *)
  fun mapALi f a = let
    fun loop (~1, acc) = acc
      | loop (n, acc)  = loop (n-1, f (n, Array.sub (a, n)) :: acc)
    in
      loop (Array.length a - 1, [])
    end

  fun boardToString b = let
    fun f NONE = "."
      | f (SOME X) = "X"
      | f (SOME O) = "O"
    in
      concat (mapAL f b)
    end

  (* 0 1 2
     3 4 5 
     6 7 8 *)

  fun mk_empty () = Array.array (9, NONE : player option)

  val initial_state = mk_empty ()

  fun player_eq (X, X) = true
    | player_eq (O, O) = true
    | player_eq _ = false

  fun count (p, b) = let
    fun ++ (optPlayer, n) = 
     (case optPlayer
       of SOME p' => if player_eq(p, p') then n+1 else n
	| NONE => n)
    in
      Array.foldl ++ 0 b
    end

  fun whose_turn b =
    if (count (X, b) = count (O, b)) then X
    else if (count (X, b) - 1 = count (O, b)) then O
    else raise Fail ("board is in an illegal state: " ^ boardToString b) 

  fun max_move b = (case whose_turn b of X => true | _ => false)
  fun min_move b = not (max_move b)

  (* empties : board -> int list *)
  (* TODO rewrite this as a fold *)
  fun empties b = let
    fun loop (9, acc) = acc 
      | loop (n, acc) = 
         (case Array.sub (b, n)
           of SOME _ => loop (n+1, acc)
            | NONE => loop (n+1, n::acc)
	  (* end case *))
    in
      loop (0, [])
    end

  fun replace (y, n, b) = (Array.update (b, n, y); b)

  fun move (p, i, b) = let
    val b' = Array.array (Array.length b, NONE : player option)
    in
      (Array.copy {src=b, dst=b', di=0};
       replace (SOME p, i, b');
       b')
    end

(*
  fun array_map (f, a) = let
    val a' = Array.array (9, NONE)
    fun f' (n, x) = Array.update (a', n, f x) 
    in
      (Array.copy {src=a, dst=a', di=0};
       Array.appi f' a';
       a')
    end
*)

  val id = fn x => x

  (* legal_moves : state -> operator list *)
  fun legal_moves b = let
    val next_mover = whose_turn b
    fun f i = (fn b => move (next_mover, i, b))
    in
      map f (empties b)
    end

  fun playerAt (i, b) = Array.sub (b, i)

  fun has_all ((i1,i2,i3), p, b) =
   (case (playerAt(i1,b), playerAt(i2,b), playerAt(i3,b))
     of (SOME(p1), SOME(p2), SOME(p3)) => 
          player_eq(p,p1) andalso player_eq(p,p2) andalso player_eq(p,p3)
      | _ => false
    (* end case *))

  (* 0 1 2
     3 4 5 
     6 7 8 *)

  val rows  = #[(0,1,2),(3,4,5),(6,7,8)]
  val cols  = #[(0,3,6),(1,4,7),(2,5,8)]
  val diags = #[(0,4,8),(2,4,6)]

  fun has_row  (p, b) = Vector.exists (fn trip => has_all (trip, p, b)) rows
  fun has_col  (p, b) = Vector.exists (fn trip => has_all (trip, p, b)) cols
  fun has_diag (p, b) = Vector.exists (fn trip => has_all (trip, p, b)) diags

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
	 		 orelse ((List.length (empties b)) = 0)

  (* apply_to : 'a -> (('a -> 'b) -> 'b) *)
  fun apply_to x = (fn f => f x)

  (* successors : state -> state list *)
  fun successors s = List.map (apply_to s) (legal_moves s)

  fun state_eq (b1, b2) = let
    fun eq ~1 = true
      | eq n  = 
         (case (Array.sub (b1, n), Array.sub (b2, n))
           of (NONE, NONE) => eq (n-1)
	    | (SOME p, SOME q) => player_eq (p, q) andalso eq (n-1)
	    | _ => false
	  (* end case *))
    val len = Array.length b1
    in
      if len <> Array.length b2 then false
      else eq (len-1)
    end

  val neg_inf = ~2
  val pos_inf =  2

end
