structure ConnectFour (* : GAME *) = struct

  datatype checker = Red | Black
  type board = checker option list (* length 42 *)

  type state = board
  type operator = board -> board

  fun apply (oper : operator, s : state) : state = oper(s)

  fun checker_eq (Red, Red) = true
    | checker_eq (Black, Black) = true
    | checker_eq _ = false

  (* 00 01 02 03 04 05 06
     07 08 09 10 11 12 13
     14 15 16 17 18 19 20
     21 22 23 24 25 26 27 
     28 29 30 31 32 33 34 
     35 36 37 38 39 40 41 *)

  val initial_state = List.tabulate (42, fn _ => NONE : checker option)

  fun rows s = let
    fun r ([], acc) = rev acc
      | r (cs, acc) = r (List.drop (cs, 7), List.take (cs, 7) :: acc)
    in
      r (s, [])
    end

  fun cols s = let
    fun c ([], acc) = rev acc
      | c (css, acc) = 
          if length (hd css) = 1 then
	    c ([], map hd css :: acc)
	  else
	    c (map tl css, map hd css :: acc)
    in
      c (rows s, [])
    end

  (* 00 01 02 03 04 05 06
     07 08 09 10 11 12 13
     14 15 16 17 18 19 20
     21 22 23 24 25 26 27 
     28 29 30 31 32 33 34 
     35 36 37 38 39 40 41 *)

  fun add n upper start = let
    fun loop (curr, acc) = 
      if curr > upper then
	  rev acc
      else loop (curr+n, curr::acc)
    in
      loop (start, [])
    end

  fun extract s ns = map (fn n => List.nth (s, n)) ns

  fun diags s = let
    val nss = (map (add 8 41) [14,7,0,1,2,3]) @
 	      (map (add 3 41) [3,4,5,6,13,20])
    in
      map (extract s) nss
    end

  fun consec4 p ss = let
    fun loop ([], n) = (n = 4)
      | loop (s::ss, n) = (n = 4) orelse
         (case s
	   of NONE => loop (ss, 0)
	    | SOME p' => if checker_eq (p, p') then loop (ss, n+1)
			 else loop (ss, 0)
 	  (* end case *))
    in
      loop (ss, 0)
    end

  fun vert s p = List.exists (consec4 p) (rows s)

  fun horiz s p = List.exists (consec4 p) (cols s)

  fun diag s p = List.exists (consec4 p) (diags s)

  fun winner s = 
    if (vert s Red) orelse (horiz s Red) orelse (diag s Red) then SOME Red
    else if (vert s Black) orelse (horiz s Black) orelse (diag s Black) then SOME Black
    else NONE

  fun full s = List.all isSome s

  fun terminal_test (s : state) : bool = isSome (winner s) orelse full s

  fun count p s = let
    fun plus (SOME p', n) = if checker_eq(p,p') then n+1 else n
      | plus (NONE, n) = n
    in 
      foldl plus 0 s
    end

  fun whose_turn s = 
    if (count Red s) = (count Black s) then Red
    else if (count Red s) - 1 = (count Black s) then Black
    else let
      val s = "Red " ^ Int.toString (count Red s) ^
	    ", Black " ^ Int.toString (count Black s)
      in
        raise Fail ("board in illegal state: " ^ s)
      end

  fun non_full_cols s = let
    fun loop ([], _, acc) = acc
      | loop (c::cs, n, acc) =
	 (case c
	   of NONE => loop (cs, n+1, n::acc)
	    | SOME _ => loop (cs, n+1, acc)
	  (* end case *))
    in
      loop (List.take (s, 7), 0, [])
    end

  (* 00 01 02 03 04 05 06
     07 08 09 10 11 12 13
     14 15 16 17 18 19 20
     21 22 23 24 25 26 27 
     28 29 30 31 32 33 34 
     35 36 37 38 39 40 41 *)

  fun put (x, 0, y::ys) = x::ys
    | put (x, n, y::ys) = y::(put(x,n-1,ys))
    | put _ = raise Fail "bug"

  fun move (p, i) b = let
    fun loop n = 
      if n<0 then raise Fail "illegal move"
      else if isSome (List.nth (b, n)) then loop (n-7)
      else put (SOME p, n, b)
    in
      loop (i+35)
    end

  val almost_there = let
    val b = move (Red, 0) initial_state
    val b = move (Black, 6) b
    val b = move (Red, 0) b
    val b = move (Black, 6) b
    val b = move (Red, 0) b
    val b = move (Black, 6) b
    in
      b
    end

  fun legal_moves s = let
    val p = whose_turn s
    in
      map (fn i => (move (p, i))) (non_full_cols s)
    end

  fun successors s = let
    val ms = legal_moves s
    in
      map (fn m => m s) ms
    end

  fun utility s = 
   (case winner s
     of SOME Red => 1
      | SOME Black => ~1
      | NONE => 0)

  fun max_move s = (whose_turn s = Red)

  fun min_move s = not (max_move s)

  fun state_eq (b1, b2) = let
    fun eq ([], []) = true
      | eq (NONE::t1, NONE::t2) = true
      | eq (SOME(c1)::t1, SOME(c2)::t2) = checker_eq(c1,c2) andalso eq(t1,t2)
      | eq _ = false
    in
      eq (b1, b2)
    end

end







