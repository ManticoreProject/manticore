functor AlphaBetaFn (G : GAME) : STRATEGIZER = struct

  structure U = Utilities
  structure G = G

  fun max (m:int, n:int) = if (m>n) then m else n
  fun min (m:int, n:int) = if (m<n) then m else n

  fun max_value (s : G.state, alpha : int, beta : int) : int =
    if G.terminal_test s then
      G.utility s
    else let
      fun loop ([] : G.state list, a : int) = a
	| loop (s::t, a) = let
            val a' = max (a, min_value (s, a, beta))
            in 
              if (a' >= beta) then beta
	      else loop (t, a')
	    end
      in
        loop (G.successors s, alpha)
      end

  and min_value (s : G.state, alpha : int, beta : int) : int =
    if G.terminal_test s then
      G.utility s
    else let
      fun loop ([] : G.state list, b : int) = b
	| loop (s::t, b) = let
            val b' = min (b, max_value (s, alpha, b))
            in
              if (b' <= alpha) then alpha
	      else loop (t, b')
            end
      in
        loop (G.successors s, beta)
      end

  fun value (s : G.state) : int =
    if G.terminal_test s then
      G.utility s
    else if G.max_move s then
      max_value (s, G.neg_inf, G.pos_inf)
    else
      min_value (s, G.neg_inf, G.pos_inf)

  fun highest_op oss = U.extreme (fn (a:int, b:int) => a>b) oss
  fun lowest_op oss  = U.extreme (fn (a:int, b:int) => a<b) oss

  fun decision (s : G.state) : G.operator = let
    val ms = G.legal_moves s
    val possibilities = map (fn m => (m, G.apply (m, s))) ms
    val oss = map (fn (m, s) => (m, value s)) possibilities
    in
      (if G.max_move s then highest_op else lowest_op) oss 
    end

end
