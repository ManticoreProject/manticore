functor AlphaBetaManticoreFn (G : GAME) : STRATEGIZER = struct

  (* This is an emulation of the Manticore algorithm for a/b pruning. *)
  (* Its purpose is to demonstrate that said algorithm produces the right result. *)

  structure U = Utilities
  structure G = G

  fun max (m:int, n:int) = if (m>n) then m else n
  fun min (m:int, n:int) = if (m<n) then m else n

  fun max_value (s : G.state, alpha : int, beta : int) : int =
    if G.terminal_test s then
      G.utility s
    else let
      val ss = G.successors s
      val t0 = min_value (hd ss, alpha, beta)
      val alpha' = max (alpha, t0)
      fun loop i =
        if (i = List.length ss) then []
        else let
(*          val ts = loop (i+1) *)
          val ti = min_value (List.nth (ss, i), alpha', beta)
          in
            if ti >= beta then [ti]
	    else ti :: (loop (i+1))
	  end
      val ch = t0 :: loop 1
      val maxScore = List.foldl max (hd ch) (tl ch)
      in
        maxScore
      end

  and min_value (s : G.state, alpha : int, beta : int) : int =
    if G.terminal_test s then
      G.utility s
    else let
      val ss = G.successors s
      val t0 = max_value (hd ss, alpha, beta)
      val beta' = min (beta, t0)
      fun loop i =
        if (i = List.length ss) then []
        else let
(*          val ts = loop (i+1) *)
          val ti = max_value (List.nth (ss, i), alpha, beta')
          in
            if ti <= alpha then [ti]
	    else ti :: (loop (i+1))
	  end
      val ch = t0 :: loop 1
      val minScore = List.foldl min (hd ch) (tl ch)
      in
        minScore
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
