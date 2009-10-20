functor MinimaxFn (G : GAME) : STRATEGIZER = struct

  structure G = G
  structure U = Utilities

  fun highest_op oss = U.extreme (fn (a:int, b:int) => a>b) oss
  fun lowest_op oss  = U.extreme (fn (a:int, b:int) => a<b) oss

  fun value (s : G.state) : int = 
    if G.terminal_test s then 
	G.utility s
    else let
      val vals = map value (G.successors s) 
      in
        (if G.max_move s then U.highest else U.lowest) vals
      end

  fun decision (s : G.state) : G.operator = let
    val ms = G.legal_moves s
    val possibilities = map (fn m => (m, G.apply (m, s))) ms
    val mss = map (fn (m, s) => (m, value s)) possibilities
    in
      (if G.max_move s then highest_op else lowest_op) mss
    end

end
