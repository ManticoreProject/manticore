functor MinimaxCacheFn (G : GAME) : STRATEGIZER = struct

  structure U = Utilities
  structure G = G

  structure D = DictionaryFn(struct
			       type key = G.state
			       type value = int
			       val key_eq = G.state_eq
			     end)

  val d = ref D.empty

  fun lookup s = D.lookup (!d, s)

  fun record (s, v) = (d := D.insert (!d, s, v))

  fun highest_op oss = U.extreme (fn (a:int, b:int) => a>b) oss
  fun lowest_op oss  = U.extreme (fn (a:int, b:int) => a<b) oss

  fun value (s : G.state) : int = let
    val score = lookup s
    in
      case score
       of NONE => let 
            val v = if G.terminal_test s then
	              G.utility s
                    else let
                      val vals = map value (G.successors s)
                      in
                        (if G.max_move s then U.highest else U.lowest) vals
                      end
            in
	     (record (s, v); v)
	    end
	| SOME k => k
    end

  fun decision (s : G.state) : G.operator = let
    val ms = G.legal_moves s
    val possibilities = map (fn m => (m, G.apply (m, s))) ms
    val values = map (fn (m, s) => (m, value s)) possibilities
    in
      (if G.max_move s then highest_op else lowest_op) values
    end

end
