functor MinimaxCacheFn (G : GAME) = struct

  structure D = DictionaryFn(struct
			       type key = G.state
			       type value = int
			       val key_eq = G.state_eq
			     end)

  structure M = MinimaxFn(G)

  val d = ref(D.empty)

  fun lookup s = D.lookup (!d, s)

  fun record (s, v) = let
    val d' = D.insert (!d, s, v)
    in
      d := d'
    end

  fun extreme_op relop oss = let
    fun h ([], (m, _)) = m
      | h ((m',n')::t, (m,n)) =
	  if relop (n',n) then h (t, (m', n'))
	  else h (t, (m, n))
    in
      case oss
       of [(oper, st)] => oper
	| (oper,st)::t => h (t, (oper,st))
	| [] => raise Fail "bug"
    end

  fun highest_op oss = extreme_op (fn (a:int, b:int) => a>b) oss
  fun lowest_op oss  = extreme_op (fn (a:int, b:int) => a<b) oss

  fun minimax_value (s : G.state) : int = let
    val score = lookup s
    in
      case score
       of NONE => let
            val v = M.minimax_value s
	    in
	     (record (s, v); v)
	    end
	| SOME k => k
  end

  fun minimax_decision (s : G.state) : G.operator = let
    val ms = G.legal_moves s
    val possibilities = map (fn m => (m, G.apply (m, s))) ms
    val values = map (fn (m, s) => (m, minimax_value s)) possibilities
    in
      (if G.max_move s then highest_op else lowest_op) values
    end

  fun play_self () = let
    fun loop (s : G.state, acc) =
      if G.terminal_test s then
        rev (s::acc)
      else let
        val d = minimax_decision s
        val s' = G.apply (d, s)
        in
          loop (s', s::acc)
        end
    in
      loop (G.initial_state, [])
    end

end
