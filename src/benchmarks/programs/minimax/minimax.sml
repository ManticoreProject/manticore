functor MinimaxFn (G : GAME) = struct

  fun highest_op oss = let
    fun h ([], (m, _)) = m
      | h ((m',n')::t, (m,n)) =
	  if n' > n then h (t, (m', n')
	  else h (t, (m, n))
    in
      case oss
       of [(oper, st)] => oper
	| (oper,st)::t => h (t, (oper,st))
	| [] => raise Fail "bug"
    end

  fun highest (ns : int list) : int =
   (case ns
     of [] => raise Fail "undefined"
      | m::ms => foldl (fn (a,b) => if (a>b) then a else b) m ms
    (* end case *))

  fun lowest (ns : int list) : int =
   (case ns
     of [] => raise Fail "undefined"
      | m::ms => foldl (fn (a,b) => (a<b) then a else b) m ms
    (* end case *))

  fun minimax_value (s : state) : int = 
    if G.terminal_test s then 
	G.utility s
    else if G.max_move s then
	highest (map minimax_value (G.successors s))
    else
	lowest (map minimax_value (G.successors s))

  fun minimax_decision (s : G.state) : G.operator = let
    val ms = G.legal_moves s
    val possibilities = map (fn m => (m, G.apply (m, s))) ms
    val values = map (fn (m, s) => (m, minimax_value s)) possibilities
    in
      highest_op values
    end

end
