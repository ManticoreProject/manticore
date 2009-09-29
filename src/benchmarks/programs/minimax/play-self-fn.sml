functor PlaySelfFn (S : STRATEGIZER) : sig

    structure G : GAME
    val play_self : unit -> G.state list

  end = struct

  structure G = S.G

  fun play_self () = let
    fun loop (s : G.state, acc) =
      if G.terminal_test s then
        rev (s::acc)
      else let
        val d = S.decision s
        val s' = G.apply (d, s)
        in
          loop (s', s::acc)
        end
    in
      loop (G.initial_state, [])
    end
      
end
