structure Timer = struct

  fun time (f : unit -> 'a) = let
    val t0 = Time.now()
    val v = f()
    val t1 = Time.now()
    in
      (v, Time.toMilliseconds t1 - Time.toMilliseconds t0)
    end

  local
    fun last [] = raise Fail "undefined"
      | last [x] = x
      | last (_::t) = last t 
(*
    structure MM  = MinimaxFn(TicTacToe)
    structure MMC = MinimaxCacheFn(TicTacToe)
    structure AB  = AlphaBetaFn(TicTacToe)
    structure ABC = AlphaBetaCacheFn(TicTacToe)
    structure P1 = PlaySelfFn(MM)
    structure P2 = PlaySelfFn(MMC)
    structure P3 = PlaySelfFn(AB)
    structure P4 = PlaySelfFn(ABC)
*)
    structure MMA  = MinimaxFn(TicTacToeArray)
    structure MMCA = MinimaxCacheFn(TicTacToeArray)
    structure ABA  = AlphaBetaFn(TicTacToeArray)
    structure ABCA = AlphaBetaCacheFn(TicTacToeArray)
    structure ABCMA = AlphaBetaManticoreFn(TicTacToeArray)
    structure P1A = PlaySelfFn(MMA)
    structure P2A = PlaySelfFn(MMCA)
    structure P3A = PlaySelfFn(ABA)
    structure P4A = PlaySelfFn(ABCA)
    structure P5A = PlaySelfFn(ABCMA)
  in
    fun test () = let
      val (g1, t1) = time P1A.play_self
      val (g2, t2) = time P2A.play_self
      val (g3, t3) = time P3A.play_self
      val (g4, t4) = time P4A.play_self
      val (g5, t5) = time P5A.play_self
      in
        ((g1, t1), (g2, t2), (g3, t3), (g4, t4), (g5, t5))
      end
  end (* local *)

end
