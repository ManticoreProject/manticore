structure TestC4 = struct

  structure C = MinimaxWithCacheFn(ConnectFour)

  fun test () = let
    val d = C.minimax_decision(ConnectFour.almost_there)
    in
      (ConnectFour.almost_there,
       ConnectFour.apply(d,ConnectFour.almost_there))
    end

end
