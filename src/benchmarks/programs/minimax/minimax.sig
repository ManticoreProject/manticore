signature MINIMAX = sig
 
  structure G : GAME
  val minimax_decision : G.state -> G.operator
  val play_self : unit -> G.state list

end
