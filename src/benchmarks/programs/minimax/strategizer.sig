signature STRATEGIZER = sig

  structure G  : GAME
  val value    : G.state -> int    
  val decision : G.state -> G.operator

end
