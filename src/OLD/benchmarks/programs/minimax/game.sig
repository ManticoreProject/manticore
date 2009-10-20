signature GAME = sig

  type state
  type operator
  
  val initial_state : state
  val apply : operator * state -> state
  val legal_moves : state -> operator list
  val successors : state -> state list
  val terminal_test : state -> bool
  val utility : state -> int
  val max_move : state -> bool
  val min_move : state -> bool

  val neg_inf : int
  val pos_inf : int

  val state_eq : state * state -> bool

end
