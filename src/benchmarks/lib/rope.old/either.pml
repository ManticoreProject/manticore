structure Either =
  struct

    datatype ('a, 'b) either = LEFT of 'a | RIGHT of 'b

  end
