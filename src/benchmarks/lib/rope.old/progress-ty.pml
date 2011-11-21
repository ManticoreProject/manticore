structure ProgressTy =
  struct

    datatype ('a, 'b) progress
      = PARTIAL of 'a * 'b
      | COMPLETE of 'b

    datatype ('a, 'b, 'c) progress2
      = PARTIAL2 of 'a * 'b * 'c
      | COMPLETE2 of 'c

  end
