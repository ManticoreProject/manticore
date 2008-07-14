structure PrimTypes =
  struct

val x = 1
    _primcode (
      typedef exn = any;
      typedef exh = cont(exn);
    )
  end