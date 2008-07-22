structure PrimTypes =
  struct

    _primcode (
      typedef exh = cont(exn);
      typedef unit = enum(0);
      typedef bool = enum(1);
      typedef fiber_fun = fun (unit / exh -> unit);
    )

    type fiber = _prim ( cont(unit) )

    datatype signal = STOP | PREEMPT of fiber

    type sigact = _prim ( cont(signal) )

  end
