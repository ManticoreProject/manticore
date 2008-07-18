structure PrimTypes =
  struct

    _primcode (
      typedef exn = any;
      typedef exh = cont(exn);
      typedef unit = enum(0);
      typedef bool = enum(1);
    )

  end
