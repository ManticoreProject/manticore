_module myId (
  define __attributes__ ("inline") @add (a : int32, b : int32 / _ : cont) -> int32 =
    return (I32ADD(a, b)) ;
)

_type int = _prim (int32);
_val intAdd : int * int -> int = _prim (@add);
intAdd(1, 2);
