_module myId (
  define __attributes__ ("inline") @add (a : any, b : any / _ : cont) -> any =
    return (I32ADD (a, b)) ;
)

_type int = _prim (any);
_val intAdd : int * int -> int = _prim (@add);
intAdd(1, 2);
