_primcode (
  define __attributes__ ("inline") @add (a : any, b : any / _ : cont) -> any =
    let result = apply this.I32ADD (this.a, this.b)
    return (this.result);
)

_type int = _prim (any);
_val intAdd : int * int -> int = _prim (@add);
intAdd(1, 2);
