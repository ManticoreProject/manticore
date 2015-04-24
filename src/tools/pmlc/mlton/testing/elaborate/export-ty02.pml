_module myId _prim (
  fun myFun () -> int32 =
    let x: int32 = 5
    return(x);
)

_type myInt = _prim(int32)
_val myFun = _prim (myFun)
