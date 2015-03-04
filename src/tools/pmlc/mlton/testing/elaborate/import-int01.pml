val myVal : int = 15

_module myId
  _import val myVal : int
  _prim (
    fun myFun () -> int32 = return(myVal);
  )
