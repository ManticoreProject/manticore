exception myExn

_module myId
  _import exception myExn
  _prim (
    fun myFun () -> exn = return(myExn);
  )
