let
  val myMLVal : int = 5
in
  _module myId
    _import val myMLVal : int as myMLVal'
    _prim (
      fun myFun () -> int32 = return(myMLVal');
    )
end
