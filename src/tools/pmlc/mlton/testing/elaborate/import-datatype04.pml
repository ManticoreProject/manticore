datatype dty = A | B

_module myId
  _import datatype dty as myDty with
    _con A as myA
    _con B as myB
  end
  _prim (
    fun myFun () -> myDty =
      let foo : myDty = myA
      let bar : myDty = myB
      return(foo);
  )
