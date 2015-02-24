(* FAIL [unbound value identifier] *)

datatype dty = A | B

_module myId
  _import datatype dty as myDty with
    _con A as myA
    _con B as myB
  end
  _prim (
    fun myFun () -> myDty =
      let foo : myDty = A
      let bar : myDty = B
      return(foo);
  )
