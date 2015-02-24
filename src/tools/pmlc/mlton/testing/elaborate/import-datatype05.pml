(* FAIL [undefined type] *)

datatype dty = A | B

_module myId
  _import datatype dty as myDty with
    _con A as myA
    _con B as myB
  end
  _prim (
    fun myFun () -> dty =
      let foo : dty = myA
      let bar : dty = myB
      return(foo);
  )
