datatype myDatatype = A | B
val myVal = A

_module myId
  _import datatype myDatatype with
    _con A
    _con B
    end
  _import val myVal : myDatatype
  _prim (
    fun myFun () -> myDatatype = return(myVal);
  )
