datatype myDatatype = A | B
val someVal = A

_module myId
  _import datatype myDatatype with
    _con A
    _con B
    end
  _import val someVal : myDatatype as myVal
  _prim (
    fun myFun () -> myDatatype = return(myVal);
  )
