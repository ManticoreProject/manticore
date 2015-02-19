datatype myDatatype = A | B

_module myId
  _import datatype myDatatype with
    _con A
    _con B
  end
  _prim (
    fun myFun () -> myDatatype =
      let myA: myDatatype = A
      return(myA);
  )
