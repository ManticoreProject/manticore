datatype innerDatatype = A | B
datatype outerDatatype = A2 of innerDatatype | B2 of innerDatatype

_module myId
  _import datatype innerDatatype with
    _con A
    _con B
  end
  _import datatype outerDatatype with
    _con A2
    _con B2
  end
  _prim (
    fun myFun () -> outerDatatype =
      let myInner : innerDatatype = A
      let toReturn : outerDatatype = alloc A2 (myInner)
      return(toReturn);
  )
