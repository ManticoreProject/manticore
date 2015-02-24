datatype innerDatatype = A | B
datatype outerDatatype = A2 of innerDatatype | B2 of innerDatatype

 (* This exists to match import-datatype02.pml, to ensure that the bug
 isn't with the handling of case statements *)

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
    fun myFun () -> innerDatatype =
      let myInner : innerDatatype = A
      let toReturn : outerDatatype = alloc A2 (myInner)
      case toReturn of
        A2 (x) => return (x)
      | B2 => return (myInner)
      end;
  )
