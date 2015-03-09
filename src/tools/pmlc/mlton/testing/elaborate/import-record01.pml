datatype inner = A
datatype outer = Wrap of {foo: inner, bar: inner, 1: inner} | B

_module myId
  _import datatype inner with
    _con A
  end
  _import datatype outer with
    _con Wrap
    _con B
  end
  _prim (
    fun myFun () -> inner =
      let myInner : inner = A
      let toReturn : outer = alloc Wrap (myInner)
      case toReturn of
        Wrap (x) => return (x)
      | B => return (myInner)
      end;
  )
