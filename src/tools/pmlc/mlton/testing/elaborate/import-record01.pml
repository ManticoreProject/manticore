datatype inner = A
datatype outer = Wrap of {foo: inner, bar: inner, 1: inner} | B

_module myId
  _import datatype inner with
    _con A
  end
  _import datatype outer with
    _con Wrap of {foo: inner, bar: inner, 1: inner}
    _con B
  end
  _prim (
    fun myFun () -> inner =
      let myInner : inner = A
      let struct : {0:inner, 1!inner, 2:inner} = alloc {0:inner, 1!inner, 2:inner} (A, A, myInner)
      let toReturn : outer = alloc Wrap(A, myInner, A)

      case toReturn of
        Wrap (x) => return (#1(x))
      | B => return (myInner)
      end;
  )
