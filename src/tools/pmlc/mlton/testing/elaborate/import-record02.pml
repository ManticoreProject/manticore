datatype inner = A
type recTy = {foo: inner, bar: inner, 1: inner}

val baz = {foo = A, bar = A, 1 = A}

_module myId
  _import datatype inner with
    _con A
  end  _import val baz: recTy
  _prim (
    fun myFun () -> inner =
      let inner1: inner = #0 (baz)
      let inner2: inner = #1 (baz)
      let inner3: inner = #2 (baz)
      case 15 of
        1 => return(inner1)
      | 2 => return(inner2)
      | 15 => return(inner3)
      end;
  )
