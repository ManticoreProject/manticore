(* FAIL [immutable record in assignment expression] *)
datatype inner = A
type recTy = {foo: inner, bar: inner, 1: inner}

val baz = {foo = A, bar = A, 1 = A}

_module myId
  _import datatype inner with
    _con A
  end  _import val baz: recTy
  _prim (
    fun myFun () -> inner =
      let inner1: inner = #1 (baz) := A
      return (inner1);
  )
