datatype dty = A | B
datatype 'a myOption = mySome of 'a | myNone

_module myId
  _import datatype dty with
    _con A
    _con B
  end
  _import datatype dty myOption with
    _con mySome
    _con myNone
  end
  _prim (
    fun myFun () -> dty =
      let some : myOption = alloc mySome (A)
        case some of
          mySome (A) => return (A)
        | myNone => return (B)
        end;
  )
