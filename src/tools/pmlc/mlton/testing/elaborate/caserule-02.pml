_module myId (
  datatype myDatatype = A | B;
  fun myFun (x: int32) -> int32 =
    let y = A
      case y of
        A => return(x)
      | B => return(x)
      end;
)
