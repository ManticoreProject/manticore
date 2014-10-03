_module myId (
  datatype myDatatype = A | B;
  fun myFun (x: int32) -> int32 =
    let y = A
    let z = B
      case y of
        A => return(A)
      | B => return(B)
      end;
)
