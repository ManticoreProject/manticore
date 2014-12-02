_module myId (
  datatype myDatatype = A | B;
  fun myFun (x: int32) -> myDatatype =
    let y = A
    let z = B
      case y of
        A => return(A)
      | B => return(B)
      end;
)
