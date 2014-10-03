_module myId (
  datatype myDatatype = A | B;
  fun myFun (x: int32) -> (myDatatype, myDatatype) =
    let y, z = return(A, B)
      return(y, z);
)
