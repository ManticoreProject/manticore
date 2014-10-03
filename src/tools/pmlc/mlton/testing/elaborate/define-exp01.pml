_module myId (
  datatype myDatatype = A | B;
  fun myFun (x: int32) -> myDatatype =
    let y: myDatatype = A
      return(y);
)
