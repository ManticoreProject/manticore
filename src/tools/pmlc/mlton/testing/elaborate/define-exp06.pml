_module myId (
  datatype myDatatype = A | B;
  fun myFun (x: int32) -> myDatatype =
    do B return(A);
)
