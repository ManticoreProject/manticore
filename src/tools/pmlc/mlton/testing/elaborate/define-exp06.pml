_module myId _prim (
  datatype myDatatype = A | B;
  fun myFun (x: int32) -> myDatatype =
    do B return(A);
)
