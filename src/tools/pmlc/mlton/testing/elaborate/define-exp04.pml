(* FAIL [left and right side of let binding are of different lengths] *)
_module myId (
  datatype myDatatype = A | B;
  fun myFun (x: int32) -> myDatatype =
    let y = return(A, B)
      return(y);
)
