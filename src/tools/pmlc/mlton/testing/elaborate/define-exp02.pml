(* FAIL [function body doesn't agree with range type] *)
_module myId _prim (
  datatype myDatatype = A | B;
  fun myFun (x: int32) -> int32 =
    let y: myDatatype = A
      return(y);
)
