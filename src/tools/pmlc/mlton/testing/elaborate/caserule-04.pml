(* FAIL [function body doesn't agree with range type] *)
_module myId (
  datatype myDatatype = A | B;
  fun myFun (x: int32) -> int32 =
    let y = A
      case y of
        A => return(A)
      end;
)
