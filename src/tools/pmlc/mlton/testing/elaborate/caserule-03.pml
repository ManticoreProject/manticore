(* FAIL [case object and rules don't agree] *)
_module myId (
  datatype myDatatype = A | B;
  fun myFun (x: int32) -> int32 =
    let y = A
      case y of
        5 => return(x)
      end;
)
