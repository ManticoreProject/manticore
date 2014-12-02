(* FAIL [not a datatype] *)
_module myId (
  type myFoo = int32;
  datatype myDt  = datatype myFoo;
)
