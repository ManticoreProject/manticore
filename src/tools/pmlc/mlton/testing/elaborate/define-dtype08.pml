(* FAIL [not a datatype] *)
_module myId _prim (
  type myFoo = int32;
  datatype myDt  = datatype myFoo;
)
