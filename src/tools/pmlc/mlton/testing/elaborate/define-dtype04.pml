(* FAIL [undefined type] *)
_module myId _prim (
  datatype myDt  = A | B | C
  and myOtherDt = D of anotherType;
)