(* FAIL [undefined type] *)
_module myId (
  datatype myDt  = A | B | C
  and myOtherDt = D of anotherType;
)
