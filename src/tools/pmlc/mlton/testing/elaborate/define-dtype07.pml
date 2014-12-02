_module myId _prim (
  datatype myDt<'a>  = A of 'a | B | C;
  datatype myOtherDt = datatype myDt;
)
