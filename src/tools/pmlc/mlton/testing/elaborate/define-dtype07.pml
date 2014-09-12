_module myId (
  datatype myDt<'a>  = A of 'a | B | C;
  datatype myOtherDt = datatype myDt;
)
