_module myId (
  type myType <'a, 'b> = ['a, 'b];
  type myType' = myType<int32, int32>;
)
