_module aModule _prim (
  type myType<'a> = ['a];
)

_module myId _prim (
  instance type aModule.myType<int32>;
)
