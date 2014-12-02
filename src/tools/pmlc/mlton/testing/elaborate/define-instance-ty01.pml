_module aModule (
  type myType<'a> = ['a];
)

_module myId (
  instance type aModule.myType<int32>;
)
